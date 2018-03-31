let s:nomodeline = (v:version > 703 || (v:version == 703 && has('patch442'))) ? '<nomodeline>' : ''

let s:hunk_re = '^@@ -\(\d\+\),\?\(\d*\) +\(\d\+\),\?\(\d*\) @@'

" True for git v1.7.2+.
function! s:git_supports_command_line_config_override() abort
  call system(g:gitgutter_git_executable.' -c foo.bar=baz --version')
  return !v:shell_error
endfunction

let s:c_flag = s:git_supports_command_line_config_override()


let s:temp_index = tempname()
let s:temp_buffer = tempname()

" Returns a diff of the buffer.
"
" The buffer contents is not the same as the file on disk so we need to pass
" two instances of the file to git-diff:
"
"     git diff myfileA myfileB
"
" where myfileA comes from
"
"     git show :myfile > myfileA
"
" and myfileB is the buffer contents.
"
" After running the diff we pass it through grep where available to reduce
" subsequent processing by the plugin.  If grep is not available the plugin
" does the filtering instead.
"
"
" Regarding line endings:
"
" git-show does not convert line endings.
" git-diff FILE FILE does convert line endings for the given files.
"
" If a file has CRLF line endings and git's core.autocrlf is true,
" the file in git's object store will have LF line endings.  Writing
" it out via git-show will produce a file with LF line endings.
"
" If this last file is one of the files passed to git-diff, git-diff will
" convert its line endings to CRLF before diffing -- which is what we want --
" but also by default output a warning on stderr.
"
"   warning: LF will be replace by CRLF in <temp file>.
"   The file will have its original line endings in your working directory.
"
" When running the diff asynchronously, the warning message triggers the stderr
" callbacks which assume the overall command has failed and reset all the
" signs.  As this is not what we want, and we can safely ignore the warning,
" we turn it off by passing the '-c "core.safecrlf=false"' argument to
" git-diff.
"
" When writing the temporary files we preserve the original file's extension
" so that repos using .gitattributes to control EOL conversion continue to
" convert correctly.
function! gitgutter#diff#run_diff(bufnr, preserve_full_diff) abort
  while gitgutter#utility#repo_path(a:bufnr, 0) == -1
    sleep 5m
  endwhile

  if gitgutter#utility#repo_path(a:bufnr, 0) == -2
    throw 'gitgutter not tracked'
  endif


  " Wrap compound commands in parentheses to make Windows happy.
  " bash doesn't mind the parentheses.
  let cmd = '('

  " Append buffer number to avoid race conditions between writing and reading
  " the files when asynchronously processing multiple buffers.
  "
  " Without the buffer number, index_file would have a race in the shell
  " between the second process writing it (with git-show) and the first
  " reading it (with git-diff).
  let index_file = s:temp_index.'.'.a:bufnr

  " Without the buffer number, buff_file would have a race between the
  " second gitgutter#process_buffer() writing the file (synchronously, below)
  " and the first gitgutter#process_buffer()'s async job reading it (with
  " git-diff).
  let buff_file = s:temp_buffer.'.'.a:bufnr

  let extension = gitgutter#utility#extension(a:bufnr)
  if !empty(extension)
    let index_file .= '.'.extension
    let buff_file .= '.'.extension
  endif

  " Write file from index to temporary file.
  let index_name = g:gitgutter_diff_base.':'.gitgutter#utility#repo_path(a:bufnr, 1)
  let cmd .= g:gitgutter_git_executable.' --no-pager show '.index_name.' > '.index_file.' && '

  " Write buffer to temporary file.
  " Note: this is synchronous.
  call s:write_buffer(a:bufnr, buff_file)

  " Call git-diff with the temporary files.
  let cmd .= g:gitgutter_git_executable.' --no-pager'
  if s:c_flag
    let cmd .= ' -c "diff.autorefreshindex=0"'
    let cmd .= ' -c "diff.noprefix=false"'
    let cmd .= ' -c "core.safecrlf=false"'
  endif
  let cmd .= ' diff --no-ext-diff --no-color -U0 '.g:gitgutter_diff_args.' -- '.index_file.' '.buff_file

  " Pipe git-diff output into grep.
  if !a:preserve_full_diff && !empty(g:gitgutter_grep)
    let cmd .= ' | '.g:gitgutter_grep.' '.gitgutter#utility#shellescape('^@@ ')
  endif

  " grep exits with 1 when no matches are found; git-diff exits with 1 when
  " differences are found.  However we want to treat non-matches and
  " differences as non-erroneous behaviour; so we OR the command with one
  " which always exits with success (0).
  let cmd .= ' || exit 0'

  let cmd .= ')'

  let cmd = gitgutter#utility#cd_cmd(a:bufnr, cmd)

  if g:gitgutter_async && gitgutter#async#available()
    call gitgutter#async#execute(cmd, a:bufnr, {
          \   'out': function('gitgutter#diff#handler'),
          \   'err': function('gitgutter#hunk#reset'),
          \ })
    return 'async'

  else
    let diff = gitgutter#utility#system(cmd)

    if v:shell_error
      call gitgutter#debug#log(diff)
      throw 'gitgutter diff failed'
    endif

    return diff
  endif
endfunction


function! gitgutter#diff#handler(bufnr, diff) abort
  call gitgutter#debug#log(a:diff)

  call gitgutter#hunk#set_hunks(a:bufnr, gitgutter#diff#parse_diff(a:diff))
  let modified_lines = gitgutter#diff#process_hunks(a:bufnr, gitgutter#hunk#hunks(a:bufnr))

  let signs_count = len(modified_lines)
  if signs_count > g:gitgutter_max_signs
    call gitgutter#utility#warn_once(a:bufnr, printf(
          \ 'exceeded maximum number of signs (%d > %d, configured by g:gitgutter_max_signs).',
          \ signs_count, g:gitgutter_max_signs), 'max_signs')
    call gitgutter#sign#clear_signs(a:bufnr)

  else
    if g:gitgutter_signs || g:gitgutter_highlight_lines
      call gitgutter#sign#update_signs(a:bufnr, modified_lines)
    endif
  endif

  call s:save_last_seen_change(a:bufnr)
  if exists('#User#GitGutter')
    let g:gitgutter_hook_context = {'bufnr': a:bufnr}
    execute 'doautocmd' s:nomodeline 'User GitGutter'
    unlet g:gitgutter_hook_context
  endif
endfunction


function! gitgutter#diff#parse_diff(diff) abort
  let hunks = []
  for line in split(a:diff, '\n')
    let hunk_info = gitgutter#diff#parse_hunk(line)
    if len(hunk_info) == 4
      call add(hunks, hunk_info)
    endif
  endfor
  return hunks
endfunction

function! gitgutter#diff#parse_hunk(line) abort
  let matches = matchlist(a:line, s:hunk_re)
  if len(matches) > 0
    let from_line  = str2nr(matches[1])
    let from_count = (matches[2] == '') ? 1 : str2nr(matches[2])
    let to_line    = str2nr(matches[3])
    let to_count   = (matches[4] == '') ? 1 : str2nr(matches[4])
    return [from_line, from_count, to_line, to_count]
  else
    return []
  end
endfunction

" This function is public so it may be used by other plugins
" e.g. vim-signature.
function! gitgutter#diff#process_hunks(bufnr, hunks) abort
  let modified_lines = []
  for hunk in a:hunks
    call extend(modified_lines, s:process_hunk(a:bufnr, hunk))
  endfor
  return modified_lines
endfunction

" Returns [ [<line_number (number)>, <name (string)>], ...]
function! s:process_hunk(bufnr, hunk) abort
  let modifications = []
  let from_line  = a:hunk[0]
  let from_count = a:hunk[1]
  let to_line    = a:hunk[2]
  let to_count   = a:hunk[3]

  if s:is_added(from_count, to_count)
    call s:process_added(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_added(a:bufnr, to_count)

  elseif s:is_removed(from_count, to_count)
    call s:process_removed(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_removed(a:bufnr, from_count)

  elseif s:is_modified(from_count, to_count)
    call s:process_modified(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_modified(a:bufnr, to_count)

  elseif s:is_modified_and_added(from_count, to_count)
    call s:process_modified_and_added(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_added(a:bufnr, to_count - from_count)
    call gitgutter#hunk#increment_lines_modified(a:bufnr, from_count)

  elseif s:is_modified_and_removed(from_count, to_count)
    call s:process_modified_and_removed(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_modified(a:bufnr, to_count)
    call gitgutter#hunk#increment_lines_removed(a:bufnr, from_count - to_count)

  endif
  return modifications
endfunction

function! s:is_added(from_count, to_count) abort
  return a:from_count == 0 && a:to_count > 0
endfunction

function! s:is_removed(from_count, to_count) abort
  return a:from_count > 0 && a:to_count == 0
endfunction

function! s:is_modified(from_count, to_count) abort
  return a:from_count > 0 && a:to_count > 0 && a:from_count == a:to_count
endfunction

function! s:is_modified_and_added(from_count, to_count) abort
  return a:from_count > 0 && a:to_count > 0 && a:from_count < a:to_count
endfunction

function! s:is_modified_and_removed(from_count, to_count) abort
  return a:from_count > 0 && a:to_count > 0 && a:from_count > a:to_count
endfunction

function! s:process_added(modifications, from_count, to_count, to_line) abort
  let offset = 0
  while offset < a:to_count
    let line_number = a:to_line + offset
    call add(a:modifications, [line_number, 'added'])
    let offset += 1
  endwhile
endfunction

function! s:process_removed(modifications, from_count, to_count, to_line) abort
  if a:to_line == 0
    call add(a:modifications, [1, 'removed_first_line'])
  else
    call add(a:modifications, [a:to_line, 'removed'])
  endif
endfunction

function! s:process_modified(modifications, from_count, to_count, to_line) abort
  let offset = 0
  while offset < a:to_count
    let line_number = a:to_line + offset
    call add(a:modifications, [line_number, 'modified'])
    let offset += 1
  endwhile
endfunction

function! s:process_modified_and_added(modifications, from_count, to_count, to_line) abort
  let offset = 0
  while offset < a:from_count
    let line_number = a:to_line + offset
    call add(a:modifications, [line_number, 'modified'])
    let offset += 1
  endwhile
  while offset < a:to_count
    let line_number = a:to_line + offset
    call add(a:modifications, [line_number, 'added'])
    let offset += 1
  endwhile
endfunction

function! s:process_modified_and_removed(modifications, from_count, to_count, to_line) abort
  let offset = 0
  while offset < a:to_count
    let line_number = a:to_line + offset
    call add(a:modifications, [line_number, 'modified'])
    let offset += 1
  endwhile
  let a:modifications[-1] = [a:to_line + offset - 1, 'modified_removed']
endfunction


" Returns a diff for the current hunk.
function! gitgutter#diff#hunk_diff(bufnr, full_diff)
  let modified_diff = []
  let keep_line = 1
  " Don't keepempty when splitting because the diff we want may not be the
  " final one.  Instead add trailing NL at end of function.
  for line in split(a:full_diff, '\n')
    let hunk_info = gitgutter#diff#parse_hunk(line)
    if len(hunk_info) == 4  " start of new hunk
      let keep_line = gitgutter#hunk#cursor_in_hunk(hunk_info)
    endif
    if keep_line
      call add(modified_diff, line)
    endif
  endfor
  return join(modified_diff, "\n")."\n"
endfunction


function! s:write_buffer(bufnr, file)
  let bufcontents = getbufline(a:bufnr, 1, '$')

  if getbufvar(a:bufnr, '&fileformat') ==# 'dos'
    call map(bufcontents, 'v:val."\r"')
  endif

  let fenc = getbufvar(a:bufnr, '&fileencoding')
  if fenc !=# &encoding
    call map(bufcontents, 'iconv(v:val, &encoding, "'.fenc.'")')
  endif

  if getbufvar(a:bufnr, '&bomb')
    let bufcontents[0]='﻿'.bufcontents[0]
  endif

  call writefile(bufcontents, a:file)
endfunction


function! s:save_last_seen_change(bufnr) abort
  call gitgutter#utility#setbufvar(a:bufnr, 'tick', getbufvar(a:bufnr, 'changedtick'))
endfunction


