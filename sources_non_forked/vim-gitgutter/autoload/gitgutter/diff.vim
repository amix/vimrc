if exists('g:gitgutter_grep_command')
  let s:grep_available = 1
  let s:grep_command = g:gitgutter_grep_command
else
  let s:grep_available = executable('grep')
  if s:grep_available
    let s:grep_command = 'grep'
    if $GREP_OPTIONS =~# '--color=always'
      let s:grep_command .= ' --color=never'
    endif
  endif
endif
let s:hunk_re = '^@@ -\(\d\+\),\?\(\d*\) +\(\d\+\),\?\(\d*\) @@'

let s:c_flag = gitgutter#utility#git_supports_command_line_config_override()

let s:temp_index = tempname()
let s:temp_buffer = tempname()

" Returns a diff of the buffer.
"
" The way to get the diff depends on whether the buffer is saved or unsaved.
"
" * Saved: the buffer contents is the same as the file on disk in the working
"   tree so we simply do:
"
"       git diff myfile
"
" * Unsaved: the buffer contents is not the same as the file on disk so we
"   need to pass two instances of the file to git-diff:
"
"       git diff myfileA myfileB
"
"   The first instance is the file in the index which we obtain with:
"
"       git show :myfile > myfileA
"
"   The second instance is the buffer contents.  Ideally we would pass this to
"   git-diff on stdin via the second argument to vim's system() function.
"   Unfortunately git-diff does not do CRLF conversion for input received on
"   stdin, and git-show never performs CRLF conversion, so repos with CRLF
"   conversion report that every line is modified due to mismatching EOLs.
"
"   Instead, we write the buffer contents to a temporary file - myfileB in this
"   example.  Note the file extension must be preserved for the CRLF
"   conversion to work.
"
" Before diffing a buffer for the first time, we check whether git knows about
" the file:
"
"     git ls-files --error-unmatch myfile
"
" After running the diff we pass it through grep where available to reduce
" subsequent processing by the plugin.  If grep is not available the plugin
" does the filtering instead.
function! gitgutter#diff#run_diff(realtime, preserve_full_diff) abort
  " Wrap compound commands in parentheses to make Windows happy.
  " bash doesn't mind the parentheses.
  let cmd = '('

  let bufnr = gitgutter#utility#bufnr()
  let tracked = gitgutter#utility#getbufvar(bufnr, 'tracked', 0)  " i.e. tracked by git
  if !tracked
    " Don't bother trying to realtime-diff an untracked file.
    " NOTE: perhaps we should pull this guard up to the caller?
    if a:realtime
      throw 'diff failed'
    else
      let cmd .= g:gitgutter_git_executable.' ls-files --error-unmatch '.gitgutter#utility#shellescape(gitgutter#utility#filename()).' && ('
    endif
  endif

  if a:realtime
    let blob_name = g:gitgutter_diff_base.':'.gitgutter#utility#shellescape(gitgutter#utility#file_relative_to_repo_root())
    let blob_file = s:temp_index
    let buff_file = s:temp_buffer
    let extension = gitgutter#utility#extension()
    if !empty(extension)
      let blob_file .= '.'.extension
      let buff_file .= '.'.extension
    endif
    let cmd .= g:gitgutter_git_executable.' show '.blob_name.' > '.blob_file.' && '

    " Writing the whole buffer resets the '[ and '] marks and also the
    " 'modified' flag (if &cpoptions includes '+').  These are unwanted
    " side-effects so we save and restore the values ourselves.
    let modified      = getbufvar(bufnr, "&mod")
    let op_mark_start = getpos("'[")
    let op_mark_end   = getpos("']")

    execute 'keepalt noautocmd silent write!' buff_file

    call setbufvar(bufnr, "&mod", modified)
    call setpos("'[", op_mark_start)
    call setpos("']", op_mark_end)
  endif

  let cmd .= g:gitgutter_git_executable
  if s:c_flag
    let cmd .= ' -c "diff.autorefreshindex=0"'
    let cmd .= ' -c "diff.noprefix=false"'
  endif
  let cmd .= ' diff --no-ext-diff --no-color -U0 '.g:gitgutter_diff_args.' '

  if a:realtime
    let cmd .= ' -- '.blob_file.' '.buff_file
  else
    let cmd .= g:gitgutter_diff_base.' -- '.gitgutter#utility#shellescape(gitgutter#utility#filename())
  endif

  if !a:preserve_full_diff && s:grep_available
    let cmd .= ' | '.s:grep_command.' '.gitgutter#utility#shellescape('^@@ ')
  endif

  if (!a:preserve_full_diff && s:grep_available) || a:realtime
    " grep exits with 1 when no matches are found; diff exits with 1 when
    " differences are found.  However we want to treat non-matches and
    " differences as non-erroneous behaviour; so we OR the command with one
    " which always exits with success (0).
    let cmd .= ' || exit 0'
  endif

  let cmd .= ')'

  if !tracked
    let cmd .= ')'
  endif

  let cmd = gitgutter#utility#command_in_directory_of_file(cmd)

  if g:gitgutter_async && gitgutter#async#available() && !a:preserve_full_diff
    call gitgutter#async#execute(cmd)
    return 'async'

  else
    let diff = gitgutter#utility#system(cmd)

    if gitgutter#utility#shell_error()
      " A shell error indicates the file is not tracked by git (unless something bizarre is going on).
      throw 'diff failed'
    endif

    return diff
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

function! gitgutter#diff#process_hunks(hunks) abort
  let modified_lines = []
  for hunk in a:hunks
    call extend(modified_lines, gitgutter#diff#process_hunk(hunk))
  endfor
  return modified_lines
endfunction

" Returns [ [<line_number (number)>, <name (string)>], ...]
function! gitgutter#diff#process_hunk(hunk) abort
  let modifications = []
  let from_line  = a:hunk[0]
  let from_count = a:hunk[1]
  let to_line    = a:hunk[2]
  let to_count   = a:hunk[3]

  if gitgutter#diff#is_added(from_count, to_count)
    call gitgutter#diff#process_added(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_added(to_count)

  elseif gitgutter#diff#is_removed(from_count, to_count)
    call gitgutter#diff#process_removed(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_removed(from_count)

  elseif gitgutter#diff#is_modified(from_count, to_count)
    call gitgutter#diff#process_modified(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_modified(to_count)

  elseif gitgutter#diff#is_modified_and_added(from_count, to_count)
    call gitgutter#diff#process_modified_and_added(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_added(to_count - from_count)
    call gitgutter#hunk#increment_lines_modified(from_count)

  elseif gitgutter#diff#is_modified_and_removed(from_count, to_count)
    call gitgutter#diff#process_modified_and_removed(modifications, from_count, to_count, to_line)
    call gitgutter#hunk#increment_lines_modified(to_count)
    call gitgutter#hunk#increment_lines_removed(from_count - to_count)

  endif
  return modifications
endfunction

function! gitgutter#diff#is_added(from_count, to_count) abort
  return a:from_count == 0 && a:to_count > 0
endfunction

function! gitgutter#diff#is_removed(from_count, to_count) abort
  return a:from_count > 0 && a:to_count == 0
endfunction

function! gitgutter#diff#is_modified(from_count, to_count) abort
  return a:from_count > 0 && a:to_count > 0 && a:from_count == a:to_count
endfunction

function! gitgutter#diff#is_modified_and_added(from_count, to_count) abort
  return a:from_count > 0 && a:to_count > 0 && a:from_count < a:to_count
endfunction

function! gitgutter#diff#is_modified_and_removed(from_count, to_count) abort
  return a:from_count > 0 && a:to_count > 0 && a:from_count > a:to_count
endfunction

function! gitgutter#diff#process_added(modifications, from_count, to_count, to_line) abort
  let offset = 0
  while offset < a:to_count
    let line_number = a:to_line + offset
    call add(a:modifications, [line_number, 'added'])
    let offset += 1
  endwhile
endfunction

function! gitgutter#diff#process_removed(modifications, from_count, to_count, to_line) abort
  if a:to_line == 0
    call add(a:modifications, [1, 'removed_first_line'])
  else
    call add(a:modifications, [a:to_line, 'removed'])
  endif
endfunction

function! gitgutter#diff#process_modified(modifications, from_count, to_count, to_line) abort
  let offset = 0
  while offset < a:to_count
    let line_number = a:to_line + offset
    call add(a:modifications, [line_number, 'modified'])
    let offset += 1
  endwhile
endfunction

function! gitgutter#diff#process_modified_and_added(modifications, from_count, to_count, to_line) abort
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

function! gitgutter#diff#process_modified_and_removed(modifications, from_count, to_count, to_line) abort
  let offset = 0
  while offset < a:to_count
    let line_number = a:to_line + offset
    call add(a:modifications, [line_number, 'modified'])
    let offset += 1
  endwhile
  let a:modifications[-1] = [a:to_line + offset - 1, 'modified_removed']
endfunction

" Generates a zero-context diff for the current hunk.
"
" diff - the full diff for the buffer
" type - stage | undo | preview
function! gitgutter#diff#generate_diff_for_hunk(diff, type) abort
  let diff_for_hunk = gitgutter#diff#discard_hunks(a:diff, a:type == 'stage' || a:type == 'undo')

  if a:type == 'stage' || a:type == 'undo'
    let diff_for_hunk = gitgutter#diff#adjust_hunk_summary(diff_for_hunk, a:type == 'stage')
  endif

  return diff_for_hunk
endfunction

" Returns the diff with all hunks discarded except the current.
"
" diff        - the diff to process
" keep_header - truthy to keep the diff header and hunk summary, falsy to discard it
function! gitgutter#diff#discard_hunks(diff, keep_header) abort
  let modified_diff = []
  let keep_line = a:keep_header
  for line in split(a:diff, '\n')
    let hunk_info = gitgutter#diff#parse_hunk(line)
    if len(hunk_info) == 4  " start of new hunk
      let keep_line = gitgutter#hunk#cursor_in_hunk(hunk_info)
    endif
    if keep_line
      call add(modified_diff, line)
    endif
  endfor

  if a:keep_header
    return gitgutter#utility#stringify(modified_diff)
  else
    " Discard hunk summary too.
    return gitgutter#utility#stringify(modified_diff[1:])
  endif
endfunction

" Adjust hunk summary (from's / to's line number) to ignore changes above/before this one.
"
" diff_for_hunk - a diff containing only the hunk of interest
" staging       - truthy if the hunk is to be staged, falsy if it is to be undone
"
" TODO: push this down to #discard_hunks?
function! gitgutter#diff#adjust_hunk_summary(diff_for_hunk, staging) abort
  let line_adjustment = gitgutter#hunk#line_adjustment_for_current_hunk()
  let adj_diff = []
  for line in split(a:diff_for_hunk, '\n')
    if match(line, s:hunk_re) != -1
      if a:staging
        " increment 'to' line number
        let line = substitute(line, '+\@<=\(\d\+\)', '\=submatch(1)+line_adjustment', '')
      else
        " decrement 'from' line number
        let line = substitute(line, '-\@<=\(\d\+\)', '\=submatch(1)-line_adjustment', '')
      endif
    endif
    call add(adj_diff, line)
  endfor
  return gitgutter#utility#stringify(adj_diff)
endfunction

