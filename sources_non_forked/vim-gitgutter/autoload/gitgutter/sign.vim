" Vim doesn't namespace sign ids so every plugin shares the same
" namespace.  Sign ids are simply integers so to avoid clashes with other
" signs we guess at a clear run.
"
" Note also we currently never reset s:next_sign_id.
let s:first_sign_id = 3000
let s:next_sign_id  = s:first_sign_id
let s:dummy_sign_id = s:first_sign_id - 1
" Remove-all-signs optimisation requires Vim 7.3.596+.
let s:supports_star = v:version > 703 || (v:version == 703 && has("patch596"))


function! gitgutter#sign#enable() abort
  let old_signs = g:gitgutter_signs

  let g:gitgutter_signs = 1
  call gitgutter#highlight#define_sign_text_highlights()

  if !old_signs && !g:gitgutter_highlight_lines
    call gitgutter#all(1)
  endif
endfunction

function! gitgutter#sign#disable() abort
  let g:gitgutter_signs = 0
  call gitgutter#highlight#define_sign_text_highlights()

  if !g:gitgutter_highlight_lines
    call gitgutter#sign#clear_signs(bufnr(''))
    call gitgutter#sign#remove_dummy_sign(bufnr(''), 0)
  endif
endfunction

function! gitgutter#sign#toggle() abort
  if g:gitgutter_signs
    call gitgutter#sign#disable()
  else
    call gitgutter#sign#enable()
  endif
endfunction


" Removes gitgutter's signs (excluding dummy sign) from the buffer being processed.
function! gitgutter#sign#clear_signs(bufnr) abort
  call s:find_current_signs(a:bufnr)

  let sign_ids = map(values(gitgutter#utility#getbufvar(a:bufnr, 'gitgutter_signs')), 'v:val.id')
  call s:remove_signs(a:bufnr, sign_ids, 1)
  call gitgutter#utility#setbufvar(a:bufnr, 'gitgutter_signs', {})
endfunction


" Updates gitgutter's signs in the buffer being processed.
"
" modified_lines: list of [<line_number (number)>, <name (string)>]
" where name = 'added|removed|modified|modified_removed'
function! gitgutter#sign#update_signs(bufnr, modified_lines) abort
  call s:find_current_signs(a:bufnr)

  let new_gitgutter_signs_line_numbers = map(copy(a:modified_lines), 'v:val[0]')
  let obsolete_signs = s:obsolete_gitgutter_signs_to_remove(a:bufnr, new_gitgutter_signs_line_numbers)

  let flicker_possible = s:remove_all_old_signs && !empty(a:modified_lines)
  if flicker_possible
    call s:add_dummy_sign(a:bufnr)
  endif

  call s:remove_signs(a:bufnr, obsolete_signs, s:remove_all_old_signs)
  call s:upsert_new_gitgutter_signs(a:bufnr, a:modified_lines)

  if flicker_possible
    call gitgutter#sign#remove_dummy_sign(a:bufnr, 0)
  endif
endfunction


function! s:add_dummy_sign(bufnr) abort
  if !gitgutter#utility#getbufvar(a:bufnr, 'dummy_sign')
    execute "sign place" s:dummy_sign_id "line=" . 9999 "name=GitGutterDummy buffer=" . a:bufnr
    call gitgutter#utility#setbufvar(a:bufnr, 'dummy_sign', 1)
  endif
endfunction

function! gitgutter#sign#remove_dummy_sign(bufnr, force) abort
  if gitgutter#utility#getbufvar(a:bufnr, 'dummy_sign') && (a:force || !g:gitgutter_sign_column_always)
    execute "sign unplace" s:dummy_sign_id "buffer=" . a:bufnr
    call gitgutter#utility#setbufvar(a:bufnr, 'dummy_sign', 0)
  endif
endfunction


"
" Internal functions
"


function! s:find_current_signs(bufnr) abort
  let gitgutter_signs = {}  " <line_number (string)>: {'id': <id (number)>, 'name': <name (string)>}
  let other_signs = []      " [<line_number (number),...]
  let dummy_sign_placed = 0

  redir => signs
    silent execute "sign place buffer=" . a:bufnr
  redir END

  for sign_line in filter(split(signs, '\n')[2:], 'v:val =~# "="')
    " Typical sign line:  line=88 id=1234 name=GitGutterLineAdded
    " We assume splitting is faster than a regexp.
    let components  = split(sign_line)
    let name        = split(components[2], '=')[1]
    if name =~# 'GitGutterDummy'
      let dummy_sign_placed = 1
    else
      let line_number = str2nr(split(components[0], '=')[1])
      if name =~# 'GitGutter'
        let id = str2nr(split(components[1], '=')[1])
        " Remove orphaned signs (signs placed on lines which have been deleted).
        " (When a line is deleted its sign lingers.  Subsequent lines' signs'
        " line numbers are decremented appropriately.)
        if has_key(gitgutter_signs, line_number)
          execute "sign unplace" gitgutter_signs[line_number].id
        endif
        let gitgutter_signs[line_number] = {'id': id, 'name': name}
      else
        call add(other_signs, line_number)
      endif
    end
  endfor

  call gitgutter#utility#setbufvar(a:bufnr, 'dummy_sign', dummy_sign_placed)
  call gitgutter#utility#setbufvar(a:bufnr, 'gitgutter_signs', gitgutter_signs)
  call gitgutter#utility#setbufvar(a:bufnr, 'other_signs', other_signs)
endfunction


" Returns a list of [<id (number)>, ...]
" Sets `s:remove_all_old_signs` as a side-effect.
function! s:obsolete_gitgutter_signs_to_remove(bufnr, new_gitgutter_signs_line_numbers) abort
  let signs_to_remove = []  " list of [<id (number)>, ...]
  let remove_all_signs = 1
  let old_gitgutter_signs = gitgutter#utility#getbufvar(a:bufnr, 'gitgutter_signs')
  for line_number in keys(old_gitgutter_signs)
    if index(a:new_gitgutter_signs_line_numbers, str2nr(line_number)) == -1
      call add(signs_to_remove, old_gitgutter_signs[line_number].id)
    else
      let remove_all_signs = 0
    endif
  endfor
  let s:remove_all_old_signs = remove_all_signs
  return signs_to_remove
endfunction


function! s:remove_signs(bufnr, sign_ids, all_signs) abort
  if a:all_signs && s:supports_star && empty(gitgutter#utility#getbufvar(a:bufnr, 'other_signs'))
    let dummy_sign_present = gitgutter#utility#getbufvar(a:bufnr, 'dummy_sign')
    execute "sign unplace * buffer=" . a:bufnr
    if dummy_sign_present
      execute "sign place" s:dummy_sign_id "line=" . 9999 "name=GitGutterDummy buffer=" . a:bufnr
    endif
  else
    for id in a:sign_ids
      execute "sign unplace" id
    endfor
  endif
endfunction


function! s:upsert_new_gitgutter_signs(bufnr, modified_lines) abort
  let other_signs         = gitgutter#utility#getbufvar(a:bufnr, 'other_signs')
  let old_gitgutter_signs = gitgutter#utility#getbufvar(a:bufnr, 'gitgutter_signs')

  " Handle special case where the first line is the site of two hunks:
  " lines deleted above at the start of the file, and lines deleted
  " immediately below.
  if a:modified_lines[0:1] == [[1, 'removed_first_line'], [1, 'removed']]
    let modified_lines = [[1, 'removed_above_and_below']] + a:modified_lines[2:]
  else
    let modified_lines = a:modified_lines
  endif

  for line in modified_lines
    let line_number = line[0]  " <number>
    if index(other_signs, line_number) == -1  " don't clobber others' signs
      let name = s:highlight_name_for_change(line[1])
      if !has_key(old_gitgutter_signs, line_number)  " insert
        let id = s:next_sign_id()
        execute "sign place" id "line=" . line_number "name=" . name "buffer=" . a:bufnr
      else  " update if sign has changed
        let old_sign = old_gitgutter_signs[line_number]
        if old_sign.name !=# name
          execute "sign place" old_sign.id "name=" . name "buffer=" . a:bufnr
        end
      endif
    endif
  endfor
  " At this point b:gitgutter_gitgutter_signs is out of date.
endfunction


function! s:next_sign_id() abort
  let next_id = s:next_sign_id
  let s:next_sign_id += 1
  return next_id
endfunction


" Only for testing.
function! gitgutter#sign#reset()
  let s:next_sign_id  = s:first_sign_id
endfunction


function! s:highlight_name_for_change(text) abort
  if a:text ==# 'added'
    return 'GitGutterLineAdded'
  elseif a:text ==# 'removed'
    return 'GitGutterLineRemoved'
  elseif a:text ==# 'removed_first_line'
    return 'GitGutterLineRemovedFirstLine'
  elseif a:text ==# 'modified'
    return 'GitGutterLineModified'
  elseif a:text ==# 'modified_removed'
    return 'GitGutterLineModifiedRemoved'
  elseif a:text ==# 'removed_above_and_below'
    return 'GitGutterLineRemovedAboveAndBelow'
  endif
endfunction


