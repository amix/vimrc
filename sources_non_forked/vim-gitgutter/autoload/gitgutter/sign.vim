" For older Vims without sign_place() the plugin has to manaage the sign ids.
let s:first_sign_id = 3000
let s:next_sign_id  = s:first_sign_id
" Remove-all-signs optimisation requires Vim 7.3.596+.
let s:supports_star = v:version > 703 || (v:version == 703 && has("patch596"))


function! gitgutter#sign#enable() abort
  let old_signs = g:gitgutter_signs

  let g:gitgutter_signs = 1
  call gitgutter#highlight#define_sign_text_highlights()

  if !old_signs && !g:gitgutter_highlight_lines && !g:gitgutter_highlight_linenrs
    call gitgutter#all(1)
  endif
endfunction

function! gitgutter#sign#disable() abort
  let g:gitgutter_signs = 0
  call gitgutter#highlight#define_sign_text_highlights()

  if !g:gitgutter_highlight_lines && !g:gitgutter_highlight_linenrs
    call gitgutter#sign#clear_signs(bufnr(''))
  endif
endfunction

function! gitgutter#sign#toggle() abort
  if g:gitgutter_signs
    call gitgutter#sign#disable()
  else
    call gitgutter#sign#enable()
  endif
endfunction


" Removes gitgutter's signs from the buffer being processed.
function! gitgutter#sign#clear_signs(bufnr) abort
  if exists('*sign_unplace')
    call sign_unplace('gitgutter', {'buffer': a:bufnr})
    return
  endif


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
  if exists('*sign_unplace')
    " Vim is (hopefully) now quick enough to remove all signs then place new ones.
    call sign_unplace('gitgutter', {'buffer': a:bufnr})

    let modified_lines = s:handle_double_hunk(a:modified_lines)
    let signs = map(copy(modified_lines), '{'.
          \ '"buffer":   a:bufnr,'.
          \ '"group":    "gitgutter",'.
          \ '"name":     s:highlight_name_for_change(v:val[1]),'.
          \ '"lnum":     v:val[0],'.
          \ '"priority": g:gitgutter_sign_priority'.
          \ '}')

    if exists('*sign_placelist')
      call sign_placelist(signs)
      return
    endif

    for sign in signs
      call sign_place(0, sign.group, sign.name, sign.buffer, {'lnum': sign.lnum, 'priority': sign.priority})
    endfor
    return
  endif


  " Derive a delta between the current signs and the ones we want.
  " Remove signs from lines that no longer need a sign.
  " Upsert the remaining signs.

  call s:find_current_signs(a:bufnr)

  let new_gitgutter_signs_line_numbers = map(copy(a:modified_lines), 'v:val[0]')
  let obsolete_signs = s:obsolete_gitgutter_signs_to_remove(a:bufnr, new_gitgutter_signs_line_numbers)

  call s:remove_signs(a:bufnr, obsolete_signs, s:remove_all_old_signs)
  call s:upsert_new_gitgutter_signs(a:bufnr, a:modified_lines)
endfunction


"
" Internal functions
"


function! s:find_current_signs(bufnr) abort
  let gitgutter_signs = {}  " <line_number (string)>: {'id': <id (number)>, 'name': <name (string)>}
  if !g:gitgutter_sign_allow_clobber
    let other_signs = []      " [<line_number (number),...]
  endif

  if exists('*getbufinfo')
    let bufinfo = getbufinfo(a:bufnr)[0]
    let signs = has_key(bufinfo, 'signs') ? bufinfo.signs : []
  else
    let signs = []

    redir => signlines
      silent execute "sign place buffer=" . a:bufnr
    redir END

    for signline in filter(split(signlines, '\n')[2:], 'v:val =~# "="')
      " Typical sign line before v8.1.0614:  line=88 id=1234 name=GitGutterLineAdded
      " We assume splitting is faster than a regexp.
      let components = split(signline)
      call add(signs, {
            \ 'lnum': str2nr(split(components[0], '=')[1]),
            \ 'id':   str2nr(split(components[1], '=')[1]),
            \ 'name':        split(components[2], '=')[1]
            \ })
    endfor
  endif

  for sign in signs
    if sign.name =~# 'GitGutter'
      " Remove orphaned signs (signs placed on lines which have been deleted).
      " (When a line is deleted its sign lingers.  Subsequent lines' signs'
      " line numbers are decremented appropriately.)
      if has_key(gitgutter_signs, sign.lnum)
        execute "sign unplace" gitgutter_signs[sign.lnum].id
      endif
      let gitgutter_signs[sign.lnum] = {'id': sign.id, 'name': sign.name}
    else
      if !g:gitgutter_sign_allow_clobber
        call add(other_signs, sign.lnum)
      endif
    endif
  endfor

  call gitgutter#utility#setbufvar(a:bufnr, 'gitgutter_signs', gitgutter_signs)
  if !g:gitgutter_sign_allow_clobber
    call gitgutter#utility#setbufvar(a:bufnr, 'other_signs', other_signs)
  endif
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
  if a:all_signs && s:supports_star && (g:gitgutter_sign_allow_clobber || empty(gitgutter#utility#getbufvar(a:bufnr, 'other_signs')))
    execute "sign unplace * buffer=" . a:bufnr
  else
    for id in a:sign_ids
      execute "sign unplace" id
    endfor
  endif
endfunction


function! s:upsert_new_gitgutter_signs(bufnr, modified_lines) abort
  if !g:gitgutter_sign_allow_clobber
    let other_signs = gitgutter#utility#getbufvar(a:bufnr, 'other_signs')
  endif
  let old_gitgutter_signs = gitgutter#utility#getbufvar(a:bufnr, 'gitgutter_signs')

  let modified_lines = s:handle_double_hunk(a:modified_lines)

  for line in modified_lines
    let line_number = line[0]  " <number>
    if g:gitgutter_sign_allow_clobber || index(other_signs, line_number) == -1  " don't clobber others' signs
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


" Handle special case where the first line is the site of two hunks:
" lines deleted above at the start of the file, and lines deleted
" immediately below.
function! s:handle_double_hunk(modified_lines)
  if a:modified_lines[0:1] == [[1, 'removed_first_line'], [1, 'removed']]
    return [[1, 'removed_above_and_below']] + a:modified_lines[2:]
  endif

  return a:modified_lines
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


