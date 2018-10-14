" yankstack.vim - keep track of your history of yanked/killed text
"
" Maintainer:   Max Brunsfeld <https://github.com/maxbrunsfeld>
" Version:      1.0.6
" Todo:
"

let s:yankstack_tail = []
let g:yankstack_size = 30
let s:last_paste = { 'changedtick': -1, 'key': '', 'mode': 'n', 'count': 1, 'register': '' }

if !exists('g:yankstack_yank_keys')
  let g:yankstack_yank_keys = ['c', 'C', 'd', 'D', 's', 'S', 'x', 'X', 'y', 'Y']
endif

function! s:yank_with_key(key)
  call s:before_yank()
  return a:key
endfunction

function! s:paste_with_key(key, mode, register, count)
  return s:paste_from_yankstack(a:key, a:mode, a:register, a:count, 1)
endfunction

function! s:paste_from_yankstack(key, mode, register, count, is_new)
  let keys = a:count . a:key
  let keys = (a:register == s:default_register()) ? keys : ('"' . a:register . keys)
  let s:last_paste = { 'key': a:key, 'mode': a:mode, 'register': a:register, 'count': a:count, 'changedtick': -1 }
  call feedkeys("\<Plug>yankstack_after_paste", "m")

  if a:mode == 'n'
    exec 'normal!' keys
  elseif a:mode == 'v'
    if a:is_new
      call s:before_yank()
      call feedkeys("\<Plug>yankstack_substitute_older_paste", "t")
      exec 'normal! gv' . keys
    else
      let head = s:get_yankstack_head()
      exec 'normal! gv' . keys
      call s:set_yankstack_head(head)
    endif

  " In insert mode, this function's return value is used in an
  " expression mapping. In other modes, it is called for its
  " side effects only.
  elseif a:mode == 'i'
    return keys
  endif

  silent! call repeat#setreg(a:register)
  silent! call repeat#set(a:key, a:count)
endfunction

function! s:substitute_paste(offset, current_mode)
  if s:last_change_was_paste()
    silent undo
    call s:yankstack_rotate(a:offset)
    return s:paste_from_yankstack(s:last_paste.key, s:last_paste.mode, s:last_paste.register, s:last_paste.count, 0)
  else
    return s:paste_from_yankstack(s:default_paste_key(a:current_mode), a:current_mode, v:register, '', 1)
  endif
endfunction

function! s:before_yank()
  let head = s:get_yankstack_head()
  if !empty(head.text) && (empty(s:yankstack_tail) || (head != s:yankstack_tail[0]))
    call insert(s:yankstack_tail, head)
    let s:yankstack_tail = s:yankstack_tail[: g:yankstack_size-1]
  endif
endfunction

function! s:yankstack_rotate(offset)
  if empty(s:yankstack_tail) | return | endif
  let offset_left = a:offset
  while offset_left != 0
    let head = s:get_yankstack_head()
    if offset_left > 0
      let entry = remove(s:yankstack_tail, 0)
      call add(s:yankstack_tail, head)
      let offset_left -= 1
    elseif offset_left < 0
      let entry = remove(s:yankstack_tail, -1)
      call insert(s:yankstack_tail, head)
      let offset_left += 1
    endif
    call s:set_yankstack_head(entry)
  endwhile
endfunction

function! s:get_yankstack_head()
  let reg = s:default_register()
  return { 'text': getreg(reg), 'type': getregtype(reg) }
endfunction

function! s:set_yankstack_head(entry)
  let reg = s:default_register()
  call setreg(reg, a:entry.text, a:entry.type)
endfunction

function! s:after_paste()
  let s:last_paste.changedtick = b:changedtick
endfunction

function! s:last_change_was_paste()
  return b:changedtick == s:last_paste.changedtick
endfunction

function! s:default_register()
  let clipboard_flags = split(&clipboard, ',')
  if index(clipboard_flags, 'unnamedplus') >= 0
    return "+"
  elseif index(clipboard_flags, 'unnamed') >= 0
    return "*"
  else
    return "\""
  endif
endfunction

function! s:default_paste_key(mode)
  if a:mode == 'i'
    return "\<C-g>u\<C-r>" . s:default_register()
  else
    return "p"
  endif
endfunction

function! g:Yankstack()
  return [s:get_yankstack_head()] + s:yankstack_tail
endfunction

command! -nargs=0 Yanks call s:show_yanks()
function! s:show_yanks()
  echohl WarningMsg | echo "--- Yanks ---" | echohl None
  let i = 0
  for yank in g:Yankstack()
    call s:show_yank(yank, i)
    let i += 1
  endfor
endfunction

function! s:show_yank(yank, index)
  let index = printf("%-4d", a:index)
  let lines = split(a:yank.text, '\n')
  let line = empty(lines) ? '' : lines[0]
  let line = substitute(line, '\t', repeat(' ', &tabstop), 'g')
  if len(line) > 80 || len(lines) > 1
    let line = line[: 80] . '…'
  endif

  echohl Directory | echo  index
  echohl None      | echon line
  echohl None
endfunction

function! yankstack#setup()
  if exists('g:yankstack_did_setup') | return | endif
  let g:yankstack_did_setup = 1

  let paste_keys = ['p', 'P', 'gp', 'gP']
  let word_characters = split("qwertyuiopasdfghjklzxcvbnm1234567890_", '\zs')

  for key in g:yankstack_yank_keys
    exec 'nnoremap <silent> <expr>'  key '<SID>yank_with_key("' . key . '")'
    exec 'xnoremap <silent> <expr>'  key '<SID>yank_with_key("' . key . '")'
  endfor

  for key in paste_keys
    exec 'nnoremap <silent>' key ':<C-u>call <SID>paste_with_key("' . key . '", "n", v:register, v:count1)<CR>'
    exec 'xnoremap <silent>' key ':<C-u>call <SID>paste_with_key("' . key . '", "v", v:register, v:count1)<CR>'
  endfor

  for key in word_characters
    exec 'smap <expr>' key '<SID>yank_with_key("' . key . '")'
  endfor
endfunction

nnoremap <silent> <Plug>yankstack_substitute_older_paste :<C-u>call <SID>substitute_paste(v:count1, 'n')<CR>
nnoremap <silent> <Plug>yankstack_substitute_newer_paste :<C-u>call <SID>substitute_paste(-v:count1, 'n')<CR>
xnoremap <silent> <Plug>yankstack_substitute_older_paste :<C-u>call <SID>substitute_paste(v:count1, 'v')<CR>
xnoremap <silent> <Plug>yankstack_substitute_newer_paste :<C-u>call <SID>substitute_paste(-v:count1, 'v')<CR>
inoremap <silent> <Plug>yankstack_substitute_older_paste <C-r>=<SID>substitute_paste(v:count1, 'i')<CR>
inoremap <silent> <Plug>yankstack_substitute_newer_paste <C-r>=<SID>substitute_paste(-v:count1, 'i')<CR>

nnoremap <silent> <Plug>yankstack_after_paste :call <SID>after_paste()<CR>
xnoremap <silent> <Plug>yankstack_after_paste :<C-u>call <SID>after_paste()<CR>
inoremap <silent> <Plug>yankstack_after_paste <C-o>:call <SID>after_paste()<CR>

if !exists('g:yankstack_map_keys') || g:yankstack_map_keys
  nmap <M-p> <Plug>yankstack_substitute_older_paste
  xmap <M-p> <Plug>yankstack_substitute_older_paste
  imap <M-p> <Plug>yankstack_substitute_older_paste
  nmap <M-P> <Plug>yankstack_substitute_newer_paste
  xmap <M-P> <Plug>yankstack_substitute_newer_paste
  imap <M-P> <Plug>yankstack_substitute_newer_paste
endif

