if exists('g:loaded_system_copy') || v:version < 700
  finish
endif
let g:loaded_system_copy = 1

let s:blockwise = 'blockwise visual'
let s:visual = 'visual'
let s:motion = 'motion'
let s:linewise = 'linewise'
let s:mac = 'mac'
let s:windows = 'windows'
let s:linux = 'linux'

function! s:system_copy(type, ...) abort
  let mode = <SID>resolve_mode(a:type, a:0)
  if mode == s:linewise
    let lines = { 'start': line("'["), 'end': line("']") }
    silent exe lines.start . "," . lines.end . "y"
  elseif mode == s:visual || mode == s:blockwise
    silent exe "normal! `<" . a:type . "`>y"
  else
    silent exe "normal! `[v`]y"
  endif
  let command = s:CopyCommandForCurrentOS()
  silent call system(command, getreg('@'))
  echohl String | echon 'Copied to clipboard using: ' . command | echohl None
endfunction

function! s:system_paste() abort
  let command = <SID>PasteCommandForCurrentOS()
  put =system(command)
  echohl String | echon 'Pasted to vim using: ' . command | echohl None
endfunction

function! s:resolve_mode(type, arg)
  let visual_mode = a:arg != 0
  if visual_mode
    return (a:type == '') ?  s:blockwise : s:visual
  elseif a:type == 'line'
    return s:linewise
  else
    return s:motion
  endif
endfunction

function! s:currentOS()
  let os = substitute(system('uname'), '\n', '', '')
  let known_os = 'unknown'
  if has("gui_mac") || os ==? 'Darwin'
    let known_os = s:mac
  elseif has("gui_win32")
    let known_os = s:windows
  elseif os ==? 'Linux'
    let known_os = s:linux
  else
    exe "normal \<Esc>"
    throw "unknown OS: " . os
  endif
  return known_os
endfunction

function! s:CopyCommandForCurrentOS()
  if exists('g:system_copy#copy_command')
    return g:system_copy#copy_command
  endif
  let os = <SID>currentOS()
  if os == s:mac
    return 'pbcopy'
  elseif os == s:windows
    return 'clip'
  elseif os == s:linux
    return 'xsel --clipboard --input'
  endif
endfunction

function! s:PasteCommandForCurrentOS()
  if exists('g:system_copy#paste_command')
    return g:system_copy#paste_command
  endif
  let os = <SID>currentOS()
  if os == s:mac
    return 'pbpaste'
  elseif os == s:windows
    return 'paste'
  elseif os == s:linux
    return 'xsel --clipboard --output'
  endif
endfunction

xnoremap <silent> <Plug>SystemCopy :<C-U>call <SID>system_copy(visualmode(),visualmode() ==# 'V' ? 1 : 0)<CR>
nnoremap <silent> <Plug>SystemCopy :<C-U>set opfunc=<SID>system_copy<CR>g@
nnoremap <silent> <Plug>SystemCopyLine :<C-U>set opfunc=<SID>system_copy<Bar>exe 'norm! 'v:count1.'g@_'<CR>
nnoremap <silent> <Plug>SystemPaste :<C-U>call <SID>system_paste()<CR>

if !hasmapto('<Plug>SystemCopy', 'n') || maparg('cp', 'n') ==# ''
  nmap cp <Plug>SystemCopy
endif

if !hasmapto('<Plug>SystemCopy', 'v') || maparg('cp', 'v') ==# ''
  xmap cp <Plug>SystemCopy
endif

if !hasmapto('<Plug>SystemCopyLine', 'n') || maparg('cP', 'n') ==# ''
  nmap cP <Plug>SystemCopyLine
endif

if !hasmapto('<Plug>SystemPaste', 'n') || maparg('cv', 'n') ==# ''
  nmap cv <Plug>SystemPaste
endif

" vim:ts=2:sw=2:sts=2
