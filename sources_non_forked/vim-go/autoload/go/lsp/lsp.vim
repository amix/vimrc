" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

" go#lsp#lsp#Position returns the LSP text position. If no arguments are
" provided, the cursor position is assumed. Otherwise, there should be two
" arguments: the line and the column.
function! go#lsp#lsp#Position(...)
  if a:0 < 2
    let [l:line, l:col] = getpos('.')[1:2]
  else
    let l:line = a:1
    let l:col = a:2
  endif
  let l:content = getline(l:line)

  " LSP uses 0-based lines.
  return [l:line - 1, s:character(l:line, l:col-1)]
endfunction

function! s:strlen(str) abort
  let l:runes = split(a:str, '\zs')
  return len(l:runes) + len(filter(l:runes, 'char2nr(v:val)>=0x10000'))
endfunction

function! s:character(line, col) abort
  return s:strlen(getline(a:line)[:col([a:line, a:col - 1])])
endfunction

" go#lsp#PositionOf returns len(content[0:units]) where units is utf-16 code
" units. This is mostly useful for converting LSP text position to vim
" position.
function! go#lsp#lsp#PositionOf(content, units) abort
  if a:units == 0
    return 1
  endif

  let l:remaining = a:units
  let l:str = ""
  for l:rune in split(a:content, '\zs')
    if l:remaining < 0
      break
    endif
    let l:remaining -= 1
    if char2nr(l:rune) >= 0x10000
      let l:remaining -= 1
    endif
    let l:str = l:str . l:rune
  endfor

  return len(l:str)
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
