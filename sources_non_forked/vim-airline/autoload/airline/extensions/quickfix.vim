" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let g:airline#extensions#quickfix#quickfix_text = 'Quickfix'
let g:airline#extensions#quickfix#location_text = 'Location'

function! airline#extensions#quickfix#apply(...)
  if &buftype == 'quickfix'
    let w:airline_section_a = s:get_text()
    let w:airline_section_b = '%{get(w:, "quickfix_title", "")}'
    let w:airline_section_c = ''
    let w:airline_section_x = ''
  endif
endfunction

function! airline#extensions#quickfix#init(ext)
  call a:ext.add_statusline_func('airline#extensions#quickfix#apply')
endfunction

function! s:get_text()
  redir => buffers
  silent ls
  redir END

  let nr = bufnr('%')
  for buf in split(buffers, '\n')
    if match(buf, '\v^\s*'.nr) > -1
      if match(buf, '\cQuickfix') > -1
        return g:airline#extensions#quickfix#quickfix_text
      else
        return g:airline#extensions#quickfix#location_text
      endif
    endif
  endfor
  return ''
endfunction

