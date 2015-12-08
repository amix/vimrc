" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:show_buffers = get(g:, 'airline#extensions#tabline#show_buffers', 1)
let s:buf_min_count = get(g:, 'airline#extensions#tabline#buffer_min_count', 0)
let s:tab_min_count = get(g:, 'airline#extensions#tabline#tab_min_count', 0)

function! airline#extensions#tabline#autoshow#off()
  if exists('s:original_tabline')
    let &tabline = s:original_tabline
    let &showtabline = s:original_showtabline
  endif

  augroup airline_tabline_autoshow
    autocmd!
  augroup END
endfunction

function! airline#extensions#tabline#autoshow#on()
  let [ s:original_tabline, s:original_showtabline ] = [ &tabline, &showtabline ]

  augroup airline_tabline_autoshow
    autocmd!
    if s:buf_min_count <= 0 && s:tab_min_count <= 1
      set showtabline=2
    else
      if s:show_buffers == 1
        autocmd BufEnter  * call <sid>show_tabline(s:buf_min_count, len(airline#extensions#tabline#buflist#list()))
        autocmd BufUnload * call <sid>show_tabline(s:buf_min_count, len(airline#extensions#tabline#buflist#list()) - 1)
      else
        autocmd TabEnter  * call <sid>show_tabline(s:tab_min_count, tabpagenr('$'))
      endif
    endif

    " Invalidate cache.  This has to come after the BufUnload for
    " s:show_buffers, to invalidate the cache for BufEnter.
    autocmd BufLeave,BufAdd,BufUnload * call airline#extensions#tabline#buflist#invalidate()
  augroup END
endfunction

function! s:show_tabline(min_count, total_count)
  if a:total_count >= a:min_count
    if &showtabline != 2
      set showtabline=2
    endif
  else
    if &showtabline != 0
      set showtabline=0
    endif
  endif
endfunction
