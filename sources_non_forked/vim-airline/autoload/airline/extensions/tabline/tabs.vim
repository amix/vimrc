" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:show_close_button = get(g:, 'airline#extensions#tabline#show_close_button', 1)
let s:show_tab_type = get(g:, 'airline#extensions#tabline#show_tab_type', 1)
let s:show_tab_nr = get(g:, 'airline#extensions#tabline#show_tab_nr', 1)
let s:tab_nr_type = get(g:, 'airline#extensions#tabline#tab_nr_type', 0)
let s:close_symbol = get(g:, 'airline#extensions#tabline#close_symbol', 'X')
let s:tabs_label = get(g:, 'airline#extensions#tabline#tabs_label', 'tabs')
let s:show_splits = get(g:, 'airline#extensions#tabline#show_splits', 1)
let s:spc = g:airline_symbols.space

let s:current_bufnr = -1
let s:current_tabnr = -1
let s:current_modified = 0

function! airline#extensions#tabline#tabs#off()
  augroup airline_tabline_tabs
    autocmd!
  augroup END
endfunction

function! airline#extensions#tabline#tabs#on()
  augroup airline_tabline_tabs
    autocmd!
    autocmd BufDelete * call airline#extensions#tabline#tabs#invalidate()
  augroup END
endfunction

function! airline#extensions#tabline#tabs#invalidate()
  let s:current_bufnr = -1
endfunction

function! airline#extensions#tabline#tabs#get()
  let curbuf = bufnr('%')
  let curtab = tabpagenr()
  call s:map_keys()
  if curbuf == s:current_bufnr && curtab == s:current_tabnr
    if !g:airline_detect_modified || getbufvar(curbuf, '&modified') == s:current_modified
      return s:current_tabline
    endif
  endif

  let b = airline#extensions#tabline#new_builder()

  for i in range(1, tabpagenr('$'))
    if i == curtab
      let group = 'airline_tabsel_right'
      if g:airline_detect_modified
        for bi in tabpagebuflist(i)
          if getbufvar(bi, '&modified')
            let group = 'airline_tabmod_right'
          endif
        endfor
      endif
      let s:current_modified = (group == 'airline_tabmod_right') ? 1 : 0
    else
      let group = 'airline_tab_right'
    endif
    let val = '%('
    if s:show_tab_nr
      if s:tab_nr_type == 0
        let val .= (g:airline_symbols.space).'%{len(tabpagebuflist('.i.'))}'
      elseif s:tab_nr_type == 1
        let val .= (g:airline_symbols.space).i
      else "== 2
        let val .= (g:airline_symbols.space).i.'.%{len(tabpagebuflist('.i.'))}'
      endif
    endif
    call b.add_section(group, val.'%'.i.'T %{airline#extensions#tabline#title('.i.')} %)')
  endfor

  call b.add_section('airline_tabfill', '')
  call b.split()
  call b.add_section('airline_tabfill', '')

  if s:show_close_button
    call b.add_section('airline_tab_right', ' %999X'.s:close_symbol.' ')
  endif

  if s:show_splits == 1
    let buffers = tabpagebuflist(curtab)
    for nr in buffers
      let group = airline#extensions#tabline#group_of_bufnr(buffers, nr)
      call b.add_section_spaced(group, '%(%{airline#extensions#tabline#get_buffer_name('.nr.')}%)')
    endfor
  elseif s:show_tab_type == 1
    call b.add_section_spaced('airline_tabtype', s:tabs_label)
  endif

  let s:current_bufnr = curbuf
  let s:current_tabnr = curtab
  let s:current_tabline = b.build()
  return s:current_tabline
endfunction

function s:map_keys()
  noremap <silent> <Plug>AirlineSelectTab1 :1tabn<CR>
  noremap <silent> <Plug>AirlineSelectTab2 :2tabn<CR>
  noremap <silent> <Plug>AirlineSelectTab3 :3tabn<CR>
  noremap <silent> <Plug>AirlineSelectTab4 :4tabn<CR>
  noremap <silent> <Plug>AirlineSelectTab5 :5tabn<CR>
  noremap <silent> <Plug>AirlineSelectTab6 :6tabn<CR>
  noremap <silent> <Plug>AirlineSelectTab7 :7tabn<CR>
  noremap <silent> <Plug>AirlineSelectTab8 :8tabn<CR>
  noremap <silent> <Plug>AirlineSelectTab9 :9tabn<CR>
  noremap <silent> <Plug>AirlineSelectPrevTab gT
  " tabn {count} goes to count tab does not go {count} tab pages forward!
  noremap <silent> <Plug>AirlineSelectNextTab :<C-U>exe repeat(':tabn\|', v:count1)<cr>
endfunction
