" MIT License. Copyright (c) 2016 Kevin Sapper
" vim: et ts=2 sts=2 sw=2

scriptencoding utf-8

let s:current_bufnr = -1
let s:current_tabnr = -1
let s:current_tabline = ''

function! airline#extensions#tabline#ctrlspace#off()
  augroup airline_tabline_ctrlspace
    autocmd!
  augroup END
endfunction

function! airline#extensions#tabline#ctrlspace#on()
  augroup airline_tabline_ctrlspace
    autocmd!
    autocmd BufDelete * call airline#extensions#tabline#ctrlspace#invalidate()
  augroup END
endfunction

function! airline#extensions#tabline#ctrlspace#invalidate()
  let s:current_bufnr = -1
  let s:current_tabnr = -1
endfunction

function! airline#extensions#tabline#ctrlspace#get()
  let cur_buf = bufnr('%')

  let s:tab_list = ctrlspace#api#TabList()
  for tab in s:tab_list
    if tab.current
      let cur_tab = tab.index
    endif
  endfor

  if cur_buf == s:current_bufnr && cur_tab == s:current_tabnr
    return s:current_tabline
  endif

  let b = airline#extensions#tabline#new_builder()

  call b.add_section_spaced('airline_tabtype', 'buffers')

  let s:buffer_list = ctrlspace#api#BufferList(cur_tab)
  for buffer in s:buffer_list
      if cur_buf == buffer.index
        if buffer.modified
          let group = 'airline_tabmod'
        else
          let group = 'airline_tabsel'
        endif
      else
        if buffer.modified
          let group = 'airline_tabmod_unsel'
        elseif buffer.visible
          let group = 'airline_tab'
        else
          let group = 'airline_tabhid'
        endif
      endif

      let buf_name = '%(%{airline#extensions#tabline#get_buffer_name('.buffer.index.')}%)'
      call b.add_section_spaced(group, buf_name)
  endfor


  call b.add_section('airline_tabfill', '')
  call b.split()
  call b.add_section('airline_tabfill', '')

  for tab in s:tab_list
    if tab.current
      if tab.modified
        let group = 'airline_tabmod_right'
      else
        let group = 'airline_tabsel_right'
      endif
    else
      if tab.modified
        let group = 'airline_tabmod_unsel_right'
      else
        let group = 'airline_tabhid_right'
      endif
    endif

    call b.add_section_spaced(group, tab.title.ctrlspace#api#TabBuffersNumber(tab.index))
  endfor

  call b.add_section_spaced('airline_tabtype', 'tabs')

  let s:current_bufnr = cur_buf
  let s:current_tabnr = cur_tab
  let s:current_tabline = b.build()
  return s:current_tabline
endfunction
