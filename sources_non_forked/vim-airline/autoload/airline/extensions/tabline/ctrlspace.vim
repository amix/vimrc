" MIT License. Copyright (c) 2016 Kevin Sapper
" vim: et ts=2 sts=2 sw=2

scriptencoding utf-8

let s:current_bufnr = -1
let s:current_tabnr = -1
let s:current_tabline = ''

let s:buffers_label = get(g:, 'airline#extensions#tabline#buffers_label', 'buffers')
let s:tabs_label = get(g:, 'airline#extensions#tabline#tabs_label', 'tabs')
let s:switch_buffers_and_tabs = get(g:, 'airline#extensions#tabline#switch_buffers_and_tabs', 0)
let s:show_buffers = get(g:, 'airline#extensions#tabline#show_buffers', 1)
let s:show_tabs = get(g:, 'airline#extensions#tabline#show_tabs', 1)

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

function! airline#extensions#tabline#ctrlspace#add_buffer_section(builder, cur_tab, cur_buf, pos)
  if a:pos == 0
    let pos_extension = ''
  else
    let pos_extension = '_right'
  endif

  let s:buffer_list = ctrlspace#api#BufferList(a:cur_tab)
  for buffer in s:buffer_list
      if a:cur_buf == buffer.index
        if buffer.modified
          let group = 'airline_tabmod'.pos_extension
        else
          let group = 'airline_tabsel'.pos_extension
        endif
      else
        if buffer.modified
          let group = 'airline_tabmod_unsel'.pos_extension
        elseif buffer.visible
          let group = 'airline_tab'.pos_extension
        else
          let group = 'airline_tabhid'.pos_extension
        endif
      endif

      let buf_name = '%(%{airline#extensions#tabline#get_buffer_name('.buffer.index.')}%)'

      if has("tablineat")
        let buf_name = '%'.buffer.index.'@airline#extensions#tabline#buffers#switchbuf@'.buf_name.'%X'
      endif

      call a:builder.add_section_spaced(group, buf_name)
  endfor
endfunction

function! airline#extensions#tabline#ctrlspace#add_tab_section(builder, pos)
  if a:pos == 0
    let pos_extension = ''
  else
    let pos_extension = '_right'
  endif

  for tab in s:tab_list
    if tab.current
      if tab.modified
        let group = 'airline_tabmod'.pos_extension
      else
        let group = 'airline_tabsel'.pos_extension
      endif
    else
      if tab.modified
        let group = 'airline_tabmod_unsel'.pos_extension
      else
        let group = 'airline_tabhid'.pos_extension
      endif
    endif

    call a:builder.add_section_spaced(group, '%'.tab.index.'T'.tab.title.ctrlspace#api#TabBuffersNumber(tab.index).'%T')
  endfor
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

  let builder = airline#extensions#tabline#new_builder()

  " Add left tabline content
  if s:show_buffers == 0
      call airline#extensions#tabline#ctrlspace#add_tab_section(builder, 0)
  elseif s:show_tabs == 0
      call airline#extensions#tabline#ctrlspace#add_buffer_section(builder, cur_tab, cur_buf, 0)
  else
    if s:switch_buffers_and_tabs == 0
      call builder.add_section_spaced('airline_tabtype', s:buffers_label)
      call airline#extensions#tabline#ctrlspace#add_buffer_section(builder, cur_tab, cur_buf, 0)
    else
      call builder.add_section_spaced('airline_tabtype', s:tabs_label)
      call airline#extensions#tabline#ctrlspace#add_tab_section(builder, 0)
    endif
  endif

  call builder.add_section('airline_tabfill', '')
  call builder.split()
  call builder.add_section('airline_tabfill', '')

  " Add right tabline content
  if s:show_buffers == 0
      call builder.add_section_spaced('airline_tabtype', s:tabs_label)
  elseif s:show_tabs == 0
      call builder.add_section_spaced('airline_tabtype', s:buffers_label)
  else
    if s:switch_buffers_and_tabs == 0
      call airline#extensions#tabline#ctrlspace#add_tab_section(builder, 1)
      call builder.add_section_spaced('airline_tabtype', s:tabs_label)
    else
      call airline#extensions#tabline#ctrlspace#add_buffer_section(builder, cur_tab, cur_buf, 1)
      call builder.add_section_spaced('airline_tabtype', s:buffers_label)
    endif
  endif

  let s:current_bufnr = cur_buf
  let s:current_tabnr = cur_tab
  let s:current_tabline = builder.build()
  return s:current_tabline
endfunction
