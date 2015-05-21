" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

scriptencoding utf-8

let s:buffer_idx_mode = get(g:, 'airline#extensions#tabline#buffer_idx_mode', 0)
let s:show_tab_type = get(g:, 'airline#extensions#tabline#show_tab_type', 1)
let s:spc = g:airline_symbols.space

let s:current_bufnr = -1
let s:current_modified = 0
let s:current_tabline = ''
let s:current_visible_buffers = []

let s:number_map = &encoding == 'utf-8'
      \ ? {
      \ '0': '⁰',
      \ '1': '¹',
      \ '2': '²',
      \ '3': '³',
      \ '4': '⁴',
      \ '5': '⁵',
      \ '6': '⁶',
      \ '7': '⁷',
      \ '8': '⁸',
      \ '9': '⁹'
      \ }
      \ : {}

function! airline#extensions#tabline#buffers#off()
  augroup airline_tabline_buffers
    autocmd!
  augroup END
endfunction

function! airline#extensions#tabline#buffers#on()
  augroup airline_tabline_buffers
    autocmd!
    autocmd BufDelete * call airline#extensions#tabline#buffers#invalidate()
  augroup END
endfunction

function! airline#extensions#tabline#buffers#invalidate()
  let s:current_bufnr = -1
endfunction

function! airline#extensions#tabline#buffers#get()
  let cur = bufnr('%')
  if cur == s:current_bufnr
    if !g:airline_detect_modified || getbufvar(cur, '&modified') == s:current_modified
      return s:current_tabline
    endif
  endif

  let l:index = 1
  let b = airline#extensions#tabline#new_builder()
  let tab_bufs = tabpagebuflist(tabpagenr())
  for nr in s:get_visible_buffers()
    if nr < 0
      call b.add_raw('%#airline_tabhid#...')
      continue
    endif

    if cur == nr
      if g:airline_detect_modified && getbufvar(nr, '&modified')
        let group = 'airline_tabmod'
      else
        let group = 'airline_tabsel'
      endif
      let s:current_modified = (group == 'airline_tabmod') ? 1 : 0
    else
      if g:airline_detect_modified && getbufvar(nr, '&modified')
        let group = 'airline_tabmod_unsel'
      elseif index(tab_bufs, nr) > -1
        let group = 'airline_tab'
      else
        let group = 'airline_tabhid'
      endif
    endif

    if s:buffer_idx_mode
      if len(s:number_map) > 0
        call b.add_section(group, s:spc . get(s:number_map, l:index, '') . '%(%{airline#extensions#tabline#get_buffer_name('.nr.')}%)' . s:spc)
      else
        call b.add_section(group, '['.l:index.s:spc.'%(%{airline#extensions#tabline#get_buffer_name('.nr.')}%)'.']')
      endif
      let l:index = l:index + 1
    else
      call b.add_section(group, s:spc.'%(%{airline#extensions#tabline#get_buffer_name('.nr.')}%)'.s:spc)
    endif
  endfor

  call b.add_section('airline_tabfill', '')
  call b.split()
  call b.add_section('airline_tabfill', '')
  if s:show_tab_type
    call b.add_section('airline_tabtype', ' buffers ')
  endif

  let s:current_bufnr = cur
  let s:current_tabline = b.build()
  return s:current_tabline
endfunction

function! s:get_visible_buffers()
  let buffers = airline#extensions#tabline#buflist#list()
  let cur = bufnr('%')

  let total_width = 0
  let max_width = 0

  for nr in buffers
    let width = len(airline#extensions#tabline#get_buffer_name(nr)) + 4
    let total_width += width
    let max_width = max([max_width, width])
  endfor

  " only show current and surrounding buffers if there are too many buffers
  let position  = index(buffers, cur)
  let vimwidth = &columns
  if total_width > vimwidth && position > -1
    let buf_count = len(buffers)

    " determine how many buffers to show based on the longest buffer width,
    " use one on the right side and put the rest on the left
    let buf_max   = vimwidth / max_width
    let buf_right = 1
    let buf_left  = max([0, buf_max - buf_right])

    let start = max([0, position - buf_left])
    let end   = min([buf_count, position + buf_right])

    " fill up available space on the right
    if position < buf_left
      let end += (buf_left - position)
    endif

    " fill up available space on the left
    if end > buf_count - 1 - buf_right
      let start -= max([0, buf_right - (buf_count - 1 - position)])
    endif

    let buffers = eval('buffers[' . start . ':' . end . ']')

    if start > 0
      call insert(buffers, -1, 0)
    endif

    if end < buf_count - 1
      call add(buffers, -1)
    endif
  endif

  let s:current_visible_buffers = buffers
  return buffers
endfunction

function! s:select_tab(buf_index)
  " no-op when called in the NERDTree buffer
  if exists('t:NERDTreeBufName') && bufname('%') == t:NERDTreeBufName
    return
  endif

  let idx = a:buf_index
  if s:current_visible_buffers[0] == -1
    let idx = idx + 1
  endif

  let buf = get(s:current_visible_buffers, idx, 0)
  if buf != 0
    exec 'b!' . buf
  endif
endfunction

function! s:jump_to_tab(offset)
    let l = s:current_visible_buffers
    let i = index(l, bufnr('%'))
    if i > -1
        exec 'b!' . l[float2nr(fmod(i + a:offset, len(l)))]
    endif
endfunction

if s:buffer_idx_mode
  noremap <unique> <Plug>AirlineSelectTab1 :call <SID>select_tab(0)<CR>
  noremap <unique> <Plug>AirlineSelectTab2 :call <SID>select_tab(1)<CR>
  noremap <unique> <Plug>AirlineSelectTab3 :call <SID>select_tab(2)<CR>
  noremap <unique> <Plug>AirlineSelectTab4 :call <SID>select_tab(3)<CR>
  noremap <unique> <Plug>AirlineSelectTab5 :call <SID>select_tab(4)<CR>
  noremap <unique> <Plug>AirlineSelectTab6 :call <SID>select_tab(5)<CR>
  noremap <unique> <Plug>AirlineSelectTab7 :call <SID>select_tab(6)<CR>
  noremap <unique> <Plug>AirlineSelectTab8 :call <SID>select_tab(7)<CR>
  noremap <unique> <Plug>AirlineSelectTab9 :call <SID>select_tab(8)<CR>
  noremap <unique> <Plug>AirlineSelectPrevTab :<C-u>call <SID>jump_to_tab(-v:count1)<CR>
  noremap <unique> <Plug>AirlineSelectNextTab :<C-u>call <SID>jump_to_tab(v:count1)<CR>
endif
