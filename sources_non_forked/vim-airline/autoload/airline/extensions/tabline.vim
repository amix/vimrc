" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:formatter = get(g:, 'airline#extensions#tabline#formatter', 'default')
let s:excludes = get(g:, 'airline#extensions#tabline#excludes', [])
let s:tab_nr_type = get(g:, 'airline#extensions#tabline#tab_nr_type', 0)
let s:show_buffers = get(g:, 'airline#extensions#tabline#show_buffers', 1)
let s:show_tab_nr = get(g:, 'airline#extensions#tabline#show_tab_nr', 1)
let s:show_tab_type = get(g:, 'airline#extensions#tabline#show_tab_type', 1)
let s:show_close_button = get(g:, 'airline#extensions#tabline#show_close_button', 1)
let s:close_symbol = get(g:, 'airline#extensions#tabline#close_symbol', 'X')
let s:buffer_idx_mode = get(g:, 'airline#extensions#tabline#buffer_idx_mode', 0)

let s:builder_context = {
      \ 'active'        : 1,
      \ 'right_sep'     : get(g:, 'airline#extensions#tabline#right_sep'    , g:airline_right_sep),
      \ 'right_alt_sep' : get(g:, 'airline#extensions#tabline#right_alt_sep', g:airline_right_alt_sep),
      \ }
if get(g:, 'airline_powerline_fonts', 0)
  let s:builder_context.left_sep     = get(g:, 'airline#extensions#tabline#left_sep'     , g:airline_left_sep)
  let s:builder_context.left_alt_sep = get(g:, 'airline#extensions#tabline#left_alt_sep' , g:airline_left_alt_sep)
else
  let s:builder_context.left_sep     = get(g:, 'airline#extensions#tabline#left_sep'     , ' ')
  let s:builder_context.left_alt_sep = get(g:, 'airline#extensions#tabline#left_alt_sep' , '|')
endif

let s:buf_min_count = get(g:, 'airline#extensions#tabline#buffer_min_count', 0)
let s:tab_min_count = get(g:, 'airline#extensions#tabline#tab_min_count', 0)
let s:spc = g:airline_symbols.space

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

function! airline#extensions#tabline#init(ext)
  if has('gui_running')
    set guioptions-=e
  endif

  autocmd User AirlineToggledOn call s:toggle_on()
  autocmd User AirlineToggledOff call s:toggle_off()
  autocmd BufDelete * let s:current_bufnr = -1

  call s:toggle_on()
  call a:ext.add_theme_func('airline#extensions#tabline#load_theme')
  if s:buffer_idx_mode
    call s:define_buffer_idx_mode_mappings()
  endif
endfunction

function! s:toggle_off()
  if exists('s:original_tabline')
    let &tabline = s:original_tabline
    let &showtabline = s:original_showtabline
  endif
endfunction

function! s:toggle_on()
  let [ s:original_tabline, s:original_showtabline ] = [ &tabline, &showtabline ]

  set tabline=%!airline#extensions#tabline#get()
  if s:buf_min_count <= 0 && s:tab_min_count <= 1
    set showtabline=2
  else
    augroup airline_tabline
      autocmd!
      if s:show_buffers == 1
        autocmd CursorMoved * call <sid>on_cursormove(s:buf_min_count, len(s:get_buffer_list()))
      else
        autocmd TabEnter * call <sid>on_cursormove(s:tab_min_count, tabpagenr('$'))
      endif
    augroup END
  endif
endfunction

function! airline#extensions#tabline#load_theme(palette)
  let colors    = get(a:palette, 'tabline', {})
  let l:tab     = get(colors, 'airline_tab', a:palette.normal.airline_b)
  let l:tabsel  = get(colors, 'airline_tabsel', a:palette.normal.airline_a)
  let l:tabtype = get(colors, 'airline_tabtype', a:palette.visual.airline_a)
  let l:tabfill = get(colors, 'airline_tabfill', a:palette.normal.airline_c)
  let l:tabmod  = get(colors, 'airline_tabmod', a:palette.insert.airline_a)
  if has_key(a:palette, 'normal_modified') && has_key(a:palette.normal_modified, 'airline_c')
    let l:tabmodu = get(colors, 'airline_tabmod_unsel', a:palette.normal_modified.airline_c)
  else
    "Fall back to normal airline_c if modified airline_c isn't present
    let l:tabmodu = get(colors, 'airline_tabmod_unsel', a:palette.normal.airline_c)
  endif

  let l:tabhid  = get(colors, 'airline_tabhid', a:palette.normal.airline_c)
  call airline#highlighter#exec('airline_tab', l:tab)
  call airline#highlighter#exec('airline_tabsel', l:tabsel)
  call airline#highlighter#exec('airline_tabtype', l:tabtype)
  call airline#highlighter#exec('airline_tabfill', l:tabfill)
  call airline#highlighter#exec('airline_tabmod', l:tabmod)
  call airline#highlighter#exec('airline_tabmod_unsel', l:tabmodu)
  call airline#highlighter#exec('airline_tabhid', l:tabhid)
endfunction

function! s:on_cursormove(min_count, total_count)
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

function! airline#extensions#tabline#get()
  let curtabcnt = tabpagenr('$')
  if curtabcnt != s:current_tabcnt
    let s:current_tabcnt = curtabcnt
    let s:current_bufnr = -1  " force a refresh...
  endif
  if s:show_buffers && curtabcnt == 1
    return s:get_buffers()
  else
    return s:get_tabs()
  endif
endfunction

function! airline#extensions#tabline#title(n)
  let buflist = tabpagebuflist(a:n)
  let winnr = tabpagewinnr(a:n)
  return airline#extensions#tabline#get_buffer_name(buflist[winnr - 1])
endfunction

function! airline#extensions#tabline#get_buffer_name(nr)
  return airline#extensions#tabline#{s:formatter}#format(a:nr, get(s:, 'current_buffer_list', s:get_buffer_list()))
endfunction

function! s:get_buffer_list()
  let buffers = []
  let cur = bufnr('%')
  for nr in range(1, bufnr('$'))
    if buflisted(nr) && bufexists(nr)
      for ex in s:excludes
        if match(bufname(nr), ex)
          continue
        endif
      endfor
      if getbufvar(nr, 'current_syntax') == 'qf'
        continue
      endif
      call add(buffers, nr)
    endif
  endfor

  let s:current_buffer_list = buffers
  return buffers
endfunction

function! s:get_visible_buffers()
  let buffers = s:get_buffer_list()
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

  let g:current_visible_buffers = buffers
  return buffers
endfunction

let s:current_bufnr = -1
let s:current_tabnr = -1
let s:current_tabcnt = -1
let s:current_tabline = ''
let s:current_modified = 0
function! s:get_buffers()
  let cur = bufnr('%')
  if cur == s:current_bufnr
    if !g:airline_detect_modified || getbufvar(cur, '&modified') == s:current_modified
      return s:current_tabline
    endif
  endif

  let l:index = 1
  let b = airline#builder#new(s:builder_context)
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
  call b.add_section('airline_tabtype', ' buffers ')

  let s:current_bufnr = cur
  let s:current_tabline = b.build()
  return s:current_tabline
endfunction

function! s:select_tab(buf_index)
  " no-op when called in the NERDTree buffer
  if exists('t:NERDTreeBufName') && bufname('%') == t:NERDTreeBufName
    return
  endif

  let idx = a:buf_index
  if g:current_visible_buffers[0] == -1
    let idx = idx + 1
  endif

  let buf = get(g:current_visible_buffers, idx, 0)
  if buf != 0
    exec 'b!' . buf
  endif
endfunction

function! s:define_buffer_idx_mode_mappings()
  noremap <unique> <Plug>AirlineSelectTab1 :call <SID>select_tab(0)<CR>
  noremap <unique> <Plug>AirlineSelectTab2 :call <SID>select_tab(1)<CR>
  noremap <unique> <Plug>AirlineSelectTab3 :call <SID>select_tab(2)<CR>
  noremap <unique> <Plug>AirlineSelectTab4 :call <SID>select_tab(3)<CR>
  noremap <unique> <Plug>AirlineSelectTab5 :call <SID>select_tab(4)<CR>
  noremap <unique> <Plug>AirlineSelectTab6 :call <SID>select_tab(5)<CR>
  noremap <unique> <Plug>AirlineSelectTab7 :call <SID>select_tab(6)<CR>
  noremap <unique> <Plug>AirlineSelectTab8 :call <SID>select_tab(7)<CR>
  noremap <unique> <Plug>AirlineSelectTab9 :call <SID>select_tab(8)<CR>
endfunction

function! s:get_tabs()
  let curbuf = bufnr('%')
  let curtab = tabpagenr()
  if curbuf == s:current_bufnr && curtab == s:current_tabnr
    if !g:airline_detect_modified || getbufvar(curbuf, '&modified') == s:current_modified
      return s:current_tabline
    endif
  endif

  let b = airline#builder#new(s:builder_context)
  for i in range(1, tabpagenr('$'))
    if i == curtab
      let group = 'airline_tabsel'
      if g:airline_detect_modified
        for bi in tabpagebuflist(i)
          if getbufvar(bi, '&modified')
            let group = 'airline_tabmod'
          endif
        endfor
      endif
      let s:current_modified = (group == 'airline_tabmod') ? 1 : 0
    else
      let group = 'airline_tab'
    endif
    let val = '%('
    if s:show_tab_nr
      if s:tab_nr_type == 0
        let val .= ' %{len(tabpagebuflist('.i.'))}'
      else
        let val .= (g:airline_symbols.space).i
      endif
    endif
    call b.add_section(group, val.'%'.i.'T %{airline#extensions#tabline#title('.i.')} %)')
  endfor

  call b.add_raw('%T')
  call b.add_section('airline_tabfill', '')
  call b.split()
  if s:show_close_button
    call b.add_section('airline_tab', ' %999X'.s:close_symbol.' ')
  endif
  if s:show_tab_type
    call b.add_section('airline_tabtype', ' tabs ')
  endif

  let s:current_bufnr = curbuf
  let s:current_tabnr = curtab
  let s:current_tabline = b.build()
  return s:current_tabline
endfunction
