" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:formatter = get(g:, 'airline#extensions#tabline#formatter', 'default')
let s:show_buffers = get(g:, 'airline#extensions#tabline#show_buffers', 1)
let s:show_tabs = get(g:, 'airline#extensions#tabline#show_tabs', 1)

let s:taboo = get(g:, 'airline#extensions#taboo#enabled', 1) && get(g:, 'loaded_taboo', 0)
if s:taboo
  let g:taboo_tabline = 0
endif


function! airline#extensions#tabline#init(ext)
  if has('gui_running')
    set guioptions-=e
  endif

  autocmd User AirlineToggledOn call s:toggle_on()
  autocmd User AirlineToggledOff call s:toggle_off()

  call s:toggle_on()
  call a:ext.add_theme_func('airline#extensions#tabline#load_theme')
endfunction

function! s:toggle_off()
  call airline#extensions#tabline#autoshow#off()
  call airline#extensions#tabline#tabs#off()
  call airline#extensions#tabline#buffers#off()
endfunction

function! s:toggle_on()
  call airline#extensions#tabline#autoshow#on()
  call airline#extensions#tabline#tabs#on()
  call airline#extensions#tabline#buffers#on()

  set tabline=%!airline#extensions#tabline#get()
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

let s:current_tabcnt = -1
function! airline#extensions#tabline#get()
  let curtabcnt = tabpagenr('$')
  if curtabcnt != s:current_tabcnt
    let s:current_tabcnt = curtabcnt
    call airline#extensions#tabline#tabs#invalidate()
    call airline#extensions#tabline#buffers#invalidate()
  endif

  if s:show_buffers && curtabcnt == 1 || !s:show_tabs
    return airline#extensions#tabline#buffers#get()
  else
    return airline#extensions#tabline#tabs#get()
  endif
endfunction

function! airline#extensions#tabline#title(n)
  let title = ''
  if s:taboo
    let title = TabooTabTitle(a:n)
  endif

  if empty(title)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    return airline#extensions#tabline#get_buffer_name(buflist[winnr - 1])
  endif

  return title
endfunction

function! airline#extensions#tabline#get_buffer_name(nr)
  return airline#extensions#tabline#formatters#{s:formatter}#format(a:nr, airline#extensions#tabline#buflist#list())
endfunction

function! airline#extensions#tabline#new_builder()
  let builder_context = {
        \ 'active'        : 1,
        \ 'right_sep'     : get(g:, 'airline#extensions#tabline#right_sep'    , g:airline_right_sep),
        \ 'right_alt_sep' : get(g:, 'airline#extensions#tabline#right_alt_sep', g:airline_right_alt_sep),
        \ }
  if get(g:, 'airline_powerline_fonts', 0)
    let builder_context.left_sep     = get(g:, 'airline#extensions#tabline#left_sep'     , g:airline_left_sep)
    let builder_context.left_alt_sep = get(g:, 'airline#extensions#tabline#left_alt_sep' , g:airline_left_alt_sep)
  else
    let builder_context.left_sep     = get(g:, 'airline#extensions#tabline#left_sep'     , ' ')
    let builder_context.left_alt_sep = get(g:, 'airline#extensions#tabline#left_alt_sep' , '|')
  endif

  return airline#builder#new(builder_context)
endfunction
