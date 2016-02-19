" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:formatter = get(g:, 'airline#extensions#tabline#formatter', 'default')
let s:show_buffers = get(g:, 'airline#extensions#tabline#show_buffers', 1)
let s:show_tabs = get(g:, 'airline#extensions#tabline#show_tabs', 1)
let s:ignore_bufadd_pat = get(g:, 'airline#extensions#tabline#ignore_bufadd_pat', '\c\vgundo|undotree|vimfiler|tagbar|nerd_tree')
let s:taboo = get(g:, 'airline#extensions#taboo#enabled', 1) && get(g:, 'loaded_taboo', 0)
if s:taboo
  let g:taboo_tabline = 0
endif

let s:ctrlspace = get(g:, 'CtrlSpaceLoaded', 0)

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
  call airline#extensions#tabline#ctrlspace#off()
endfunction

function! s:toggle_on()
  call airline#extensions#tabline#autoshow#on()
  call airline#extensions#tabline#tabs#on()
  call airline#extensions#tabline#buffers#on()
  call airline#extensions#tabline#ctrlspace#on()

  set tabline=%!airline#extensions#tabline#get()
endfunction

function! s:update_tabline()
  let match = expand('<afile>')
  if pumvisible()
    return
  elseif !get(g:, 'airline#extensions#tabline#enabled', 0)
    return
  " return, if buffer matches ignore pattern or is directory (netrw)
  elseif empty(match) 
        \ || match(match, s:ignore_bufadd_pat) > -1
        \ || isdirectory(expand("<afile>"))
    return
  endif
  if empty(mapcheck("<Plug>AirlineTablineRefresh", 'n'))
    noremap <silent> <Plug>AirlineTablineRefresh :set mod!<cr>
  endif
  call feedkeys("\<Plug>AirlineTablineRefresh")
  call feedkeys("\<Plug>AirlineTablineRefresh")
  "call feedkeys(',,', 't')
  "call feedkeys(':unmap ,,')
  " force re-evaluation of tabline setting
  " disable explicit redraw, may cause E315
  "redraw
endfunction

function! airline#extensions#tabline#load_theme(palette)
  if pumvisible()
    return
  endif
  let colors    = get(a:palette, 'tabline', {})
  " Theme for tabs on the left
  let l:tab     = get(colors, 'airline_tab', a:palette.normal.airline_b)
  let l:tabsel  = get(colors, 'airline_tabsel', a:palette.normal.airline_a)
  let l:tabtype = get(colors, 'airline_tabtype', a:palette.visual.airline_a)
  let l:tabfill = get(colors, 'airline_tabfill', a:palette.normal.airline_c)
  let l:tabmod  = get(colors, 'airline_tabmod', a:palette.insert.airline_a)
  let l:tabhid  = get(colors, 'airline_tabhid', a:palette.normal.airline_c)
  if has_key(a:palette, 'normal_modified') && has_key(a:palette.normal_modified, 'airline_c')
    let l:tabmodu = get(colors, 'airline_tabmod_unsel', a:palette.normal_modified.airline_c)
  else
    "Fall back to normal airline_c if modified airline_c isn't present
    let l:tabmodu = get(colors, 'airline_tabmod_unsel', a:palette.normal.airline_c)
  endif
  call airline#highlighter#exec('airline_tab', l:tab)
  call airline#highlighter#exec('airline_tabsel', l:tabsel)
  call airline#highlighter#exec('airline_tabtype', l:tabtype)
  call airline#highlighter#exec('airline_tabfill', l:tabfill)
  call airline#highlighter#exec('airline_tabmod', l:tabmod)
  call airline#highlighter#exec('airline_tabmod_unsel', l:tabmodu)
  call airline#highlighter#exec('airline_tabhid', l:tabhid)

  " Theme for tabs on the right
  let l:tabsel_right  = get(colors, 'airline_tabsel_right', a:palette.normal.airline_a)
  let l:tabmod_right  = get(colors, 'airline_tabmod_right', a:palette.insert.airline_a)
  let l:tabhid_right  = get(colors, 'airline_tabhid_right', a:palette.normal.airline_c)
  if has_key(a:palette, 'normal_modified') && has_key(a:palette.normal_modified, 'airline_c')
    let l:tabmodu_right = get(colors, 'airline_tabmod_unsel_right', a:palette.normal_modified.airline_c)
  else
    "Fall back to normal airline_c if modified airline_c isn't present
    let l:tabmodu_right = get(colors, 'airline_tabmod_unsel_right', a:palette.normal.airline_c)
  endif
  call airline#highlighter#exec('airline_tabsel_right', l:tabsel_right)
  call airline#highlighter#exec('airline_tabmod_right', l:tabmod_right)
  call airline#highlighter#exec('airline_tabhid_right', l:tabhid_right)
  call airline#highlighter#exec('airline_tabmod_unsel_right', l:tabmodu_right)
endfunction

let s:current_tabcnt = -1
function! airline#extensions#tabline#get()
  let curtabcnt = tabpagenr('$')
  if curtabcnt != s:current_tabcnt
    let s:current_tabcnt = curtabcnt
    call airline#extensions#tabline#tabs#invalidate()
    call airline#extensions#tabline#buffers#invalidate()
    call airline#extensions#tabline#ctrlspace#invalidate()
  endif

  if !exists('#airline#BufAdd#*')
    autocmd airline BufAdd * call <sid>update_tabline()
  endif
  if s:ctrlspace
    return airline#extensions#tabline#ctrlspace#get()
  elseif s:show_buffers && curtabcnt == 1 || !s:show_tabs
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
