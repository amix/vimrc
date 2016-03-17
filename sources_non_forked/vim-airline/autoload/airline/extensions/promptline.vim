" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !exists(':PromptlineSnapshot')
  finish
endif

if !exists('airline#extensions#promptline#snapshot_file') || !len('airline#extensions#promptline#snapshot_file')
  finish
endif

let s:prompt_snapshot_file = get(g:, 'airline#extensions#promptline#snapshot_file', '')
let s:color_template = get(g:, 'airline#extensions#promptline#color_template', 'normal')

function! airline#extensions#promptline#init(ext)
  call a:ext.add_theme_func('airline#extensions#promptline#set_prompt_colors')
endfunction

function! airline#extensions#promptline#set_prompt_colors(palette)
  let color_template = has_key(a:palette, s:color_template) ? s:color_template : 'normal'
  let mode_palette = a:palette[color_template]

  if !has_key(g:, 'promptline_symbols')
    let g:promptline_symbols = {
          \ 'left'           : g:airline_left_sep,
          \ 'right'          : g:airline_right_sep,
          \ 'left_alt'       : g:airline_left_alt_sep,
          \ 'right_alt'      : g:airline_right_alt_sep}
  endif

  let promptline_theme = promptline#api#create_theme_from_airline(mode_palette)
  call promptline#api#create_snapshot_with_theme(s:prompt_snapshot_file, promptline_theme)
endfunction
