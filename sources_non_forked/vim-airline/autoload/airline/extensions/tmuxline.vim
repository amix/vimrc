" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !exists(':Tmuxline')
  finish
endif

let s:tmuxline_snapshot_file = get(g:, 'airline#extensions#tmuxline#snapshot_file', '')
let s:color_template = get(g:, 'airline#extensions#tmuxline#color_template', 'normal')

function! airline#extensions#tmuxline#init(ext)
  call a:ext.add_theme_func('airline#extensions#tmuxline#set_tmux_colors')
endfunction

function! airline#extensions#tmuxline#set_tmux_colors(palette)
  let color_template = has_key(a:palette, s:color_template) ? s:color_template : 'normal'
  let mode_palette = a:palette[color_template]

  let tmuxline_theme = tmuxline#api#create_theme_from_airline(mode_palette)
  call tmuxline#api#set_theme(tmuxline_theme)

  if strlen(s:tmuxline_snapshot_file)
    call tmuxline#api#snapshot(s:tmuxline_snapshot_file)
  endif
endfunction

