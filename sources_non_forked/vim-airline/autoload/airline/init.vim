" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

function! s:check_defined(variable, default)
  if !exists(a:variable)
    let {a:variable} = a:default
  endif
endfunction

let s:loaded = 0
function! airline#init#bootstrap()
  if s:loaded
    return
  endif
  let s:loaded = 1

  let g:airline#init#bootstrapping = 1
  call s:check_defined('g:airline_left_sep', get(g:, 'airline_powerline_fonts', 0)?"\ue0b0":">")
  call s:check_defined('g:airline_left_alt_sep', get(g:, 'airline_powerline_fonts', 0)?"\ue0b1":">")
  call s:check_defined('g:airline_right_sep', get(g:, 'airline_powerline_fonts', 0)?"\ue0b2":"<")
  call s:check_defined('g:airline_right_alt_sep', get(g:, 'airline_powerline_fonts', 0)?"\ue0b3":"<")
  call s:check_defined('g:airline_detect_modified', 1)
  call s:check_defined('g:airline_detect_paste', 1)
  call s:check_defined('g:airline_detect_crypt', 1)
  call s:check_defined('g:airline_detect_iminsert', 0)
  call s:check_defined('g:airline_inactive_collapse', 1)
  call s:check_defined('g:airline_exclude_filenames', ['DebuggerWatch','DebuggerStack','DebuggerStatus'])
  call s:check_defined('g:airline_exclude_filetypes', [])
  call s:check_defined('g:airline_exclude_preview', 0)
  call s:check_defined('g:airline_gui_mode', airline#init#gui_mode())

  call s:check_defined('g:airline_mode_map', {})
  call extend(g:airline_mode_map, {
        \ '__' : '------',
        \ 'n'  : 'NORMAL',
        \ 'i'  : 'INSERT',
        \ 'R'  : 'REPLACE',
        \ 'v'  : 'VISUAL',
        \ 'V'  : 'V-LINE',
        \ 'c'  : 'COMMAND',
        \ '' : 'V-BLOCK',
        \ 's'  : 'SELECT',
        \ 'S'  : 'S-LINE',
        \ '' : 'S-BLOCK',
        \ 't'  : 'TERMINAL',
        \ }, 'keep')

  call s:check_defined('g:airline_theme_map', {})
  call extend(g:airline_theme_map, {
        \ 'Tomorrow.*': 'tomorrow',
        \ 'base16.*': 'base16',
        \ 'mo[l|n]okai': 'molokai',
        \ 'wombat.*': 'wombat',
        \ '.*zenburn.*': 'zenburn',
        \ '.*solarized.*': 'solarized',
        \ }, 'keep')

  call s:check_defined('g:airline_symbols', {})
  call extend(g:airline_symbols, {
        \ 'paste': 'PASTE',
        \ 'readonly': get(g:, 'airline_powerline_fonts', 0) ? "\ue0a2" : 'RO',
        \ 'whitespace': get(g:, 'airline_powerline_fonts', 0) ? "\u2739" : '!',
        \ 'linenr': get(g:, 'airline_powerline_fonts', 0) ? "\ue0a1" : ':',
        \ 'branch': get(g:, 'airline_powerline_fonts', 0) ? "\ue0a0" : '',
        \ 'notexists': "\u2204",
        \ 'modified': '+',
        \ 'space': ' ',
        \ 'crypt': get(g:, 'airline_crypt_symbol', nr2char(0x1F512)),
        \ }, 'keep')

  call airline#parts#define('mode', {
        \ 'function': 'airline#parts#mode',
        \ 'accent': 'bold',
        \ })
  call airline#parts#define_function('iminsert', 'airline#parts#iminsert')
  call airline#parts#define_function('paste', 'airline#parts#paste')
  call airline#parts#define_function('crypt', 'airline#parts#crypt')
  call airline#parts#define_function('filetype', 'airline#parts#filetype')
  call airline#parts#define('readonly', {
        \ 'function': 'airline#parts#readonly',
        \ 'accent': 'red',
        \ })
  call airline#parts#define_raw('file', '%f%m')
  call airline#parts#define_raw('path', '%F%m')
  call airline#parts#define('linenr', {
        \ 'raw': '%{g:airline_symbols.linenr}%#__accent_bold#%4l%#__restore__#',
        \ 'accent': 'bold'})
  call airline#parts#define_function('ffenc', 'airline#parts#ffenc')
  call airline#parts#define_empty(['hunks', 'branch', 'tagbar', 'syntastic',
        \ 'eclim', 'whitespace','windowswap', 'ycm_error_count', 'ycm_warning_count'])
  call airline#parts#define_text('capslock', '')

  unlet g:airline#init#bootstrapping
endfunction

function! airline#init#gui_mode()
  return ((has('nvim') && exists('$NVIM_TUI_ENABLE_TRUE_COLOR'))
        \ || has('gui_running') || (has("termtruecolor") && &guicolors == 1)) ?
        \ 'gui' : 'cterm'
endfunction

function! airline#init#sections()
  let spc = g:airline_symbols.space
  if !exists('g:airline_section_a')
    let g:airline_section_a = airline#section#create_left(['mode', 'crypt', 'paste', 'capslock', 'iminsert'])
  endif
  if !exists('g:airline_section_b')
    let g:airline_section_b = airline#section#create(['hunks', 'branch'])
  endif
  if !exists('g:airline_section_c')
    if exists("+autochdir") && &autochdir == 1
      let g:airline_section_c = airline#section#create(['%<', 'path', spc, 'readonly'])
    else
      let g:airline_section_c = airline#section#create(['%<', 'file', spc, 'readonly'])
    endif
  endif
  if !exists('g:airline_section_gutter')
    let g:airline_section_gutter = airline#section#create(['%='])
  endif
  if !exists('g:airline_section_x')
    let g:airline_section_x = airline#section#create_right(['tagbar', 'filetype'])
  endif
  if !exists('g:airline_section_y')
    let g:airline_section_y = airline#section#create_right(['ffenc'])
  endif
  if !exists('g:airline_section_z')
    let g:airline_section_z = airline#section#create(['windowswap', '%3p%%'.spc, 'linenr', ':%3v '])
  endif
  if !exists('g:airline_section_error')
    let g:airline_section_error = airline#section#create(['ycm_error_count', 'syntastic', 'eclim'])
  endif
  if !exists('g:airline_section_warning')
    let g:airline_section_warning = airline#section#create(['ycm_warning_count', 'whitespace'])
  endif
endfunction

