" Author:   Nate Kane <nathanaelkane AT gmail DOT com>
" Homepage: http://github.com/nathanaelkane/vim-indent-guides

" Do not load if vim is too old
if (v:version == 701 && !exists('*matchadd')) || (v:version < 701)
  finish
endif

if exists('g:loaded_indent_guides') || &cp
  finish
endif
let g:loaded_indent_guides = 1
call indent_guides#define_default_highlights()

function! s:IndentGuidesToggle()
  call indent_guides#toggle()
endfunction

function! s:IndentGuidesEnable()
  call indent_guides#enable()
endfunction

function! s:IndentGuidesDisable()
  call indent_guides#disable()
endfunction

" Commands
command! -bar IndentGuidesToggle  call s:IndentGuidesToggle()
command! -bar IndentGuidesEnable  call s:IndentGuidesEnable()
command! -bar IndentGuidesDisable call s:IndentGuidesDisable()

"
" Initializes a given variable to a given value. The variable is only
" initialized if it does not exist prior.
"
function s:InitVariable(var, value)
  if !exists(a:var)
    if type(a:value) == type("")
      exec 'let ' . a:var . ' = ' . "'" . a:value . "'"
    else
      exec 'let ' . a:var . ' = ' .  a:value
    endif
  endif
endfunction

" Fixed global variables
let g:indent_guides_autocmds_enabled         = 0
let g:indent_guides_color_hex_pattern        = '#[0-9A-Fa-f]\{6\}'
let g:indent_guides_color_hex_guibg_pattern  = 'guibg=\zs' . g:indent_guides_color_hex_pattern . '\ze'
let g:indent_guides_color_name_guibg_pattern = "guibg='\\?\\zs[0-9A-Za-z ]\\+\\ze'\\?"

" Configurable global variables
call s:InitVariable('g:indent_guides_indent_levels', 30)
call s:InitVariable('g:indent_guides_auto_colors', 1)
call s:InitVariable('g:indent_guides_color_change_percent', 10) " ie. 10%
call s:InitVariable('g:indent_guides_guide_size', 0)
call s:InitVariable('g:indent_guides_start_level', 1)
call s:InitVariable('g:indent_guides_enable_on_vim_startup', 0)
call s:InitVariable('g:indent_guides_debug', 0)
call s:InitVariable('g:indent_guides_space_guides', 1)
call s:InitVariable('g:indent_guides_tab_guides', 1)
call s:InitVariable('g:indent_guides_soft_pattern', '\s')
call s:InitVariable('g:indent_guides_default_mapping', 1)

if !exists('g:indent_guides_exclude_filetypes')
  let g:indent_guides_exclude_filetypes = ['help']
endif

" Default mapping
if !hasmapto('<Plug>IndentGuidesToggle', 'n') && maparg('<Leader>ig', 'n') == ''
    \ && g:indent_guides_default_mapping != 0
  nmap <silent><unique> <Leader>ig <Plug>IndentGuidesToggle
endif

" Plug mappings
nnoremap <unique><script> <Plug>IndentGuidesToggle  :IndentGuidesToggle<CR>
nnoremap <unique><script> <Plug>IndentGuidesEnable  :IndentGuidesEnable<CR>
nnoremap <unique><script> <Plug>IndentGuidesDisable :IndentGuidesDisable<CR>

" Auto commands
augroup indent_guides
  autocmd!

  if g:indent_guides_enable_on_vim_startup
    autocmd VimEnter * :IndentGuidesEnable
  endif

  autocmd BufEnter,WinEnter,FileType * call indent_guides#process_autocmds()

  " Trigger BufEnter and process modelines.
  autocmd ColorScheme * doautocmd indent_guides BufEnter
augroup END
