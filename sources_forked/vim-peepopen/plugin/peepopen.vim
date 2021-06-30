" plugin/peepopen.vim
" Author:   Geoffrey Grosenbach <boss@topfunky.com>
" License:  MIT License

" Install this file as plugin/peepopen.vim.

" If you prefer Command-T, use this snippet in your .gvimrc:

" if has("gui_macvim")
"   macmenu &File.New\ Tab key=<nop>
"   map <D-t> <Plug>PeepOpen
" end

" ============================================================================

" Exit quickly when:
" - this plugin was already loaded (or disabled)
" - when 'compatible' is set
if &cp || exists("g:peepopen_loaded") && g:peepopen_loaded
  finish
endif
let g:peepopen_loaded = 1
let s:save_cpo = &cpo
set cpo&vim

if !exists('g:peepopen_quit')
  let g:peepopen_quit = 0
endif

function s:LaunchPeepOpenViaVim()
  silent exe "!open -a PeepOpen " . shellescape(getcwd())
  redraw!
endfunction   

function s:QuitPeepOpenViaVim()
  silent exe '!ps ax | grep PeepOpen | grep -v grep | awk "{ print $1 }" | xargs kill -QUIT'
endfunction

command! PeepOpen :call <SID>LaunchPeepOpenViaVim()
command! PeepQuit :call <SID>QuitPeepOpenViaVim()

if has('autocmd') && exists('g:peepopen_quit') && g:peepopen_quit
  au VimLeave * :call <SID>QuitPeepOpenViaVim() 
endif

noremap <unique> <script> <Plug>PeepOpen <SID>Launch
noremap <SID>Launch :call <SID>LaunchPeepOpenViaVim()<CR>

if !hasmapto('<Plug>PeepOpen')
  map! <unique> <silent> <Leader>p <Plug>PeepOpen
endif

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set sw=2 sts=2:
 
