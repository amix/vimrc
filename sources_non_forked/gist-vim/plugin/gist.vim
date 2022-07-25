"=============================================================================
" File: gist.vim
" Author: Yasuhiro Matsumoto <mattn.jp@gmail.com>
" WebPage: http://github.com/mattn/gist-vim
" License: BSD
" GetLatestVimScripts: 2423 1 :AutoInstall: gist.vim
" script type: plugin

if &compatible || (exists('g:loaded_gist_vim') && g:loaded_gist_vim)
  finish
endif
let g:loaded_gist_vim = 1

function! s:CompleteArgs(arg_lead,cmdline,cursor_pos)
    return filter(copy(["-p", "-P", "-a", "-m", "-e", "-s", "-d", "+1", "-1", "-f", "-c", "-l", "-la", "-ls", "-b",
                \ "--listall", "--liststar", "--list", "--multibuffer", "--private", "--public", "--anonymous", "--description", "--clipboard",
                \ "--rawurl", "--delete", "--edit", "--star", "--unstar", "--fork", "--browser"
                \ ]), 'stridx(v:val, a:arg_lead)==0')
endfunction

command! -nargs=? -range=% -bang -complete=customlist,s:CompleteArgs Gist :call gist#Gist(<count>, "<bang>", <line1>, <line2>, <f-args>)

" vim:set et:
