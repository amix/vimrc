"jedi-vim - Omni Completion for python in vim
" Maintainer: David Halter <davidhalter88@gmail.com>
"
" This part of the software is just the vim interface. The really big deal is
" the Jedi Python library.

if !exists("g:jedi#auto_vim_configuration") || g:jedi#auto_vim_configuration
    " jedi-vim doesn't work in compatible mode (vim script syntax problems)
    if &compatible
        set nocompatible
    endif

    " jedi-vim really needs, otherwise jedi-vim cannot start.
    filetype plugin on

    " Change completeopt, but only if it has Vim's default value.
    let s:save_completeopt=&completeopt
    set completeopt&
    let s:default_completeopt=&completeopt
    let &completeopt=s:save_completeopt
    if s:default_completeopt == &completeopt
        set completeopt=menuone,longest,preview
    endif

    if len(mapcheck('<C-c>', 'i')) == 0
        inoremap <C-c> <ESC>
    endif
endif

" Pyimport command
command! -nargs=1 -complete=custom,jedi#py_import_completions Pyimport :call jedi#py_import(<q-args>)

" vim: set et ts=4:
