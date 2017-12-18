" ============================================================================
" File:        tagbar.vim
" Description: List the current file's tags in a sidebar, ordered by class etc
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     2.7
" Note:        This plugin was heavily inspired by the 'Taglist' plugin by
"              Yegappan Lakshmanan and uses a small amount of code from it.
"
" Original taglist copyright notice:
"              Permission is hereby granted to use and distribute this code,
"              with or without modifications, provided that this copyright
"              notice is copied with it. Like anything else that's free,
"              taglist.vim is provided *as is* and comes with no warranty of
"              any kind, either expressed or implied. In no event will the
"              copyright holder be liable for any damamges resulting from the
"              use of this software.
" ============================================================================

scriptencoding utf-8

if &cp || exists('g:loaded_tagbar')
    finish
endif

" Basic init {{{1

if v:version < 700
    echohl WarningMsg
    echomsg 'Tagbar: Vim version is too old, Tagbar requires at least 7.0'
    echohl None
    finish
endif

if v:version == 700 && !has('patch167')
    echohl WarningMsg
    echomsg 'Tagbar: Vim versions lower than 7.0.167 have a bug'
          \ 'that prevents this version of Tagbar from working.'
          \ 'Please use the alternate version posted on the website.'
    echohl None
    finish
endif

function! s:init_var(var, value) abort
    if !exists('g:tagbar_' . a:var)
        execute 'let g:tagbar_' . a:var . ' = ' . string(a:value)
    endif
endfunction

function! s:setup_options() abort
    if !exists('g:tagbar_vertical') || g:tagbar_vertical == 0
        let previewwin_pos = 'topleft'
    else
        let previewwin_pos = 'rightbelow vertical'
    endif
    let options = [
        \ ['autoclose', 0],
        \ ['autofocus', 0],
        \ ['autopreview', 0],
        \ ['autoshowtag', 0],
        \ ['case_insensitive', 0],
        \ ['compact', 0],
        \ ['expand', 0],
        \ ['foldlevel', 99],
        \ ['hide_nonpublic', 0],
        \ ['indent', 2],
        \ ['left', 0],
        \ ['previewwin_pos', previewwin_pos],
        \ ['show_visibility', 1],
        \ ['show_linenumbers', 0],
        \ ['singleclick', 0],
        \ ['sort', 1],
        \ ['systemenc', &encoding],
        \ ['vertical', 0],
        \ ['width', 40],
        \ ['zoomwidth', 1],
        \ ['silent', 0],
    \ ]

    for [opt, val] in options
        call s:init_var(opt, val)
    endfor
endfunction
call s:setup_options()

if !exists('g:tagbar_iconchars')
    if has('multi_byte') && has('unix') && &encoding == 'utf-8' &&
     \ (empty(&termencoding) || &termencoding == 'utf-8')
        let g:tagbar_iconchars = ['▶', '▼']
    else
        let g:tagbar_iconchars = ['+', '-']
    endif
endif

function! s:setup_keymaps() abort
    let keymaps = [
        \ ['jump',          '<CR>'],
        \ ['preview',       'p'],
        \ ['previewwin',    'P'],
        \ ['nexttag',       '<C-N>'],
        \ ['prevtag',       '<C-P>'],
        \ ['showproto',     '<Space>'],
        \ ['hidenonpublic', 'v'],
        \
        \ ['openfold',      ['+', '<kPlus>', 'zo']],
        \ ['closefold',     ['-', '<kMinus>', 'zc']],
        \ ['togglefold',    ['o', 'za']],
        \ ['openallfolds',  ['*', '<kMultiply>', 'zR']],
        \ ['closeallfolds', ['=', 'zM']],
        \ ['incrementfolds',  ['zr']],
        \ ['decrementfolds',  ['zm']],
        \ ['nextfold',      'zj'],
        \ ['prevfold',      'zk'],
        \
        \ ['togglesort',            's'],
        \ ['togglecaseinsensitive', 'i'],
        \ ['toggleautoclose',       'c'],
        \ ['zoomwin',               'x'],
        \ ['close',                 'q'],
        \ ['help',                  ['<F1>', '?']],
    \ ]

    for [map, key] in keymaps
        call s:init_var('map_' . map, key)
        unlet key
    endfor
endfunction
call s:setup_keymaps()

augroup TagbarSession
    autocmd!
    autocmd SessionLoadPost * nested call tagbar#RestoreSession()
augroup END

" Commands {{{1
command! -nargs=0 Tagbar              call tagbar#ToggleWindow()
command! -nargs=0 TagbarToggle        call tagbar#ToggleWindow()
command! -nargs=? TagbarOpen          call tagbar#OpenWindow(<f-args>)
command! -nargs=0 TagbarOpenAutoClose call tagbar#OpenWindow('fcj')
command! -nargs=0 TagbarClose         call tagbar#CloseWindow()
command! -nargs=1 -bang TagbarSetFoldlevel  call tagbar#SetFoldLevel(<args>, <bang>0)
command! -nargs=0 TagbarShowTag       call tagbar#highlighttag(1, 1)
command! -nargs=? TagbarCurrentTag    echo tagbar#currenttag('%s', 'No current tag', <f-args>)
command! -nargs=1 TagbarGetTypeConfig call tagbar#gettypeconfig(<f-args>)
command! -nargs=? TagbarDebug         call tagbar#debug#start_debug(<f-args>)
command! -nargs=0 TagbarDebugEnd      call tagbar#debug#stop_debug()
command! -nargs=0 TagbarTogglePause   call tagbar#toggle_pause()

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
