"============================================================================
"File:        sphinx.vim
"Description: Syntax checking plugin for Sphinx reStructuredText files
"Maintainer:  Buck Evan <buck dot 2019 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_rst_sphinx_checker")
    finish
endif
let g:loaded_syntastic_rst_sphinx_checker = 1

let s:save_cpo = &cpo
set cpo&vim

let s:sphinx_cache_location = syntastic#util#tmpdir()
lockvar s:sphinx_cache_location

augroup syntastic
    autocmd VimLeave * call syntastic#util#rmrf(s:sphinx_cache_location)
augroup END

function! SyntaxCheckers_rst_sphinx_GetLocList() dict
    let buf = bufnr('')

    let srcdir = syntastic#util#bufVar(buf, 'rst_sphinx_source_dir')
    call self.log('syntastic_rst_sphinx_source_dir =', srcdir)
    if srcdir ==# ''
        let config = syntastic#util#findFileInParent('conf.py',  fnamemodify(bufname(buf), ':p:h'))
        if config ==# '' || !filereadable(config)
            call self.log('conf.py file not found')
            return []
        endif
        let srcdir = fnamemodify(config, ':p:h')
    endif

    let confdir = syntastic#util#bufVar(buf, 'rst_sphinx_config_dir')
    call self.log('syntastic_rst_sphinx_config_dir =', confdir)
    if confdir ==# ''
        let config = syntastic#util#findFileInParent('conf.py',  fnamemodify(bufname(buf), ':p:h'))
        let confdir = (config !=# '' && filereadable(config)) ? fnamemodify(config, ':p:h') : srcdir
    endif

    let makeprg = self.makeprgBuild({
        \ 'args': '-n -E',
        \ 'args_after': '-q -N -b pseudoxml -c ' . syntastic#util#shescape(confdir),
        \ 'fname': syntastic#util#shescape(srcdir),
        \ 'fname_after': syntastic#util#shescape(s:sphinx_cache_location) })

    let errorformat =
        \ '%E%f:%l: SEVER%t: %m,' .
        \ '%f:%l: %tRROR: %m,' .
        \ '%f:%l: %tARNING: %m,' .
        \ '%E%f:: SEVER%t: %m,' .
        \ '%f:: %tRROR: %m,' .
        \ '%f:: %tARNING: %m,' .
        \ '%trror: %m,' .
        \ '%+C%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0] })

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'rst',
    \ 'name': 'sphinx',
    \ 'exec': 'sphinx-build' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
