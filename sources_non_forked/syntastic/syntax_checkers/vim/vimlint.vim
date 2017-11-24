"============================================================================
"File:        vimlint.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_vim_vimlint_checker')
    finish
endif
let g:loaded_syntastic_vim_vimlint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_vim_vimlint_GetHighlightRegex(item) " {{{1
    let term = matchstr(a:item['text'], '\m `\zs[^`]\+\ze`')
    if term !=# ''
        let col = get(a:item, 'col', 0)

        if col && term[0:1] ==# 'l:'
            if getline(a:item.lnum)[col-1:col] !=# 'l:'
                let term = term[2:]
            endif
        endif

        return col ? '\%>' . (col - 1) . 'c\%<' . (col + strlen(term)) . 'c' : '\V' . escape(term, '\')
    endif

    return ''
endfunction " }}}1

function! SyntaxCheckers_vim_vimlint_IsAvailable() dict " {{{1
    try
        " Vim 7.2-051 and later
        let vimlparser = globpath(&runtimepath, 'autoload/vimlparser.vim', 1)
        let vimlint    = globpath(&runtimepath, 'autoload/vimlint.vim', 1)
    catch /\m^Vim\%((\a\+)\)\=:E118/
        let vimlparser = globpath(&runtimepath, 'autoload/vimlparser.vim')
        let vimlint    = globpath(&runtimepath, 'autoload/vimlint.vim')
    endtry
    call self.log("globpath(&runtimepath, 'autoload/vimlparser.vim', 1) = " . string(vimlparser) . ', ' .
                \ "globpath(&runtimepath, 'autoload/vimlint.vim', 1) = " .    string(vimlint))
    return vimlparser !=# '' && vimlint !=# ''
endfunction " }}}1

function! SyntaxCheckers_vim_vimlint_GetLocList() dict " {{{1
    let buf = bufnr('')

    " EVL102: unused variable v
    " EVL103: unused argument v
    " EVL104: variable may not be initialized on some execution path: v
    " EVL105: global variable v is defined without g:
    " EVL106: local variable v is used without l:
    " EVL201: unreachable code
    " EVL204: constant in conditional context
    " EVL205: missing scriptencoding
    " value 3: the message is a warning
    "
    " References: :help vimlint-errorcode and :help vimlint-variables
    let param = {
        \ 'output': function('s:vimlintOutput'),
        \ 'quiet':  1,
        \ 'EVL102': 3,
        \ 'EVL103': 3,
        \ 'EVL104': 3,
        \ 'EVL105': 3,
        \ 'EVL106': 3,
        \ 'EVL201': 3,
        \ 'EVL204': 3,
        \ 'EVL205': 3 }

    let opts = syntastic#util#bufVar(buf, 'vimlint_options')
    if type(opts) == type({})
        let options = filter(copy(opts), 'v:key =~# "\\m^EVL"')
        call extend(param, options, 'force')
    endif

    call self.log('options =', param)

    return vimlint#vimlint(bufname(buf), param)
endfunction " }}}1

" Utilities {{{1

" @vimlint(EVL103, 1, a:filename)
function! s:vimlintOutput(filename, pos, ev, eid, mes, obj) " {{{2
    call add(a:obj.error, {
        \ 'bufnr': bufnr(''),
        \ 'lnum': a:pos.lnum,
        \ 'col': a:pos.col,
        \ 'vcol': 0,
        \ 'type': a:ev[0],
        \ 'text': '[' . a:eid . '] ' . a:mes,
        \ 'valid': a:pos.lnum > 0 })
endfunction " }}}2
" @vimlint(EVL103, 0, a:filename)

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vim',
    \ 'name': 'vimlint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
