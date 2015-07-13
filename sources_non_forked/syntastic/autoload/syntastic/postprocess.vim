if exists('g:loaded_syntastic_postprocess_autoload') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_postprocess_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

" Public functions {{{1

" merge consecutive blanks
function! syntastic#postprocess#compressWhitespace(errors) abort " {{{2
    for e in a:errors
        let e['text'] = substitute(e['text'], "\001", '', 'g')
        let e['text'] = substitute(e['text'], '\n', ' ', 'g')
        let e['text'] = substitute(e['text'], '\m\s\{2,}', ' ', 'g')
        let e['text'] = substitute(e['text'], '\m^\s\+', '', '')
        let e['text'] = substitute(e['text'], '\m\s\+$', '', '')
    endfor

    return a:errors
endfunction " }}}2

" remove spurious CR under Cygwin
function! syntastic#postprocess#cygwinRemoveCR(errors) abort " {{{2
    if has('win32unix')
        for e in a:errors
            let e['text'] = substitute(e['text'], '\r', '', 'g')
        endfor
    endif

    return a:errors
endfunction " }}}2

" decode XML entities
function! syntastic#postprocess#decodeXMLEntities(errors) abort " {{{2
    for e in a:errors
        let e['text'] = syntastic#util#decodeXMLEntities(e['text'])
    endfor

    return a:errors
endfunction " }}}2

" filter out errors referencing other files
function! syntastic#postprocess#filterForeignErrors(errors) abort " {{{2
    return filter(copy(a:errors), 'get(v:val, "bufnr") == ' . bufnr(''))
endfunction " }}}2

" make sure line numbers are not past end of buffers
" XXX: this loads all referenced buffers in memory
function! syntastic#postprocess#guards(errors) abort " {{{2
    let buffers = syntastic#util#unique(map(filter(copy(a:errors), 'v:val["valid"]'), 'str2nr(v:val["bufnr"])'))

    let guards = {}
    for b in buffers
        let guards[b] = len(getbufline(b, 1, '$'))
    endfor

    for e in a:errors
        if e['valid'] && e['lnum'] > guards[e['bufnr']]
            let e['lnum'] = guards[e['bufnr']]
        endif
    endfor

    return a:errors
endfunction " }}}2

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
