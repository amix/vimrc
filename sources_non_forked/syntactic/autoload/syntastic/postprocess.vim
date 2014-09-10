if exists("g:loaded_syntastic_postprocess_autoload") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_postprocess_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

" Public functions {{{1

function! s:compareErrorItems(a, b) " {{{2
    if a:a['bufnr'] != a:b['bufnr']
        " group by files
        return a:a['bufnr'] - a:b['bufnr']
    elseif a:a['lnum'] != a:b['lnum']
        return a:a['lnum'] - a:b['lnum']
    elseif a:a['type'] !=? a:b['type']
        " errors take precedence over warnings
        return a:a['type'] ==? 'e' ? -1 : 1
    else
        return get(a:a, 'col', 0) - get(a:b, 'col', 0)
    endif
endfunction " }}}2

" natural sort
function! syntastic#postprocess#sort(errors) " {{{2
    return sort(copy(a:errors), 's:compareErrorItems')
endfunction " }}}2

" merge consecutive blanks
function! syntastic#postprocess#compressWhitespace(errors) " {{{2
    for e in a:errors
        let e['text'] = substitute(e['text'], "\001", '', 'g')
        let e['text'] = substitute(e['text'], '\n', ' ', 'g')
        let e['text'] = substitute(e['text'], '\m\s\{2,}', ' ', 'g')
    endfor

    return a:errors
endfunction " }}}2

" remove spurious CR under Cygwin
function! syntastic#postprocess#cygwinRemoveCR(errors) " {{{2
    if has('win32unix')
        for e in a:errors
            let e['text'] = substitute(e['text'], '\r', '', 'g')
        endfor
    endif

    return a:errors
endfunction " }}}2

" decode XML entities
function! syntastic#postprocess#decodeXMLEntities(errors) " {{{2
    for e in a:errors
        let e['text'] = syntastic#util#decodeXMLEntities(e['text'])
    endfor

    return a:errors
endfunction " }}}2

" filter out errors referencing other files
function! syntastic#postprocess#filterForeignErrors(errors) " {{{2
    return filter(copy(a:errors), 'get(v:val, "bufnr") == ' . bufnr(''))
endfunction " }}}2

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
