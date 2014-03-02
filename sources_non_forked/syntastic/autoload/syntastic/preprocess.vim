if exists("g:loaded_syntastic_preprocess_autoload") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_preprocess_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

" Public functions {{{1

function! syntastic#preprocess#checkstyle(errors) " {{{2
    let out = []
    let fname = expand('%')
    for err in a:errors
        if match(err, '\m<error\>') > -1
            let line = str2nr(matchstr(err, '\m\<line="\zs\d\+\ze"'))
            if line == 0
                continue
            endif

            let col = str2nr(matchstr(err, '\m\<column="\zs\d\+\ze"'))

            let type = matchstr(err, '\m\<severity="\zs.\ze')
            if type !~? '^[EW]'
                let type = 'E'
            endif

            let message = syntastic#util#decodeXMLEntities(matchstr(err, '\m\<message="\zs[^"]\+\ze"'))

            call add(out, join([fname, type, line, col, message], ':'))
        elseif match(err, '\m<file name="') > -1
            let fname = syntastic#util#decodeXMLEntities(matchstr(err, '\v\<file name\="\zs[^"]+\ze"'))
        endif
    endfor
    return out
endfunction " }}}2

function! syntastic#preprocess#cppcheck(errors) " {{{2
    return map(copy(a:errors), 'substitute(v:val, ''\v^\[[^]]+\]\zs( -\> \[[^]]+\])+\ze:'', "", "")')
endfunction " }}}2

function! syntastic#preprocess#killEmpty(errors) " {{{2
    return filter(copy(a:errors), 'v:val != ""')
endfunction " }}}2

function! syntastic#preprocess#perl(errors) " {{{2
    let out = []

    for e in a:errors
        let parts = matchlist(e, '\v^(.*)\sat\s(.*)\sline\s(\d+)(.*)$')
        if !empty(parts)
            call add(out, parts[2] . ':' . parts[3] . ':' . parts[1] . parts[4])
        endif
    endfor

    return syntastic#util#unique(out)
endfunction " }}}2

function! syntastic#preprocess#validator(errors) " {{{2
    let out = []
    for e in a:errors
        let parts = matchlist(e, '\v^"([^"]+)"(.+)')
        if len(parts) >= 3
            " URL decode, except leave alone any "+"
            let parts[1] = substitute(parts[1], '\m%\(\x\x\)', '\=nr2char("0x".submatch(1))', 'g')
            let parts[1] = substitute(parts[1], '\m\\"', '"', 'g')
            let parts[1] = substitute(parts[1], '\m\\\\', '\\', 'g')
            call add(out, '"' . parts[1] . '"' . parts[2])
        endif
    endfor
    return out
endfunction " }}}2

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
