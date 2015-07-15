if exists('g:loaded_syntastic_preprocess_autoload') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_preprocess_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

" Public functions {{{1

function! syntastic#preprocess#cabal(errors) abort " {{{2
    let out = []
    let star = 0
    for err in a:errors
        if star
            if err ==# ''
                let star = 0
            else
                let out[-1] .= ' ' . err
            endif
        else
            call add(out, err)
            if err =~# '\m^*\s'
                let star = 1
            endif
        endif
    endfor
    return out
endfunction " }}}2

function! syntastic#preprocess#checkstyle(errors) abort " {{{2
    let out = []
    let fname = expand('%', 1)
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

function! syntastic#preprocess#cppcheck(errors) abort " {{{2
    return map(copy(a:errors), 'substitute(v:val, ''\v^\[[^]]+\]\zs( -\> \[[^]]+\])+\ze:'', "", "")')
endfunction " }}}2

" @vimlint(EVL102, 1, l:true)
" @vimlint(EVL102, 1, l:false)
" @vimlint(EVL102, 1, l:null)
function! syntastic#preprocess#flow(errors) abort " {{{2
    " JSON artifacts
    let true = 1
    let false = 0
    let null = ''

    let idx = 0
    while idx < len(a:errors) && a:errors[idx][0] != '{'
        let idx += 1
    endwhile

    " A hat tip to Marc Weber for this trick
    " http://stackoverflow.com/questions/17751186/iterating-over-a-string-in-vimscript-or-parse-a-json-file/19105763#19105763
    try
        let errs = eval(join(a:errors[idx :], ''))
    catch
        let errs = {}
    endtry

    let out = []
    if type(errs) == type({}) && has_key(errs, 'errors') && type(errs['errors']) == type([])
        for e in errs['errors']
            if type(e) == type({}) && has_key(e, 'message') && type(e['message']) == type([]) && len(e['message'])
                let m = e['message'][0]
                let t = e['message'][1:]

                try
                    let msg =
                        \ m['path'] . ':' .
                        \ m['line'] . ':' .
                        \ m['start'] . ':' .
                        \ (m['line'] ==# m['endline'] && str2nr(m['end']) > 0 ? m['end'] . ':' : '') .
                        \ ' ' . m['descr']

                    if len(t)
                        let msg .= ' ' . join(map(t,
                            \ 'v:val["descr"] . " (" . v:val["path"] . ":" . v:val["line"] . ":" . v:val["start"] . ' .
                            \ '"," . (v:val["line"] !=# v:val["endline"] ? v:val["endline"] . ":" : "") . ' .
                            \ 'v:val["end"] . ")"'))
                    endif

                    let msg = substitute(msg, '\r', '', 'g')
                    let msg = substitute(msg, '\n', ' ', 'g')

                    call add(out, msg)
                catch /\m^Vim\%((\a\+)\)\=:E716/
                    call syntastic#log#warn('checker javascript/flow: unknown error format')
                    let out = []
                    break
                endtry
            else
                call syntastic#log#warn('checker javascript/flow: unknown error format')
                let out = []
                break
            endif
        endfor
    else
        call syntastic#log#warn('checker javascript/flow: unknown error format')
    endif

    return out
endfunction " }}}2
" @vimlint(EVL102, 0, l:true)
" @vimlint(EVL102, 0, l:false)
" @vimlint(EVL102, 0, l:null)

function! syntastic#preprocess#iconv(errors) abort " {{{2
    return
        \ (has('iconv') || has('iconv/dyn')) && &encoding !=# '' && &encoding !=# 'utf-8' ?
        \       map(a:errors, 'iconv(v:val, "utf-8", &encoding)') :
        \       a:errors
endfunction " }}}2

function! syntastic#preprocess#killEmpty(errors) abort " {{{2
    return filter(copy(a:errors), 'v:val !=# ""')
endfunction " }}}2

function! syntastic#preprocess#perl(errors) abort " {{{2
    let out = []

    for e in a:errors
        let parts = matchlist(e, '\v^(.*)\sat\s(.{-})\sline\s(\d+)(.*)$')
        if !empty(parts)
            call add(out, parts[2] . ':' . parts[3] . ':' . parts[1] . parts[4])
        endif
    endfor

    return syntastic#util#unique(out)
endfunction " }}}2

" @vimlint(EVL102, 1, l:true)
" @vimlint(EVL102, 1, l:false)
" @vimlint(EVL102, 1, l:null)
function! syntastic#preprocess#prospector(errors) abort " {{{2
    " JSON artifacts
    let true = 1
    let false = 0
    let null = ''

    " A hat tip to Marc Weber for this trick
    " http://stackoverflow.com/questions/17751186/iterating-over-a-string-in-vimscript-or-parse-a-json-file/19105763#19105763
    try
        let errs = eval(join(a:errors, ''))
    catch
        let errs = {}
    endtry

    let out = []
    if type(errs) == type({}) && has_key(errs, 'messages')
        if type(errs['messages']) == type([])
            for e in errs['messages']
                if type(e) == type({})
                    try
                        if e['source'] ==# 'pylint'
                            let e['location']['character'] += 1
                        endif

                        let msg =
                            \ e['location']['path'] . ':' .
                            \ e['location']['line'] . ':' .
                            \ e['location']['character'] . ': ' .
                            \ e['code'] . ' ' .
                            \ e['message'] . ' ' .
                            \ '[' . e['source'] . ']'

                        call add(out, msg)
                    catch /\m^Vim\%((\a\+)\)\=:E716/
                        call syntastic#log#warn('checker python/prospector: unknown error format')
                        let out = []
                        break
                    endtry
                else
                    call syntastic#log#warn('checker python/prospector: unknown error format')
                    let out = []
                    break
                endif
            endfor
        else
            call syntastic#log#warn('checker python/prospector: unknown error format')
        endif
    endif

    return out
endfunction " }}}2
" @vimlint(EVL102, 0, l:true)
" @vimlint(EVL102, 0, l:false)
" @vimlint(EVL102, 0, l:null)

function! syntastic#preprocess#rparse(errors) abort " {{{2
    let errlist = copy(a:errors)

    " remove uninteresting lines and handle continuations
    let i = 0
    while i < len(errlist)
        if i > 0 && errlist[i][:1] ==# '  ' && errlist[i] !~# '\m\s\+\^$'
            let errlist[i-1] .= errlist[i][1:]
            call remove(errlist, i)
        elseif errlist[i] !~# '\m^\(Lint:\|Lint checking:\|Error in\) '
            call remove(errlist, i)
        else
            let i += 1
        endif
    endwhile

    let out = []
    let fname = ''
    for e in errlist
        if match(e, '\m^Lint: ') == 0
            let parts = matchlist(e, '\m^Lint: \(.*\): found on lines \([0-9, ]\+\)\(+\(\d\+\) more\)\=')
            if len(parts) >= 3
                for line in split(parts[2], '\m,\s*')
                    call add(out, 'E:' . fname . ':' . line . ': ' . parts[1])
                endfor
            endif
            if len(parts) >= 5 && parts[4] !=# ''
                call add(out, 'E:' . fname . ':0: ' . parts[1] . ' - ' . parts[4] . ' messages not shown')
            endif
        elseif match(e, '\m^Lint checking: ') == 0
            let fname = matchstr(e, '\m^Lint checking: \zs.*')
        elseif match(e, '\m^Error in ') == 0
            call add(out, substitute(e, '\m^Error in .\+ : .\+\ze:\d\+:\d\+: ', 'E:' . fname, ''))
        endif
    endfor

    return out
endfunction " }}}2

function! syntastic#preprocess#tslint(errors) abort " {{{2
    return map(copy(a:errors), 'substitute(v:val, ''\m^\(([^)]\+)\)\s\(.\+\)$'', ''\2 \1'', "")')
endfunction " }}}2

function! syntastic#preprocess#validator(errors) abort " {{{2
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

" @vimlint(EVL102, 1, l:true)
" @vimlint(EVL102, 1, l:false)
" @vimlint(EVL102, 1, l:null)
function! syntastic#preprocess#vint(errors) abort " {{{2
    " JSON artifacts
    let true = 1
    let false = 0
    let null = ''

    " A hat tip to Marc Weber for this trick
    " http://stackoverflow.com/questions/17751186/iterating-over-a-string-in-vimscript-or-parse-a-json-file/19105763#19105763
    try
        let errs = eval(join(a:errors, ''))
    catch
        let errs = []
    endtry

    let out = []
    if type(errs) == type([])
        for e in errs
            if type(e) == type({})
                try
                    let msg =
                        \ e['file_path'] . ':' .
                        \ e['line_number'] . ':' .
                        \ e['column_number'] . ':' .
                        \ e['severity'][0] . ': ' .
                        \ e['description'] . ' (' .
                        \ e['policy_name'] . ')'

                    call add(out, msg)
                catch /\m^Vim\%((\a\+)\)\=:E716/
                    call syntastic#log#warn('checker vim/vint: unknown error format')
                    let out = []
                    break
                endtry
            else
                call syntastic#log#warn('checker vim/vint: unknown error format')
                let out = []
                break
            endif
        endfor
    else
        call syntastic#log#warn('checker vim/vint: unknown error format')
    endif

    return out
endfunction " }}}2
" @vimlint(EVL102, 0, l:true)
" @vimlint(EVL102, 0, l:false)
" @vimlint(EVL102, 0, l:null)

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
