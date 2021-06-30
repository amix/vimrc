" Author: Horacio Sanson - https://github.com/hsanson
" Description: Support for bibclean linter for BibTeX files.

call ale#Set('bib_bibclean_executable', 'bibclean')

function! ale_linters#bib#bibclean#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'bib_bibclean_executable')

    return ale#Escape(l:executable) . ' -file-position '
endfunction

function! ale_linters#bib#bibclean#get_type(str) abort
    if a:str is# '??'
        return 'E'
    else
        return 'W'
    endif
endfunction

function! ale_linters#bib#bibclean#match_msg(line) abort
    " Legacy message pattern works for bibclean <= v2.11.4. If empty, try
    " the new message pattern for bibtex > v2.11.4
    let l:matches_legacy = matchlist(a:line, '^\(.*\) "stdin", line \(\d\+\): \(.*\)$')

    return ! empty(l:matches_legacy) ? l:matches_legacy
    \ : matchlist(a:line, '^\(.*\) stdin:\(\d\+\):\(.*\)$')
endfunction

function! ale_linters#bib#bibclean#match_entry(line) abort
    return matchlist(a:line, 'Entry   input byte=.* line=\(.*\) column=\(.*\) output .*$')
endfunction

function! ale_linters#bib#bibclean#match_value(line) abort
    return matchlist(a:line, 'Value   input byte=.* line=\(.*\) column=\(.*\) output .*$')
endfunction

function! ale_linters#bib#bibclean#Handle(buffer, lines) abort
    let l:output = []

    let l:type = 'E'
    let l:msg  = ''

    for l:line in a:lines
        if empty(l:msg)
            let l:mlist = ale_linters#bib#bibclean#match_msg(l:line)

            if !empty(l:mlist)
                let l:msg = l:mlist[3]
                let l:type = ale_linters#bib#bibclean#get_type(l:mlist[1])
            endif
        else
            if l:type is# 'E'
                let l:mlist = ale_linters#bib#bibclean#match_entry(l:line)
            else
                let l:mlist = ale_linters#bib#bibclean#match_value(l:line)
            endif

            if !empty(l:mlist)
                call add(l:output, {
                \ 'lnum': l:mlist[1],
                \ 'col': l:mlist[2],
                \ 'text': l:msg,
                \ 'type': l:type
                \})

                let l:msg = ''
            endif
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('bib', {
\   'name': 'bibclean',
\   'executable': {b -> ale#Var(b, 'bib_bibclean_executable')},
\   'command': function('ale_linters#bib#bibclean#GetCommand'),
\   'output_stream': 'stderr',
\   'callback': 'ale_linters#bib#bibclean#Handle',
\})
