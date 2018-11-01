" Author: Johannes Wienke <languitar@semipol.de>
" Description: PMD for Java files

function! ale_linters#java#pmd#Handle(buffer, lines) abort
    let l:pattern = '"\(\d\+\)",".*","\(.\+\)","\(\d\+\)","\(\d\+\)","\(.\+\)","\(.\+\)","\(.\+\)"$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'type': 'W',
        \   'lnum': l:match[4] + 0,
        \   'text': l:match[5],
        \   'code': l:match[6] . ' - ' . l:match[7],
        \})
    endfor

    return l:output
endfunction

function! ale_linters#java#pmd#GetCommand(buffer) abort
    return 'pmd '
    \ . ale#Var(a:buffer, 'java_pmd_options')
    \ . ' -f csv'
    \ . ' -d %t'
endfunction

if !exists('g:ale_java_pmd_options')
    let g:ale_java_pmd_options = '-R category/java/bestpractices.xml'
endif

call ale#linter#Define('java', {
\   'name': 'pmd',
\   'executable': 'pmd',
\   'command_callback': 'ale_linters#java#pmd#GetCommand',
\   'callback': 'ale_linters#java#pmd#Handle',
\})
