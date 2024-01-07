" Author: ymap - https://github.com/ymap
" Description: Packwerk, a static analyzer used to enforce boundaries and modularize Rails applications.

call ale#Set('ruby_packwerk_executable', 'packwerk')
call ale#Set('ruby_packwerk_options', '')

function! ale_linters#ruby#packwerk#Handle(buffer, lines) abort
    let l:pattern = '\v^[^:]+:(\d+):(\d+)$'
    let l:index = 0
    let l:output = []

    while l:index < len(a:lines) - 1
        let l:cleaned_line = substitute(a:lines[l:index], '\v\e\[[0-9;]*m', '', 'g')
        let l:match = matchlist(l:cleaned_line, l:pattern)

        if len(l:match) > 0
            call add(l:output, {
            \   'lnum': l:match[1] + 0,
            \   'col': l:match[2] + 0,
            \   'text': a:lines[l:index + 1],
            \})
        endif

        let l:index += 1
    endwhile

    return l:output
endfunction

function! ale_linters#ruby#packwerk#GetCommand(buffer) abort
    let l:rails_root = ale#ruby#FindRailsRoot(a:buffer)

    if l:rails_root is? ''
        return ''
    endif

    let l:executable = ale#Var(a:buffer, 'ruby_packwerk_executable')
    let l:sep = has('win32') ? '\' : '/'
    let l:abs_path = expand('#' . a:buffer . ':p')
    let l:rel_path = substitute(l:abs_path, escape(l:rails_root . l:sep, '\'), '', '')

    return ale#ruby#EscapeExecutable(l:executable, 'packwerk')
    \   . ' check'
    \   . ale#Pad(ale#Var(a:buffer, 'ruby_packwerk_options'))
    \   . ' '
    \   . ale#Escape(rel_path)
endfunction

call ale#linter#Define('ruby', {
\   'name': 'packwerk',
\   'executable': {b -> ale#Var(b, 'ruby_packwerk_executable')},
\   'command': function('ale_linters#ruby#packwerk#GetCommand'),
\   'callback': 'ale_linters#ruby#packwerk#Handle',
\   'lint_file': 1,
\})
