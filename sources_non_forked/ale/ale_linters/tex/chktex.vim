" Author: Andrew Balmos - <andrew@balmos.org>
" Description: chktex for LaTeX files

call ale#Set('tex_chktex_executable', 'chktex')
call ale#Set('tex_chktex_options', '-I')

function! ale_linters#tex#chktex#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'tex_chktex_executable')
endfunction

function! ale_linters#tex#chktex#GetCommand(buffer, version) abort
    let l:options = ''

    " Avoid bug when used without -p (last warning has gibberish for a filename)
    let l:options .= ' -v0 -p stdin -q'

    " Avoid bug of reporting wrong column when using tabs (issue #723)
    if ale#semver#GTE(a:version, [1, 7, 7])
        let l:options .= ' -S TabSize=1'
    endif

    " Check for optional .chktexrc
    let l:chktex_config = ale#path#FindNearestFile(a:buffer, '.chktexrc')

    if !empty(l:chktex_config)
        let l:options .= ' -l ' . ale#Escape(l:chktex_config)
    endif

    let l:options .= ' ' . ale#Var(a:buffer, 'tex_chktex_options')

    return '%e' . l:options
endfunction

function! ale_linters#tex#chktex#Handle(buffer, lines) abort
    " Mattes lines like:
    "
    " stdin:499:2:24:Delete this space to maintain correct pagereferences.
    " stdin:507:81:3:You should enclose the previous parenthesis with `{}'.
    let l:pattern = '^stdin:\(\d\+\):\(\d\+\):\(\d\+\):\(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[4] . ' (' . (l:match[3]+0) . ')',
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('tex', {
\   'name': 'chktex',
\   'executable': function('ale_linters#tex#chktex#GetExecutable'),
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale_linters#tex#chktex#GetExecutable(buffer),
\       '%e --version',
\       function('ale_linters#tex#chktex#GetCommand'),
\   )},
\   'callback': 'ale_linters#tex#chktex#Handle'
\})
