" Author: Andrew Balmos - <andrew@balmos.org>
" Description: chktex for LaTeX files

let g:ale_tex_chktex_executable =
\   get(g:, 'ale_tex_chktex_executable', 'chktex')

let g:ale_tex_chktex_options =
\   get(g:, 'ale_tex_chktex_options', '-I')

function! ale_linters#tex#chktex#GetCommand(buffer) abort
    " Check for optional .chktexrc
    let l:chktex_config = ale#path#FindNearestFile(
    \   a:buffer,
    \   '.chktexrc')

    let l:command = ale#Var(a:buffer, 'tex_chktex_executable')
    " Avoid bug when used without -p (last warning has gibberish for a filename)
    let l:command .= ' -v0 -p stdin -q'

    if !empty(l:chktex_config)
        let l:command .= ' -l ' . ale#Escape(l:chktex_config)
    endif

    let l:command .= ' ' . ale#Var(a:buffer, 'tex_chktex_options')

    return l:command
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
\   'executable': 'chktex',
\   'command_callback': 'ale_linters#tex#chktex#GetCommand',
\   'callback': 'ale_linters#tex#chktex#Handle'
\})
