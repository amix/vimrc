" Author: jeremija <https://github.com/jeremija>
" Description: Support for nimlsp (language server for nim)

call ale#Set('nim_nimlsp_nim_sources', '')

function! ale_linters#nim#nimlsp#GetProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestDirectory(a:buffer, '.git')

    if !empty(l:project_root)
        return fnamemodify(l:project_root, ':h:h')
    endif

    return ''
endfunction

function! ale_linters#nim#nimlsp#GetCommand(buffer) abort
    let l:nim_sources = ale#Var(a:buffer, 'nim_nimlsp_nim_sources')

    if !empty(l:nim_sources)
        let l:nim_sources = ale#Escape(l:nim_sources)
    endif

    return '%e' . ale#Pad(l:nim_sources)
endfunction

call ale#linter#Define('nim', {
\   'name': 'nimlsp',
\   'lsp': 'stdio',
\   'executable': 'nimlsp',
\   'command': function('ale_linters#nim#nimlsp#GetCommand'),
\   'language': 'nim',
\   'project_root': function('ale_linters#nim#nimlsp#GetProjectRoot'),
\})
