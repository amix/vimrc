" Author: Jeffrey Lau - https://github.com/zoonfafer
" Description: Metals Language Server for Scala https://scalameta.org/metals/

call ale#Set('scala_metals_executable', 'metals-vim')
call ale#Set('scala_metals_project_root', '')

function! ale_linters#scala#metals#GetProjectRoot(buffer) abort
    let l:project_root = ale#Var(a:buffer, 'scala_metals_project_root')

    if !empty(l:project_root)
        return l:project_root
    endif

    let l:potential_roots = [
    \   'build.sc',
    \   'build.sbt',
    \   '.bloop',
    \   '.metals',
    \]

    for l:root in l:potential_roots
        let l:project_root = ale#path#ResolveLocalPath(
        \   a:buffer,
        \   l:root,
        \   ''
        \)

        if !empty(l:project_root)
            return fnamemodify(
            \   l:project_root,
            \   ':h',
            \)
        endif
    endfor
endfunction

function! ale_linters#scala#metals#GetCommand(buffer) abort
    return '%e' . ale#Pad('stdio')
endfunction

call ale#linter#Define('scala', {
\   'name': 'metals',
\   'lsp': 'stdio',
\   'language': 'scala',
\   'executable': {b -> ale#Var(b, 'scala_metals_executable')},
\   'command': function('ale_linters#scala#metals#GetCommand'),
\   'project_root': function('ale_linters#scala#metals#GetProjectRoot'),
\})
