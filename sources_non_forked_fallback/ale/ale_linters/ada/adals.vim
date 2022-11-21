" Author: Bartek Jasicki http://github.com/thindil
" Description: Support for Ada Language Server

call ale#Set('ada_adals_executable', 'ada_language_server')
call ale#Set('ada_adals_project', 'default.gpr')
call ale#Set('ada_adals_encoding', 'utf-8')

function! ale_linters#ada#adals#GetAdaLSConfig(buffer) abort
    return {
    \   'ada.projectFile': ale#Var(a:buffer, 'ada_adals_project'),
    \   'ada.defaultCharset': ale#Var(a:buffer, 'ada_adals_encoding')
    \}
endfunction

function! ale_linters#ada#adals#GetRootDirectory(buffer) abort
    return fnamemodify(bufname(a:buffer), ':p:h')
endfunction

call ale#linter#Define('ada', {
\   'name': 'adals',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'ada_adals_executable')},
\   'command': '%e',
\   'project_root': function('ale_linters#ada#adals#GetRootDirectory'),
\   'lsp_config': function('ale_linters#ada#adals#GetAdaLSConfig')
\})
