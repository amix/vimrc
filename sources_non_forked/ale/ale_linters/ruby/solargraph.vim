" Author: Horacio Sanson - https://github.com/hsanson
" Description: Solargraph Language Server https://solargraph.org/
"
" Author: Devon Meunier <devon.meunier@gmail.com>
" Description: updated to use stdio

call ale#Set('ruby_solargraph_executable', 'solargraph')

function! ale_linters#ruby#solargraph#GetCommand(buffer) abort
    return '%e' . ale#Pad('stdio')
endfunction

call ale#linter#Define('ruby', {
\   'name': 'solargraph',
\   'lsp': 'stdio',
\   'language': 'ruby',
\   'executable_callback': ale#VarFunc('ruby_solargraph_executable'),
\   'command_callback': 'ale_linters#ruby#solargraph#GetCommand',
\   'project_root_callback': 'ale#ruby#FindProjectRoot',
\})
