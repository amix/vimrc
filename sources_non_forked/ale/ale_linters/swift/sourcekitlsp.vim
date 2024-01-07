" Author: Dan Loman <https://github.com/namolnad>
" Description: Support for sourcekit-lsp https://github.com/apple/sourcekit-lsp

call ale#Set('sourcekit_lsp_executable', 'sourcekit-lsp')

call ale#linter#Define('swift', {
\   'name': 'sourcekitlsp',
\   'aliases': ['sourcekit'],
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'sourcekit_lsp_executable')},
\   'command': '%e',
\   'project_root': function('ale#swift#FindProjectRoot'),
\   'language': 'swift',
\})
