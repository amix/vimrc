" Author: Ye Jingchen <ye.jingchen@gmail.com>, Ben Falconer <ben@falconers.me.uk>, jtalowell <jtalowell@protonmail.com>
" Description: A language server for C

call ale#Set('c_ccls_executable', 'ccls')
call ale#Set('c_ccls_init_options', {})
call ale#Set('c_build_dir', '')

call ale#linter#Define('c', {
\   'name': 'ccls',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'c_ccls_executable')},
\   'command': '%e',
\   'project_root': function('ale#handlers#ccls#GetProjectRoot'),
\   'initialization_options': {b -> ale#handlers#ccls#GetInitOpts(b, 'c_ccls_init_options')},
\})
