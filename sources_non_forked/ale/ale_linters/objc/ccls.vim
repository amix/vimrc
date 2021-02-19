" Author: Ye Jingchen <ye.jingchen@gmail.com>, Ben Falconer <ben@falconers.me.uk>, jtalowell <jtalowell@protonmail.com>
" Description: A language server for Objective-C

call ale#Set('objc_ccls_executable', 'ccls')
call ale#Set('objc_ccls_init_options', {})
call ale#Set('c_build_dir', '')

call ale#linter#Define('objc', {
\   'name': 'ccls',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'objc_ccls_executable')},
\   'command': '%e',
\   'project_root': function('ale#handlers#ccls#GetProjectRoot'),
\   'initialization_options': {b -> ale#handlers#ccls#GetInitOpts(b, 'objc_ccls_init_options')},
\})
