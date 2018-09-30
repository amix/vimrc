" Author: Ye Jingchen <ye.jingchen@gmail.com>, Ben Falconer <ben@falconers.me.uk>, jtalowell <jtalowell@protonmail.com>
" Description: A language server for Objective-C

call ale#Set('objc_ccls_executable', 'ccls')
call ale#Set('objc_ccls_init_options', {})

call ale#linter#Define('objc', {
\   'name': 'ccls',
\   'lsp': 'stdio',
\   'executable_callback': ale#VarFunc('objc_ccls_executable'),
\   'command': '%e',
\   'project_root_callback': 'ale#handlers#ccls#GetProjectRoot',
\   'initialization_options_callback': ale#VarFunc('objc_ccls_init_options'),
\})
