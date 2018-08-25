" Author: Chaucerbao, w0rp <devw0rp@gmail.com>
" Description: tsserver integration for ALE

call ale#Set('javascript_tsserver_executable', 'tsserver')
call ale#Set('javascript_tsserver_config_path', '')
call ale#Set('javascript_tsserver_use_global', get(g:, 'ale_use_global_executables', 0))

call ale#linter#Define('javascript', {
\   'name': 'tsserver',
\   'lsp': 'tsserver',
\   'executable_callback': ale#node#FindExecutableFunc('javascript_tsserver', [
\       'node_modules/.bin/tsserver',
\   ]),
\   'command': '%e',
\   'project_root_callback': {-> ''},
\   'language': '',
\})
