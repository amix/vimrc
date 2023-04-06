" Author: w0rp <dev@w0rp.com>
" Description: lua-language-server integration (https://github.com/LuaLS/lua-language-server)

call ale#Set('lua_language_server_executable', 'lua-language-server')
call ale#Set('lua_language_server_config', {})

call ale#linter#Define('lua', {
\   'name': 'lua_language_server',
\   'aliases': ['lua-language-server'],
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'lua_language_server_executable')},
\   'command': '%e',
\   'project_root': function('ale#lua#FindProjectRoot'),
\   'lsp_config': {b -> ale#Var(b, 'lua_language_server_config')},
\})
