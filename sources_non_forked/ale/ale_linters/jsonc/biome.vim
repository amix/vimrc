" Description: biome for jsonc files

call ale#linter#Define('jsonc', {
\   'name': 'biome',
\   'lsp': 'stdio',
\   'language': function('ale#handlers#biome#GetLanguage'),
\   'executable': function('ale#handlers#biome#GetExecutable'),
\   'command': '%e lsp-proxy',
\   'project_root': function('ale#handlers#biome#GetProjectRoot'),
\})
