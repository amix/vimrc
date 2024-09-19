" Description: biome for json files

call ale#linter#Define('json', {
\   'name': 'biome',
\   'lsp': 'stdio',
\   'language': function('ale#handlers#biome#GetLanguage'),
\   'executable': function('ale#handlers#biome#GetExecutable'),
\   'command': '%e lsp-proxy',
\   'project_root': function('ale#handlers#biome#GetProjectRoot'),
\})
