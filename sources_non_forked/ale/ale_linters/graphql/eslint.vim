" Author: Benjie Gillam <code@benjiegillam.com>
" Description: eslint for GraphQL files

call ale#linter#Define('graphql', {
\   'name': 'eslint',
\   'executable_callback': 'ale#handlers#eslint#GetExecutable',
\   'command_callback': 'ale#handlers#eslint#GetCommand',
\   'callback': 'ale#handlers#eslint#Handle',
\})
