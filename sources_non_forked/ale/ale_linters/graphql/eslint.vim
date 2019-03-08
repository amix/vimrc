" Author: Benjie Gillam <code@benjiegillam.com>
" Description: eslint for GraphQL files

call ale#linter#Define('graphql', {
\   'name': 'eslint',
\   'executable': function('ale#handlers#eslint#GetExecutable'),
\   'command': function('ale#handlers#eslint#GetCommand'),
\   'callback': 'ale#handlers#eslint#Handle',
\})
