" Author: Michiel Westerbeek <happylinks@gmail.com>
" Description: Linter for GraphQL Schemas

function! ale_linters#graphql#gqlint#GetCommand(buffer) abort
    return ale#path#BufferCdString(a:buffer)
    \   . 'gqlint'
    \   . ' --reporter=simple %t'
endfunction

call ale#linter#Define('graphql', {
\   'name': 'gqlint',
\   'executable': 'gqlint',
\   'command': function('ale_linters#graphql#gqlint#GetCommand'),
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
