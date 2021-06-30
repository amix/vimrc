" Authors: Franco Victorio - https://github.com/fvictorio, Henrique Barcelos
" https://github.com/hbarcelos
" Description: Report errors in Solidity code with solhint

call ale#linter#Define('solidity', {
\   'name': 'solhint',
\   'output_stream': 'both',
\   'executable': function('ale#handlers#solhint#GetExecutable'),
\   'cwd': function('ale#handlers#solhint#GetCwd'),
\   'command': function('ale#handlers#solhint#GetCommand'),
\   'callback': 'ale#handlers#solhint#Handle',
\})
