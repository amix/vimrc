" Author: Jeff Sutherland - https://github.com/jdsutherland
" Description: Report errors in Solidity code with solium

call ale#linter#Define('solidity', {
\   'name': 'solium',
\   'executable': 'solium',
\   'command': 'solium --reporter gcc --file %t',
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\})
