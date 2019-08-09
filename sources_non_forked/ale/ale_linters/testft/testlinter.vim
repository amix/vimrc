" Author: neersighted <bjorn@neersighted.com>
" Description: dummy linter to use in tests

call ale#linter#Define('testft', {
\   'name': 'testlinter',
\   'output_stream': 'stdout',
\   'executable': 'testlinter',
\   'command': 'testlinter',
\   'callback': 'testCB',
\})
