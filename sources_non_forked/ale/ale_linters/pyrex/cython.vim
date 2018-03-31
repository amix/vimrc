" Author: w0rp <devw0rp@gmail.com>
" Description: cython syntax checking for cython files.

call ale#linter#Define('pyrex', {
\   'name': 'cython',
\   'output_stream': 'stderr',
\   'executable': 'cython',
\   'command': 'cython --warning-extra -o ' . g:ale#util#nul_file . ' %t',
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
