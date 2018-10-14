" Author: rhysd <https://rhysd.github.io>
" Description: Support for checking LLVM IR with llc

call ale#Set('llvm_llc_executable', 'llc')

function! ale_linters#llvm#llc#HandleErrors(buffer, lines) abort
    " Handle '{path}: {file}:{line}:{col}: error: {message}' format
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+: [^:]+:(\d+):(\d+): (.+)$'

    return map(ale#util#GetMatches(a:lines, l:pattern), "{
    \   'lnum': str2nr(v:val[1]),
    \   'col': str2nr(v:val[2]),
    \   'text': v:val[3],
    \   'type': 'E',
    \}")
endfunction

call ale#linter#Define('llvm', {
\   'name': 'llc',
\   'executable_callback': ale#VarFunc('llvm_llc_executable'),
\   'output_stream': 'stderr',
\   'command_callback': {-> '%e -filetype=null -o=' . g:ale#util#nul_file},
\   'callback': 'ale_linters#llvm#llc#HandleErrors',
\})
