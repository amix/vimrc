" Author: Derek Sifford <dereksifford@gmail.com>
" Description: Handlers for tsserver

function! ale#handlers#tsserver#GetProjectRoot(buffer) abort
    let l:tsconfig_file = ale#path#FindNearestFile(a:buffer, 'tsconfig.json')

    return !empty(l:tsconfig_file) ? fnamemodify(l:tsconfig_file, ':h') : ''
endfunction
