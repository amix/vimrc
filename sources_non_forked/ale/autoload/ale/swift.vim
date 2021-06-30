" Author: Dan Loman <https://github.com/namolnad>
" Description: Functions for integrating with Swift tools

" Find the nearest dir containing a Package.swift file and assume it is the root of the Swift project.
function! ale#swift#FindProjectRoot(buffer) abort
    let l:swift_config = ale#path#FindNearestFile(a:buffer, 'Package.swift')

    if !empty(l:swift_config)
        return fnamemodify(l:swift_config, ':h')
    endif

    return ''
endfunction
