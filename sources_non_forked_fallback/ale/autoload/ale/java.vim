" Author: Horacio Sanson https://github.com/hsanson
" Description: Functions for integrating with Java tools

" Find the nearest dir contining a gradle or pom file and assume it
" the root of a java app.
function! ale#java#FindProjectRoot(buffer) abort
    let l:gradle_root = ale#gradle#FindProjectRoot(a:buffer)

    if !empty(l:gradle_root)
        return l:gradle_root
    endif

    let l:maven_pom_file = ale#path#FindNearestFile(a:buffer, 'pom.xml')

    if !empty(l:maven_pom_file)
        return fnamemodify(l:maven_pom_file, ':h')
    endif

    let l:ant_root = ale#ant#FindProjectRoot(a:buffer)

    if !empty(l:ant_root)
        return l:ant_root
    endif

    return ''
endfunction
