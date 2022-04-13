" Author: Bartolomeo Stellato bartolomeo.stellato@gmail.com
" Description: Functions for integrating with Julia tools

" Find the nearest dir containing a julia project
let s:__ale_julia_project_filenames = ['REQUIRE', 'Manifest.toml', 'Project.toml']

function! ale#julia#FindProjectRoot(buffer) abort
    for l:project_filename in s:__ale_julia_project_filenames
        let l:full_path = ale#path#FindNearestFile(a:buffer, l:project_filename)

        if !empty(l:full_path)
            let l:path = fnamemodify(l:full_path, ':p:h')

            return l:path
        endif
    endfor

    return ''
endfunction
