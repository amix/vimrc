" Author: James Kim <jhlink@users.noreply.github.com>
" Description: Fix C/C++ files with astyle.

function! s:set_variables() abort
    for l:ft in ['c', 'cpp']
        call ale#Set(l:ft . '_astyle_executable', 'astyle')
        call ale#Set(l:ft . '_astyle_project_options', '')
    endfor
endfunction

call s:set_variables()


function! ale#fixers#astyle#Var(buffer, name) abort
    let l:ft = getbufvar(str2nr(a:buffer), '&filetype')
    let l:ft = l:ft =~# 'cpp' ? 'cpp' : 'c'

    return ale#Var(a:buffer, l:ft . '_astyle_' . a:name)
endfunction

" Try to find a project options file.
function! ale#fixers#astyle#FindProjectOptions(buffer) abort
    let l:proj_options = ale#fixers#astyle#Var(a:buffer, 'project_options')

    " If user has set project options variable then use it and skip any searching.
    " This would allow users to use project files named differently than .astylerc.
    if !empty(l:proj_options)
        return l:proj_options
    endif

    " Try to find nearest .astylerc file.
    let l:proj_options = fnamemodify(ale#path#FindNearestFile(a:buffer, '.astylerc'), ':t')

    if !empty(l:proj_options)
        return l:proj_options
    endif

    " Try to find nearest _astylerc file.
    let l:proj_options = fnamemodify(ale#path#FindNearestFile(a:buffer, '_astylerc'), ':t')

    if !empty(l:proj_options)
        return l:proj_options
    endif

    " If no project options file is found return an empty string.
    return ''
endfunction

function! ale#fixers#astyle#Fix(buffer) abort
    let l:executable = ale#fixers#astyle#Var(a:buffer, 'executable')
    let l:proj_options = ale#fixers#astyle#FindProjectOptions(a:buffer)
    let l:command = ' --stdin=' . ale#Escape(expand('#' . a:buffer))

    return {
    \   'command': ale#Escape(l:executable)
    \     . (empty(l:proj_options) ? '' : ' --project=' . l:proj_options)
    \     . l:command
    \}
endfunction
