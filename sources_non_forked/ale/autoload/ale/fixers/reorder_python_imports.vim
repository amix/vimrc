" Author: jake <me@jake.computer>
" Description: Fixing Python imports with reorder-python-imports.

call ale#Set('python_reorder_python_imports_executable', 'reorder-python-imports')
call ale#Set('python_reorder_python_imports_options', '')
call ale#Set('python_reorder_python_imports_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#fixers#reorder_python_imports#Fix(buffer) abort
    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'python_reorder_python_imports',
    \   ['reorder-python-imports'],
    \)

    if !executable(l:executable)
        return 0
    endif

    let l:options = ale#Var(a:buffer, 'python_reorder_python_imports_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (!empty(l:options) ? ' ' . l:options : '') . ' -',
    \}
endfunction
