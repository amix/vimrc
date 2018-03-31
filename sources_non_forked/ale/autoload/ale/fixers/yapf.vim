" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing Python files with yapf.

call ale#Set('python_yapf_executable', 'yapf')
call ale#Set('python_yapf_use_global', 0)

function! ale#fixers#yapf#Fix(buffer) abort
    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'python_yapf',
    \   ['yapf'],
    \)

    if !executable(l:executable)
        return 0
    endif

    let l:config = ale#path#FindNearestFile(a:buffer, '.style.yapf')
    let l:config_options = !empty(l:config)
    \   ? ' --no-local-style --style ' . ale#Escape(l:config)
    \   : ''

    return {
    \   'command': ale#Escape(l:executable) . l:config_options,
    \}
endfunction
