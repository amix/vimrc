" Author: meain <abinsimon10@gmail.com>
" Description: Fixing HTML files with tidy.

call ale#Set('html_tidy_executable', 'tidy')
call ale#Set('html_tidy_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#fixers#tidy#Fix(buffer) abort
    let l:executable = ale#node#FindExecutable(
    \   a:buffer,
    \   'html_tidy',
    \   ['tidy'],
    \)

    if !executable(l:executable)
        return 0
    endif

    let l:config = ale#path#FindNearestFile(a:buffer, '.tidyrc')
    let l:config_options = !empty(l:config)
    \   ? ' -q --tidy-mark no --show-errors 0 --show-warnings 0 -config ' . ale#Escape(l:config)
    \   : ' -q --tidy-mark no --show-errors 0 --show-warnings 0'

    return {
    \   'command': ale#Escape(l:executable) . l:config_options,
    \}
endfunction
