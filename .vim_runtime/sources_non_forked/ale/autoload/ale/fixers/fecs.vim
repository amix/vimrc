" Author: harttle <yangjvn@126.com>
" Description: Apply fecs format to a file.

function! ale#fixers#fecs#Fix(buffer) abort
    let l:executable = ale#handlers#fecs#GetExecutable(a:buffer)

    if !executable(l:executable)
        return 0
    endif

    let l:config_options = ' format --replace=true %t'

    return {
    \   'command': ale#Escape(l:executable) . l:config_options,
    \   'read_temporary_file': 1,
    \}
endfunction
