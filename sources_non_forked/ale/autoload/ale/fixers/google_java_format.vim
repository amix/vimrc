" Author: butlerx <butlerx@notthe,cloud>
" Description: Integration of Google-java-format with ALE.

call ale#Set('java_google_java_format_executable', 'google-java-format')
call ale#Set('java_google_java_format_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('java_google_java_format_options', '')

function! ale#fixers#google_java_format#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'java_google_java_format_options')
    let l:executable = ale#Var(a:buffer, 'java_google_java_format_executable')

    if !executable(l:executable)
        return 0
    endif

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' ' . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' --replace'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
