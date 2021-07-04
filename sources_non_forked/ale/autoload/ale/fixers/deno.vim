function! ale#fixers#deno#Fix(buffer) abort
    let l:executable = ale#handlers#deno#GetExecutable(a:buffer)

    if !executable(l:executable)
        return 0
    endif

    let l:options = ' fmt -'

    if ale#Var(a:buffer, 'deno_unstable')
        let l:options = l:options . ' --unstable'
    endif

    return {
    \   'command': ale#Escape(l:executable) . l:options
    \}
endfunction
