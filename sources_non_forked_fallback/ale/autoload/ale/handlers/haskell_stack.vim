function! ale#handlers#haskell_stack#EscapeExecutable(executable, stack_exec) abort
    let l:exec_args = a:executable =~? 'stack$'
    \   ? ' exec ' . ale#Escape(a:stack_exec) . ' --'
    \   : ''

    return ale#Escape(a:executable) . l:exec_args
endfunction
