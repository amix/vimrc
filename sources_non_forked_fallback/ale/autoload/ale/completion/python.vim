function! ale#completion#python#CompletionItemFilter(buffer, item) abort
    return a:item.label !~# '\v^__[a-z_]+__'
endfunction
