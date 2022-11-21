function! ale#racket#FindProjectRoot(buffer) abort
    let l:cwd = expand('#' . a:buffer . ':p:h')
    let l:highest_init = l:cwd

    for l:path in ale#path#Upwards(l:cwd)
        if filereadable(l:path.'/init.rkt')
            let l:highest_init = l:path
        endif
    endfor

    return l:highest_init
endfunction
