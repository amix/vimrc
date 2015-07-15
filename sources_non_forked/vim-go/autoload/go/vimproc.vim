"Check if has vimproc
function! go#vimproc#has_vimproc()
    if !exists('g:go#use_vimproc')
        if go#util#IsWin()
            try
                call vimproc#version()
                let exists_vimproc = 1
            catch
                let exists_vimproc = 0
            endtry
        else
            let exists_vimproc = 0
        endif

        let g:go#use_vimproc = exists_vimproc
    endif

    return g:go#use_vimproc
endfunction

" vim:ts=4:sw=4:et
