" vint: -ProhibitAutocmdWithNoGroup

autocmd BufRead,BufNewFile *.rs call s:set_rust_filetype()

if has('patch-8.0.613')
    autocmd BufRead,BufNewFile Cargo.toml setf FALLBACK cfg
endif

function! s:set_rust_filetype() abort
    if &filetype !=# 'rust'
        set filetype=rust
    endif
endfunction

" vim: set et sw=4 sts=4 ts=8:
