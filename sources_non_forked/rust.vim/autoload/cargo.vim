function! cargo#cmd(args)
    silent! clear
    if !a:args
        execute "!" . "cargo ". a:args
    else
        echom "Missing arguments"
    endif
endfunction

function! cargo#build(args)
    silent! clear
    if !a:args
        execute "!" . "cargo build " . a:args
    else
        execute "!" . "cargo build"
    endif
    silent! clear
    execute "!" . "cargo build"
endfunction

function! cargo#clean(args)
    silent! clear
    if !a:args
        execute "!" . "cargo clean " . a:args
    else
        execute "!" . "cargo clean"
    endif
    silent! clear
    execute "!" . "cargo clean"
endfunction

function! cargo#doc(args)
    silent! clear
    if !a:args
        execute "!" . "cargo doc " . a:args
    else
        execute "!" . "cargo doc"
    endif
endfunction

function! cargo#new(args)
    silent! clear
    if !a:args
        execute "!cargo new " . a:args
        :cd `=a:args`
    else
        echom "Missing arguments"
    endif
endfunction

function! cargo#init(args)
    silent! clear
    if !a:args
        execute "!" . "cargo init " . a:args
    else
        execute "!" . "cargo init"
    endif
endfunction

function! cargo#run(args)
    silent! clear
    if !a:args
        execute "!" . "cargo run " . a:args
    else
        execute "!" . "cargo run"
    endif
endfunction

function! cargo#test(args)
    silent! clear
    if !a:args
        execute "!" . "cargo test " . a:args
    else
        execute "!" . "cargo test"
    endif
endfunction

function! cargo#bench(args)
    silent! clear
    if !a:args
        execute "!" . "cargo bench " . a:args
    else
        execute "!" . "cargo bench"
    endif
endfunction
