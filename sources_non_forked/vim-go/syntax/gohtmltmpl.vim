if exists("b:current_syntax")
    finish
endif

if !exists("main_syntax")
    let main_syntax = 'html'
endif

runtime! syntax/gotexttmpl.vim
runtime! syntax/html.vim
unlet b:current_syntax

let b:current_syntax = "gohtmltmpl"

" vim:ts=4:sw=4:et
