scriptencoding utf-8
" Author: Jesse Hathaway <jesse@mbuki-mvuki.org>
" Description: Fix markdown files with pandoc.

call ale#Set('markdown_pandoc_executable', 'pandoc')
call ale#Set('markdown_pandoc_options', '-f gfm -t gfm -s -')

function! ale#fixers#pandoc#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'markdown_pandoc_executable')
    let l:options = ale#Var(a:buffer, 'markdown_pandoc_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' ' . l:options,
    \}
endfunction
