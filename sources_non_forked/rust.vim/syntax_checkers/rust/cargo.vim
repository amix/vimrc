" Vim syntastic plugin
" Language:     Rust
" Maintainer:   Julien Levesy <jlevesy@gmail.com>
"
" See for details on how to add an external Syntastic checker:
" https://github.com/scrooloose/syntastic/wiki/Syntax-Checker-Guide#external

if exists("g:loaded_syntastic_rust_cargo_checker")
    finish
endif

let g:loaded_syntastic_rust_cargo_checker = 1

" Force syntastic to call cargo without a specific file name
let g:syntastic_rust_cargo_fname = ""

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_rust_cargo_IsAvailable() dict
  return executable(self.getExec()) &&
    \ syntastic#util#versionIsAtLeast(self.getVersion(), [0, 16, 0])
endfunction

function! SyntaxCheckers_rust_cargo_GetLocList() dict
    let makeprg = self.makeprgBuild({ "args": "check" })

    " Ignored patterns, and blank lines
    let errorformat  =
        \ '%-G,' .
        \ '%-Gerror: aborting %.%#,' .
        \ '%-Gerror: Could not compile %.%#,'

    " Meaningful lines (errors, notes, warnings, contextual information)
    let errorformat .=
        \ '%Eerror: %m,' .
        \ '%Eerror[E%n]: %m,' .
        \ '%Wwarning: %m,' .
        \ '%Inote: %m,' .
        \ '%C %#--> %f:%l:%c'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'cwd': expand('%:p:h'),
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'rust',
    \ 'name': 'cargo'})

let &cpo = s:save_cpo
unlet s:save_cpo
