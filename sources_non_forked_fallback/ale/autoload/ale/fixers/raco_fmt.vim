" Author: Jeremy Cantrell <jmcantrell@gmail.com>
" Description: Integration of raco fmt with ALE.

call ale#Set('racket_raco_fmt_executable', 'raco')
call ale#Set('racket_raco_fmt_options', '')

function! ale#fixers#raco_fmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'racket_raco_fmt_executable')
    let l:options = ale#Var(a:buffer, 'racket_raco_fmt_options')

    return {
    \   'command': ale#Escape(l:executable) . ' fmt'
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \}
endfunction
