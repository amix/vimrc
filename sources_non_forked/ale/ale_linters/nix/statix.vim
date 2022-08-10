scriptencoding utf-8
" Author: David Houston <houstdav000>
" Description: statix analysis and suggestions for Nix files

call ale#Set('nix_statix_check_executable', 'statix')
call ale#Set('nix_statix_check_options', '')

function! ale_linters#nix#statix#GetCommand(buffer) abort
    return '%e check -o errfmt --stdin'
    \   . ale#Pad(ale#Var(a:buffer, 'nix_statix_check_options'))
endfunction

call ale#linter#Define('nix', {
\   'name': 'statix',
\   'executable': {b -> ale#Var(b, 'nix_statix_check_executable')},
\   'command': function('ale_linters#nix#statix#GetCommand'),
\   'callback': 'ale#handlers#statix#Handle',
\})
