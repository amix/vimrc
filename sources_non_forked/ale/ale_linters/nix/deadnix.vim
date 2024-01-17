call ale#Set('nix_deadnix_executable', 'deadnix')
call ale#Set('nix_deadnix_options', '')

function! ale_linters#nix#deadnix#GetCommand(buffer) abort
    return '%e -o json' . ale#Pad(ale#Var(a:buffer, 'nix_deadnix_options')) . ' -- %t'
endfunction

call ale#linter#Define('nix', {
\   'name': 'deadnix',
\   'executable': {b -> ale#Var(b, 'nix_deadnix_executable')},
\   'command': function('ale_linters#nix#deadnix#GetCommand'),
\   'callback': 'ale#handlers#deadnix#Handle',
\})
