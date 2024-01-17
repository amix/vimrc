call ale#Set('solidity_forge_executable', 'forge')

function! ale#fixers#forge#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'solidity_forge_executable')

    return {
    \  'command': ale#Escape(l:executable)
    \       . ' fmt %t',
    \   'read_temporary_file': 1,
    \}
endfunction
