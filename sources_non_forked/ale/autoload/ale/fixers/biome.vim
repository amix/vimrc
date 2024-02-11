" Author: Akiomi Kamakura <akiomik@gmail.com>
" Description: Fixing files with biome (ex.rome).

function! ale#fixers#biome#Fix(buffer) abort
    let l:executable = ale#handlers#biome#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'javascript_biome_options')
    let l:node = ale#Var(a:buffer, 'javascript_biome_node_executable')

    return {
    \   'command': (has('win32') ? (ale#Escape(l:node) . ' ') : '')
    \       . ale#Escape(l:executable)
    \       . ' check --apply'
    \       . ale#Pad(l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
