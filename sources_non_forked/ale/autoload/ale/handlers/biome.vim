" Author: Akiomi Kamakura <akiomik@gmail.com>
" Description: Functions for working with biome, for fixing files.

call ale#Set('javascript_biome_node_executable', 'node.exe')
call ale#Set('javascript_biome_executable', 'biome')
call ale#Set('javascript_biome_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_biome_options', '')

function! ale#handlers#biome#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'javascript_biome', [
    \   'node_modules/.bin/biome',
    \   'node_modules/@biomejs/biome/bin/biome',
    \])
endfunction
