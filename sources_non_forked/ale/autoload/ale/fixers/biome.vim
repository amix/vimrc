function! ale#fixers#biome#Fix(buffer) abort
    let l:executable = ale#handlers#biome#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'biome_options')
    let l:apply = ale#Var(a:buffer, 'biome_fixer_apply_unsafe') ? '--apply-unsafe' : '--apply'

    return {
    \   'read_temporary_file': 1,
    \   'command': ale#Escape(l:executable) . ' check ' . l:apply
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' %t'
    \}
endfunction
