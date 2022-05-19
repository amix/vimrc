call ale#Set('crystal_format_executable', 'crystal')
call ale#Set('crystal_format_options', '')

function! ale#fixers#crystal#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'crystal_format_executable')
    let l:options = ale#Var(a:buffer, 'crystal_format_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' tool format'
    \       . ale#Pad(l:options)
    \       . ' -'
    \}
endfunction
