" Author: ghsang <gwonhyuksang@gmail.com>
" Description: Integration of dotnet format with ALE.

call ale#Set('cs_dotnet_format_executable', 'dotnet')
call ale#Set('cs_dotnet_format_options', '')

function! ale#fixers#dotnet_format#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'cs_dotnet_format_executable')
    let l:options = ale#Var(a:buffer, 'cs_dotnet_format_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' format'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' --folder --include %t "$(dirname %t)"',
    \   'read_temporary_file': 1,
    \}
endfunction
