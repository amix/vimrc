call ale#Set('dprint_executable', 'dprint')
call ale#Set('dprint_executable_override', 0)
call ale#Set('dprint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('dprint_options', '')
call ale#Set('dprint_config', 'dprint.json')

function! ale#fixers#dprint#Fix(buffer) abort
    let l:executable = ale#path#FindExecutable(a:buffer, 'dprint', ['dprint'])
    let l:executable_override = ale#Var(a:buffer, 'dprint_executable_override')

    if !executable(l:executable) && !l:executable_override
        return 0
    endif

    let l:options = ale#Var(a:buffer, 'dprint_options')
    let l:config = ale#path#FindNearestFile(a:buffer, ale#Var(a:buffer, 'dprint_config'))

    if !empty(l:config)
        let l:options = l:options . ' -c ' . ale#Escape(l:config)
    endif

    let l:options = l:options . ' --stdin %s'

    return {
    \   'command': ale#Escape(l:executable)
    \     . ' fmt '
    \     . l:options
    \}
endfunction
