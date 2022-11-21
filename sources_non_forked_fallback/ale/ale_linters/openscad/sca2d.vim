" Description: SCA2D linter for OpenSCAD files

call ale#Set('openscad_sca2d_executable', 'sca2d')
call ale#Set('openscad_sca2d_options', '')

function! ale_linters#openscad#sca2d#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'openscad_sca2d_executable')
endfunction

function! ale_linters#openscad#sca2d#GetCommand(buffer) abort
    let l:executable = ale_linters#openscad#sca2d#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'openscad_sca2d_options')

    return ale#Escape(l:executable) . ale#Pad(l:options) . ' %s'
endfunction

call ale#linter#Define('openscad', {
\ 'name': 'SCA2D',
\ 'aliases': ['sca2d'],
\ 'executable': function('ale_linters#openscad#sca2d#GetExecutable'),
\ 'command': function('ale_linters#openscad#sca2d#GetCommand'),
\ 'callback': 'ale#handlers#openscad#SCA2D_callback',
\ 'lint_file': 1,
\ })
