" Author: Sven-Hendrik Haase <svenstaro@gmail.com>
" Description: A language server for glsl

call ale#Set('glsl_glslls_executable', 'glslls')
call ale#Set('glsl_glslls_logfile', '')

function! ale_linters#glsl#glslls#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'glsl_glslls_executable')
endfunction

function! ale_linters#glsl#glslls#GetCommand(buffer) abort
    let l:executable = ale_linters#glsl#glslls#GetExecutable(a:buffer)
    let l:logfile = ale#Var(a:buffer, 'glsl_glslls_logfile')
    let l:logfile_args = ''
    if l:logfile isnot# ''
        let l:logfile_args = ' --verbose -l ' . l:logfile
    endif
    return ale#Escape(l:executable) . l:logfile_args . ' --stdin'
endfunction

function! ale_linters#glsl#glslls#GetLanguage(buffer) abort
    return 'glsl'
endfunction

function! ale_linters#glsl#glslls#GetProjectRoot(buffer) abort
    let l:project_root = ale#c#FindProjectRoot(a:buffer)

    return !empty(l:project_root) ? fnamemodify(l:project_root, ':h:h') : ''
endfunction

call ale#linter#Define('glsl', {
\   'name': 'glslls',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#glsl#glslls#GetExecutable',
\   'command_callback': 'ale_linters#glsl#glslls#GetCommand',
\   'language_callback': 'ale_linters#glsl#glslls#GetLanguage',
\   'project_root_callback': 'ale_linters#glsl#glslls#GetProjectRoot',
\})
