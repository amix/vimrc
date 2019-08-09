" Author: Horacio Sanson <https://github.com/hsanson>
" Description: Support for the Java language server https://github.com/georgewfraser/vscode-javac

call ale#Set('java_javalsp_executable', 'java')

function! ale_linters#java#javalsp#Executable(buffer) abort
    return ale#Var(a:buffer, 'java_javalsp_executable')
endfunction

function! ale_linters#java#javalsp#Command(buffer) abort
    let l:executable = ale_linters#java#javalsp#Executable(a:buffer)

    return ale#Escape(l:executable) . ' -Xverify:none -m javacs/org.javacs.Main'
endfunction

call ale#linter#Define('java', {
\   'name': 'javalsp',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#java#javalsp#Executable'),
\   'command': function('ale_linters#java#javalsp#Command'),
\   'language': 'java',
\   'project_root': function('ale#java#FindProjectRoot'),
\})
