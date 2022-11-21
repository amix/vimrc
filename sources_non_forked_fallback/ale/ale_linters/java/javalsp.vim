" Author: Horacio Sanson <https://github.com/hsanson>
" Description: Support for the Java language server https://github.com/georgewfraser/vscode-javac

call ale#Set('java_javalsp_executable', '')
call ale#Set('java_javalsp_config', {})

function! ale_linters#java#javalsp#Executable(buffer) abort
    return ale#Var(a:buffer, 'java_javalsp_executable')
endfunction

function! ale_linters#java#javalsp#Config(buffer) abort
    let l:defaults = { 'java': { 'classPath': [], 'externalDependencies': [] } }
    let l:config = ale#Var(a:buffer, 'java_javalsp_config')

    " Ensure the config dictionary contains both classPath and
    " externalDependencies keys to avoid a NPE crash on Java Language Server.
    call extend(l:config, l:defaults, 'keep')
    call extend(l:config['java'], l:defaults['java'], 'keep')

    return l:config
endfunction

function! ale_linters#java#javalsp#Command(buffer) abort
    let l:executable = ale_linters#java#javalsp#Executable(a:buffer)

    if fnamemodify(l:executable, ':t') is# 'java'
        " For backward compatibility.
        let l:cmd = [
        \ ale#Escape(l:executable),
        \ '--add-exports jdk.compiler/com.sun.tools.javac.api=javacs',
        \ '--add-exports jdk.compiler/com.sun.tools.javac.code=javacs',
        \ '--add-exports jdk.compiler/com.sun.tools.javac.comp=javacs',
        \ '--add-exports jdk.compiler/com.sun.tools.javac.main=javacs',
        \ '--add-exports jdk.compiler/com.sun.tools.javac.tree=javacs',
        \ '--add-exports jdk.compiler/com.sun.tools.javac.model=javacs',
        \ '--add-exports jdk.compiler/com.sun.tools.javac.util=javacs',
        \ '--add-opens jdk.compiler/com.sun.tools.javac.api=javacs',
        \ '-m javacs/org.javacs.Main',
        \]

        return join(l:cmd, ' ')
    else
        return ale#Escape(l:executable)
    endif
endfunction

call ale#linter#Define('java', {
\   'name': 'javalsp',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#java#javalsp#Executable'),
\   'command': function('ale_linters#java#javalsp#Command'),
\   'language': 'java',
\   'project_root': function('ale#java#FindProjectRoot'),
\   'lsp_config': function('ale_linters#java#javalsp#Config')
\})
