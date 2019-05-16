" Author: Horacio Sanson <https://github.com/hsanson>
" Description: Support for the Eclipse language server https://github.com/eclipse/eclipse.jdt.ls

let s:version_cache = {}

call ale#Set('java_eclipselsp_path', ale#path#Simplify($HOME . '/eclipse.jdt.ls'))
call ale#Set('java_eclipselsp_executable', 'java')

function! ale_linters#java#eclipselsp#Executable(buffer) abort
    return ale#Var(a:buffer, 'java_eclipselsp_executable')
endfunction

function! ale_linters#java#eclipselsp#TargetPath(buffer) abort
    return ale#Var(a:buffer, 'java_eclipselsp_path')
endfunction

function! ale_linters#java#eclipselsp#JarPath(buffer) abort
    let l:path = ale_linters#java#eclipselsp#TargetPath(a:buffer)

    " Search jar file within repository path when manually built using mvn
    let l:repo_path = l:path . '/org.eclipse.jdt.ls.product/target/repository'
    let l:files = globpath(l:repo_path, '**/plugins/org.eclipse.equinox.launcher_\d\.\d\.\d\d\d\.*\.jar', 1, 1)

    if len(l:files) == 1
        return l:files[0]
    endif

    " Search jar file within VSCode extensions folder.
    let l:files = globpath(l:path, '**/plugins/org.eclipse.equinox.launcher_\d\.\d\.\d\d\d\.*\.jar', 1, 1)

    if len(l:files) == 1
        return l:files[0]
    endif

    return ''
endfunction

function! ale_linters#java#eclipselsp#ConfigurationPath(buffer) abort
    let l:path = fnamemodify(ale_linters#java#eclipselsp#JarPath(a:buffer), ':p:h:h')

    if has('win32')
        let l:path = l:path . '/config_win'
    elseif has('macunix')
        let l:path = l:path . '/config_mac'
    else
        let l:path = l:path . '/config_linux'
    endif

    return ale#path#Simplify(l:path)
endfunction

function! ale_linters#java#eclipselsp#VersionCheck(version_lines) abort
    return s:GetVersion('', a:version_lines)
endfunction

function! s:GetVersion(executable, version_lines) abort
    let l:version = []

    for l:line in a:version_lines
        let l:match = matchlist(l:line, '\(\d\+\)\.\(\d\+\)\.\(\d\+\)')

        if !empty(l:match)
            let l:version = [l:match[1] + 0, l:match[2] + 0, l:match[3] + 0]
            let s:version_cache[a:executable] = l:version
            break
        endif
    endfor

    return l:version
endfunction

function! ale_linters#java#eclipselsp#CommandWithVersion(buffer, version_lines, meta) abort
    let l:executable = ale_linters#java#eclipselsp#Executable(a:buffer)
    let l:version = s:GetVersion(l:executable, a:version_lines)

    return ale_linters#java#eclipselsp#Command(a:buffer, l:version)
endfunction

function! ale_linters#java#eclipselsp#Command(buffer, version) abort
    let l:path = ale#Var(a:buffer, 'java_eclipselsp_path')

    let l:executable = ale_linters#java#eclipselsp#Executable(a:buffer)

    let l:cmd = [ ale#Escape(l:executable),
    \ '-Declipse.application=org.eclipse.jdt.ls.core.id1',
    \ '-Dosgi.bundles.defaultStartLevel=4',
    \ '-Declipse.product=org.eclipse.jdt.ls.core.product',
    \ '-Dlog.level=ALL',
    \ '-noverify',
    \ '-Xmx1G',
    \ '-jar',
    \ ale_linters#java#eclipselsp#JarPath(a:buffer),
    \ '-configuration',
    \ ale_linters#java#eclipselsp#ConfigurationPath(a:buffer),
    \ '-data',
    \ ale#java#FindProjectRoot(a:buffer)
    \ ]

    if ale#semver#GTE(a:version, [1, 9])
        call add(l:cmd, '--add-modules=ALL-SYSTEM')
        call add(l:cmd, '--add-opens java.base/java.util=ALL-UNNAMED')
        call add(l:cmd, '--add-opens java.base/java.lang=ALL-UNNAMED')
    endif

    return join(l:cmd, ' ')
endfunction

function! ale_linters#java#eclipselsp#RunWithVersionCheck(buffer) abort
    let l:executable = ale_linters#java#eclipselsp#Executable(a:buffer)

    if empty(l:executable)
        return ''
    endif

    let l:cache = s:version_cache

    if has_key(s:version_cache, l:executable)
        return ale_linters#java#eclipselsp#Command(a:buffer, s:version_cache[l:executable])
    endif

    let l:command = ale#Escape(l:executable) . ' -version'

    return ale#command#Run(
    \ a:buffer,
    \ l:command,
    \ function('ale_linters#java#eclipselsp#CommandWithVersion')
    \)
endfunction

call ale#linter#Define('java', {
\   'name': 'eclipselsp',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#java#eclipselsp#Executable'),
\   'command': function('ale_linters#java#eclipselsp#RunWithVersionCheck'),
\   'language': 'java',
\   'project_root': function('ale#java#FindProjectRoot'),
\})
