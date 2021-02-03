" Author: Horacio Sanson <https://github.com/hsanson>
" Description: Support for the Eclipse language server https://github.com/eclipse/eclipse.jdt.ls

let s:version_cache = {}

call ale#Set('java_eclipselsp_path', ale#path#Simplify($HOME . '/eclipse.jdt.ls'))
call ale#Set('java_eclipselsp_config_path', '')
call ale#Set('java_eclipselsp_workspace_path', '')
call ale#Set('java_eclipselsp_executable', 'java')
call ale#Set('java_eclipselsp_javaagent', '')

function! ale_linters#java#eclipselsp#Executable(buffer) abort
    return ale#Var(a:buffer, 'java_eclipselsp_executable')
endfunction

function! ale_linters#java#eclipselsp#TargetPath(buffer) abort
    return ale#Var(a:buffer, 'java_eclipselsp_path')
endfunction

function! ale_linters#java#eclipselsp#JarPath(buffer) abort
    let l:path = ale_linters#java#eclipselsp#TargetPath(a:buffer)

    if has('win32')
        let l:platform = 'win32'
    elseif has('macunix')
        let l:platform = 'macosx'
    else
        let l:platform = 'linux'
    endif

    " Search jar file within repository path when manually built using mvn
    let l:files = globpath(l:path, '**/'.l:platform.'/**/plugins/org.eclipse.equinox.launcher_\d\.\d\.\d\d\d\.*\.jar', 1, 1)

    if len(l:files) >= 1
        return l:files[0]
    endif

    " Search jar file within VSCode extensions folder.
    let l:files = globpath(l:path, '**/'.l:platform.'/plugins/org.eclipse.equinox.launcher_\d\.\d\.\d\d\d\.*\.jar', 1, 1)

    if len(l:files) >= 1
        return l:files[0]
    endif

    " Search jar file within unzipped tar.gz file
    let l:files = globpath(l:path, 'plugins/org.eclipse.equinox.launcher_\d\.\d\.\d\d\d\.*\.jar', 1, 1)

    if len(l:files) >= 1
        return l:files[0]
    endif

    " Search jar file within system package path
    let l:files = globpath('/usr/share/java/jdtls/plugins', 'org.eclipse.equinox.launcher_\d\.\d\.\d\d\d\.*\.jar', 1, 1)

    if len(l:files) >= 1
        return l:files[0]
    endif

    return ''
endfunction

function! ale_linters#java#eclipselsp#ConfigurationPath(buffer) abort
    let l:path = fnamemodify(ale_linters#java#eclipselsp#JarPath(a:buffer), ':p:h:h')
    let l:config_path = ale#Var(a:buffer, 'java_eclipselsp_config_path')

    if !empty(l:config_path)
        return ale#path#Simplify(l:config_path)
    endif

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

function! ale_linters#java#eclipselsp#WorkspacePath(buffer) abort
    let l:wspath = ale#Var(a:buffer, 'java_eclipselsp_workspace_path')

    if !empty(l:wspath)
        return l:wspath
    endif

    return ale#path#Dirname(ale#java#FindProjectRoot(a:buffer))
endfunction

function! ale_linters#java#eclipselsp#Javaagent(buffer) abort
    let l:rets = []
    let l:raw = ale#Var(a:buffer, 'java_eclipselsp_javaagent')

    if empty(l:raw)
        return ''
    endif

    let l:jars = split(l:raw)

    for l:jar in l:jars
        call add(l:rets, ale#Escape('-javaagent:' . l:jar))
    endfor

    return join(l:rets, ' ')
endfunction

function! ale_linters#java#eclipselsp#Command(buffer, version) abort
    let l:path = ale#Var(a:buffer, 'java_eclipselsp_path')

    let l:executable = ale_linters#java#eclipselsp#Executable(a:buffer)

    let l:cmd = [ ale#Escape(l:executable),
    \ ale_linters#java#eclipselsp#Javaagent(a:buffer),
    \ '-Declipse.application=org.eclipse.jdt.ls.core.id1',
    \ '-Dosgi.bundles.defaultStartLevel=4',
    \ '-Declipse.product=org.eclipse.jdt.ls.core.product',
    \ '-Dlog.level=ALL',
    \ '-noverify',
    \ '-Xmx1G',
    \ '-jar',
    \ ale#Escape(ale_linters#java#eclipselsp#JarPath(a:buffer)),
    \ '-configuration',
    \ ale#Escape(ale_linters#java#eclipselsp#ConfigurationPath(a:buffer)),
    \ '-data',
    \ ale#Escape(ale_linters#java#eclipselsp#WorkspacePath(a:buffer))
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
    \ function('ale_linters#java#eclipselsp#CommandWithVersion'),
    \ { 'output_stream': 'both' }
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
