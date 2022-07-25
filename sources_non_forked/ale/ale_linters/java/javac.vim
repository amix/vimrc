" Author: farenjihn <farenjihn@gmail.com>, w0rp <devw0rp@gmail.com>
" Description: Lints java files using javac

let s:classpath_sep = has('unix') ? ':' : ';'

call ale#Set('java_javac_executable', 'javac')
call ale#Set('java_javac_options', '')
call ale#Set('java_javac_classpath', '')
call ale#Set('java_javac_sourcepath', '')

function! ale_linters#java#javac#RunWithImportPaths(buffer) abort
    let [l:cwd, l:command] = ale#maven#BuildClasspathCommand(a:buffer)

    " Try to use Gradle if Maven isn't available.
    if empty(l:command)
        let [l:cwd, l:command] = ale#gradle#BuildClasspathCommand(a:buffer)
    endif

    " Try to use Ant if Gradle and Maven aren't available
    if empty(l:command)
        let [l:cwd, l:command] = ale#ant#BuildClasspathCommand(a:buffer)
    endif

    if empty(l:command)
        return ale_linters#java#javac#GetCommand(a:buffer, [], {})
    endif

    return ale#command#Run(
    \   a:buffer,
    \   l:command,
    \   function('ale_linters#java#javac#GetCommand'),
    \   {'cwd': l:cwd},
    \)
endfunction

function! s:BuildClassPathOption(buffer, import_paths) abort
    " Filter out lines like [INFO], etc.
    let l:class_paths = filter(a:import_paths[:], 'v:val !~# ''[''')
    let l:cls_path = ale#Var(a:buffer, 'java_javac_classpath')

    if !empty(l:cls_path) && type(l:cls_path) is v:t_string
        call extend(l:class_paths, split(l:cls_path, s:classpath_sep))
    endif

    if !empty(l:cls_path) && type(l:cls_path) is v:t_list
        call extend(l:class_paths, l:cls_path)
    endif

    return !empty(l:class_paths)
    \   ? '-cp ' . ale#Escape(join(l:class_paths, s:classpath_sep))
    \   : ''
endfunction

function! ale_linters#java#javac#GetCommand(buffer, import_paths, meta) abort
    let l:cp_option = s:BuildClassPathOption(a:buffer, a:import_paths)
    let l:sp_option = ''

    " Find the src directory, for files in this project.
    let l:src_dir = ale#path#FindNearestDirectory(a:buffer, 'src/main/java')
    let l:sp_dirs = []

    if !empty(l:src_dir)
        call add(l:sp_dirs, l:src_dir)

        " Automatically include the jaxb directory too, if it's there.
        let l:jaxb_dir = fnamemodify(l:src_dir, ':h:h')
        \   . (has('win32') ? '\jaxb\' : '/jaxb/')

        if isdirectory(l:jaxb_dir)
            call add(l:sp_dirs, l:jaxb_dir)
        endif
    endif

    " Automatically include the test directory, but only for test code.
    if expand('#' . a:buffer . ':p') =~? '\vsrc[/\\]test[/\\]java'
        let l:test_dir = ale#path#FindNearestDirectory(a:buffer, 'src/test/java')

        if isdirectory(l:test_dir)
            call add(l:sp_dirs, l:test_dir)
        endif
    endif

    let l:source_paths = []
    let l:source_path = ale#Var(a:buffer, 'java_javac_sourcepath')

    if !empty(l:source_path) && type(l:source_path) is v:t_string
        let l:source_paths = split(l:source_path, s:classpath_sep)
    endif

    if !empty(l:source_path) && type(l:source_path) is v:t_list
        let l:source_paths = l:source_path
    endif

    if !empty(l:source_paths)
        for l:path in l:source_paths
            let l:sp_path = ale#path#FindNearestDirectory(a:buffer, l:path)

            if !empty(l:sp_path)
                call add(l:sp_dirs, l:sp_path)
            endif
        endfor
    endif

    if !empty(l:sp_dirs)
        let l:sp_option = '-sourcepath '
        \   . ale#Escape(join(l:sp_dirs, s:classpath_sep))
    endif

    " Create .class files in a temporary directory, which we will delete later.
    let l:class_file_directory = ale#command#CreateDirectory(a:buffer)

    " Always run javac from the directory the file is in, so we can resolve
    " relative paths correctly.
    return '%e -Xlint'
    \ . ale#Pad(l:cp_option)
    \ . ale#Pad(l:sp_option)
    \ . ' -d ' . ale#Escape(l:class_file_directory)
    \ . ale#Pad(ale#Var(a:buffer, 'java_javac_options'))
    \ . ' %t'
endfunction

function! ale_linters#java#javac#Handle(buffer, lines) abort
    " Look for lines like the following.
    "
    " Main.java:13: warning: [deprecation] donaught() in Testclass has been deprecated
    " Main.java:16: error: ';' expected
    let l:directory = expand('#' . a:buffer . ':p:h')
    let l:pattern = '\v^(.*):(\d+): (.{-1,}):(.+)$'
    let l:col_pattern = '\v^(\s*\^)$'
    let l:symbol_pattern = '\v^ +symbol: *(class|method) +([^ ]+)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, [l:pattern, l:col_pattern, l:symbol_pattern])
        if empty(l:match[2]) && empty(l:match[3])
            if !empty(l:match[1]) && !empty(l:output)
                let l:output[-1].col = len(l:match[1])
            endif
        elseif empty(l:match[3])
            " Add symbols to 'cannot find symbol' errors.
            if l:output[-1].text is# 'error: cannot find symbol'
                let l:output[-1].text .= ': ' . l:match[2]
            endif
        else
            call add(l:output, {
            \   'filename': ale#path#GetAbsPath(l:directory, l:match[1]),
            \   'lnum': l:match[2] + 0,
            \   'text': l:match[3] . ':' . l:match[4],
            \   'type': l:match[3] is# 'error' ? 'E' : 'W',
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('java', {
\   'name': 'javac',
\   'executable': {b -> ale#Var(b, 'java_javac_executable')},
\   'cwd': '%s:h',
\   'command': function('ale_linters#java#javac#RunWithImportPaths'),
\   'output_stream': 'stderr',
\   'callback': 'ale_linters#java#javac#Handle',
\})
