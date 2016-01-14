"============================================================================
"File:        javac.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Jochen Keil <jochen.keil at gmail dot com>
"             Dmitry Geurkov <d.geurkov at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_java_javac_checker')
    finish
endif
let g:loaded_syntastic_java_javac_checker = 1
let g:syntastic_java_javac_maven_pom_tags = ['build', 'properties']
let g:syntastic_java_javac_maven_pom_properties = {}
let s:has_maven = 0

let s:save_cpo = &cpo
set cpo&vim

" Checker options {{{1

if !exists('g:syntastic_java_javac_executable')
    let g:syntastic_java_javac_executable = 'javac'
endif

if !exists('g:syntastic_java_maven_executable')
    let g:syntastic_java_maven_executable = 'mvn'
endif

if !exists('g:syntastic_java_javac_options')
    let g:syntastic_java_javac_options = '-Xlint'
endif

if !exists('g:syntastic_java_maven_options')
    let g:syntastic_java_maven_options = ''
endif

if !exists('g:syntastic_java_javac_classpath')
    let g:syntastic_java_javac_classpath = ''
endif

if !exists('g:syntastic_java_javac_delete_output')
    let g:syntastic_java_javac_delete_output = 1
endif

if !exists('g:syntastic_java_javac_autoload_maven_classpath')
    let g:syntastic_java_javac_autoload_maven_classpath = 1
endif

if !exists('g:syntastic_java_javac_config_file_enabled')
    let g:syntastic_java_javac_config_file_enabled = 0
endif

if !exists('g:syntastic_java_javac_config_file')
    let g:syntastic_java_javac_config_file = '.syntastic_javac_config'
endif

if !exists('g:syntastic_java_javac_custom_classpath_command')
    let g:syntastic_java_javac_custom_classpath_command = ''
endif

if !exists('g:syntastic_java_javac_maven_pom_ftime')
    let g:syntastic_java_javac_maven_pom_ftime = {}
endif

if !exists('g:syntastic_java_javac_maven_pom_classpath')
    let g:syntastic_java_javac_maven_pom_classpath = {}
endif

" }}}1

" Constants {{{1

let s:_FILE_SHORTCUTS = {
        \ '%FILE_PATH%':  '%:p',
        \ '%FILE_NAME%':  '%:t',
        \ '%FILE_DIR%':   '%:p:h',
    \ }
lockvar! s:_FILE_SHORTCUTS

" }}}1

" Commands {{{1

command! SyntasticJavacEditClasspath call s:EditClasspath()
command! SyntasticJavacEditConfig    call s:EditConfig()

" }}}1

function! SyntaxCheckers_java_javac_IsAvailable() dict " {{{1
    let s:has_maven = executable(expand(g:syntastic_java_maven_executable, 1))
    return executable(expand(g:syntastic_java_javac_executable, 1))
endfunction " }}}1

function! SyntaxCheckers_java_javac_GetLocList() dict " {{{1
    let javac_opts = g:syntastic_java_javac_options

    let output_dir = ''
    if g:syntastic_java_javac_delete_output
        let output_dir = syntastic#util#tmpdir()
        let javac_opts .= ' -d ' . syntastic#util#shescape(output_dir)
    endif

    " load classpath from config file
    if g:syntastic_java_javac_config_file_enabled
        call s:LoadConfigFile()
    endif


    " add classpathes to javac_classpath {{{2
    let javac_classpath = ''

    for path in split(g:syntastic_java_javac_classpath, s:ClassSep())
        if path !=# ''
            try
                let ps = glob(path, 1, 1)
            catch
                let ps = split(glob(path, 1), "\n")
            endtry
            if type(ps) == type([])
                for p in ps
                    let javac_classpath = s:AddToClasspath(javac_classpath, p)
                endfor
            else
                let javac_classpath = s:AddToClasspath(javac_classpath, ps)
            endif
        endif
    endfor

    if s:has_maven && g:syntastic_java_javac_autoload_maven_classpath
        if !g:syntastic_java_javac_delete_output
            let javac_opts .= ' -d ' . syntastic#util#shescape(s:MavenOutputDirectory())
        endif
        let javac_classpath = s:AddToClasspath(javac_classpath, s:GetMavenClasspath())
    endif
    " }}}2

    " load custom classpath {{{2
    if g:syntastic_java_javac_custom_classpath_command !=# ''
        " Pre-process the classpath command string a little.
        let classpath_command = g:syntastic_java_javac_custom_classpath_command
        for [key, val] in items(s:_FILE_SHORTCUTS)
            let classpath_command = substitute(classpath_command, '\V' . key, syntastic#util#shexpand(val), 'g')
        endfor
        let lines = syntastic#util#system(classpath_command)
        if syntastic#util#isRunningWindows() || has('win32unix')
            let lines = substitute(lines, "\r\n", "\n", 'g')
        endif
        for l in split(lines, "\n")
            let javac_classpath = s:AddToClasspath(javac_classpath, l)
        endfor
    endif

    if javac_classpath !=# ''
        let javac_opts .= ' -cp ' . syntastic#util#shexpand(javac_classpath)
    endif
    " }}}2

    let fname = expand('%:p:h', 1) . syntastic#util#Slash() . expand ('%:t', 1)

    if has('win32unix')
        let fname = syntastic#util#CygwinPath(fname)
    endif

    let makeprg = self.makeprgBuild({
        \ 'args': javac_opts,
        \ 'fname': syntastic#util#shescape(fname) })

    " unashamedly stolen from *errorformat-javac* (quickfix.txt) and modified to include error types
    let errorformat =
        \ '%E%f:%l: error: %m,'.
        \ '%W%f:%l: warning: %m,'.
        \ '%E%f:%l: %m,'.
        \ '%Z%p^,'.
        \ '%-G%.%#'

    if output_dir !=# ''
        silent! call mkdir(output_dir, 'p')
    endif
    let errors = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['cygwinRemoveCR'] })

    if output_dir !=# ''
        call syntastic#util#rmrf(output_dir)
    endif
    return errors

endfunction " }}}1

" Utilities {{{1

function! s:RemoveCarriageReturn(line) " {{{2
    return substitute(a:line, "\r", '', 'g')
endfunction " }}}2

function! s:ClassSep() " {{{2
    return (syntastic#util#isRunningWindows() || has('win32unix')) ? ';' : ':'
endfunction " }}}2

function! s:AddToClasspath(classpath, path) " {{{2
    if a:path ==# ''
        return a:classpath
    endif
    return (a:classpath !=# '') ? a:classpath . s:ClassSep() . a:path : a:path
endfunction " }}}2

function! s:SplitClasspath(classpath) " {{{2
    return split(a:classpath, s:ClassSep())
endfunction " }}}2

function! s:LoadConfigFile() " {{{2
    if filereadable(expand(g:syntastic_java_javac_config_file, 1))
        execute 'source ' . fnameescape(expand(g:syntastic_java_javac_config_file, 1))
    endif
endfunction " }}}2

function! s:SaveClasspath() " {{{2
    " build classpath from lines
    let path = ''
    let lines = getline(1, line('$'))
    for l in lines
        let path = s:AddToClasspath(path, l)
    endfor
    " save classpath to config file
    if g:syntastic_java_javac_config_file_enabled
        if filereadable(expand(g:syntastic_java_javac_config_file, 1))
            " load lines from config file
            let lines = readfile(expand(g:syntastic_java_javac_config_file, 1))
            " strip g:syntastic_java_javac_classpath options from config file lines
            let i = 0
            while i < len(lines)
                if match(lines[i], 'g:syntastic_java_javac_classpath') != -1
                    call remove(lines, i)
                else
                    let i += 1
                endif
            endwhile
        else
            let lines = []
        endif
        " add new g:syntastic_java_javac_classpath option to config
        call add(lines, 'let g:syntastic_java_javac_classpath = ' . string(path))
        " save config file lines
        call writefile(lines, expand(g:syntastic_java_javac_config_file, 1))
    endif
    " set new classpath
    let g:syntastic_java_javac_classpath = path
    let &modified = 0
endfunction " }}}2

function! s:EditClasspath() " {{{2
    let command = 'syntastic javac classpath'
    let winnr = bufwinnr('^' . command . '$')
    if winnr < 0
        let path = []
        let pathlines = split(g:syntastic_java_javac_classpath, "\n")
        for p in pathlines
            call extend(path, s:SplitClasspath(p))
        endfor
        execute (len(path) + 5) . 'sp ' . fnameescape(command)

        augroup syntastic
            autocmd BufWriteCmd <buffer> call s:SaveClasspath() | bwipeout
        augroup END

        setlocal buftype=acwrite bufhidden=wipe nobuflisted noswapfile nowrap number
        for p in path
            call append(line('$') - 1, p)
        endfor
        let &modified = 0
    else
        execute winnr . 'wincmd w'
    endif
endfunction " }}}2

function! s:SaveConfig() " {{{2
    " get lines
    let lines = getline(1, line('$'))
    if g:syntastic_java_javac_config_file_enabled
        " save config file lines
        call writefile(lines, expand(g:syntastic_java_javac_config_file, 1))
    endif
    let &modified = 0
endfunction " }}}2

function! s:EditConfig() " {{{2
    if !g:syntastic_java_javac_config_file_enabled
        return
    endif

    let command = 'syntastic javac config'
    let winnr = bufwinnr('^' . command . '$')
    if winnr < 0
        let lines = []
        if filereadable(expand(g:syntastic_java_javac_config_file, 1))
            let lines = readfile(expand(g:syntastic_java_javac_config_file, 1))
        endif
        execute (len(lines) + 5) . 'sp ' . fnameescape(command)

        augroup syntastic
            autocmd BufWriteCmd <buffer> call s:SaveConfig() | bwipeout
        augroup END

        setlocal ft=vim buftype=acwrite bufhidden=wipe nobuflisted noswapfile nowrap number
        for l in lines
            call append(line('$') - 1, l)
        endfor
        let &modified = 0
    else
        execute winnr . 'wincmd w'
    endif
endfunction " }}}2

function! s:GetMavenProperties() " {{{2
    let mvn_properties = {}
    let pom = syntastic#util#findFileInParent('pom.xml', expand('%:p:h', 1))
    if s:has_maven && filereadable(pom)
        if !has_key(g:syntastic_java_javac_maven_pom_properties, pom)
            let mvn_cmd = syntastic#util#shexpand(g:syntastic_java_maven_executable) .
                \ ' -f ' . syntastic#util#shescape(pom) .
                \ ' ' . g:syntastic_java_maven_options
            let mvn_is_managed_tag = 1
            let mvn_settings_output = split(syntastic#util#system(mvn_cmd . ' help:effective-pom'), "\n")
            let current_path = 'project'
            for line in mvn_settings_output
                let matches = matchlist(line, '\m^\s*<\([a-zA-Z0-9\-\.]\+\)>\s*$')
                if mvn_is_managed_tag && !empty(matches)
                    let mvn_is_managed_tag = index(g:syntastic_java_javac_maven_pom_tags, matches[1]) >= 0
                    let current_path .= '.' . matches[1]
                else
                    let matches = matchlist(line, '\m^\s*</\([a-zA-Z0-9\-\.]\+\)>\s*$')
                    if !empty(matches)
                        let mvn_is_managed_tag = index(g:syntastic_java_javac_maven_pom_tags, matches[1]) < 0
                        let current_path  = substitute(current_path, '\m\.' . matches[1] . '$', '', '')
                    else
                        let matches = matchlist(line, '\m^\s*<\([a-zA-Z0-9\-\.]\+\)>\(.\+\)</[a-zA-Z0-9\-\.]\+>\s*$')
                        if mvn_is_managed_tag && !empty(matches)
                            let mvn_properties[current_path . '.' . matches[1]] = matches[2]
                        endif
                    endif
                endif
            endfor
            let g:syntastic_java_javac_maven_pom_properties[pom] = mvn_properties
        endif
        return g:syntastic_java_javac_maven_pom_properties[pom]
    endif
    return mvn_properties
endfunction " }}}2

function! s:GetMavenClasspath() " {{{2
    let pom = syntastic#util#findFileInParent('pom.xml', expand('%:p:h', 1))
    if s:has_maven && filereadable(pom)
        if !has_key(g:syntastic_java_javac_maven_pom_ftime, pom) || g:syntastic_java_javac_maven_pom_ftime[pom] != getftime(pom)
            let mvn_cmd = syntastic#util#shexpand(g:syntastic_java_maven_executable) .
                \ ' -f ' . syntastic#util#shescape(pom) .
                \ ' ' . g:syntastic_java_maven_options
            let mvn_classpath_output = split(syntastic#util#system(mvn_cmd . ' dependency:build-classpath -DincludeScope=test'), "\n")
            let mvn_classpath = ''
            let class_path_next = 0

            for line in mvn_classpath_output
                if class_path_next == 1
                    let mvn_classpath = s:RemoveCarriageReturn(line)
                    break
                endif
                if stridx(line, 'Dependencies classpath:') >= 0
                    let class_path_next = 1
                endif
            endfor

            let mvn_properties = s:GetMavenProperties()

            let sep = syntastic#util#Slash()
            let output_dir = get(mvn_properties, 'project.build.outputDirectory', join(['target', 'classes'], sep))
            let mvn_classpath = s:AddToClasspath(mvn_classpath, output_dir)

            let test_output_dir = get(mvn_properties, 'project.build.testOutputDirectory', join(['target', 'test-classes'], sep))
            let mvn_classpath = s:AddToClasspath(mvn_classpath, test_output_dir)

            let g:syntastic_java_javac_maven_pom_ftime[pom] = getftime(pom)
            let g:syntastic_java_javac_maven_pom_classpath[pom] = mvn_classpath
        endif
        return g:syntastic_java_javac_maven_pom_classpath[pom]
    endif
    return ''
endfunction " }}}2

function! s:MavenOutputDirectory() " {{{2
    let pom = syntastic#util#findFileInParent('pom.xml', expand('%:p:h', 1))
    if s:has_maven && filereadable(pom)
        let mvn_properties = s:GetMavenProperties()
        let output_dir = get(mvn_properties, 'project.properties.build.dir', getcwd())

        let sep = syntastic#util#Slash()
        let src_main_dir = get(mvn_properties, 'project.build.sourceDirectory', join(['src', 'main', 'java'], sep))
        let src_test_dir = get(mvn_properties, 'project.build.testsourceDirectory', join(['src', 'test', 'java'], sep))
        if stridx(expand('%:p:h', 1), src_main_dir) >= 0
            let output_dir = get(mvn_properties, 'project.build.outputDirectory', join ([output_dir, 'target', 'classes'], sep))
        endif
        if stridx(expand('%:p:h', 1), src_test_dir) >= 0
            let output_dir = get(mvn_properties, 'project.build.testOutputDirectory', join([output_dir, 'target', 'test-classes'], sep))
        endif

        if has('win32unix')
            let output_dir = syntastic#util#CygwinPath(output_dir)
        endif
        return output_dir
    endif
    return '.'
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'java',
    \ 'name': 'javac'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
