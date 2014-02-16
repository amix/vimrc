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

if exists("g:loaded_syntastic_java_javac_checker")
    finish
endif
let g:loaded_syntastic_java_javac_checker = 1
let g:syntastic_java_javac_maven_pom_tags = ["build", "properties"]
let g:syntastic_java_javac_maven_pom_properties = {}
let s:has_maven = 0

" Global Options
if !exists("g:syntastic_java_javac_executable")
    let g:syntastic_java_javac_executable = 'javac'
endif

if !exists("g:syntastic_java_maven_executable")
    let g:syntastic_java_maven_executable = 'mvn'
endif

if !exists("g:syntastic_java_javac_options")
    let g:syntastic_java_javac_options = '-Xlint'
endif

if !exists("g:syntastic_java_javac_classpath")
    let g:syntastic_java_javac_classpath = ''
endif

if !exists("g:syntastic_java_javac_delete_output")
    let g:syntastic_java_javac_delete_output = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! s:CygwinPath(path)
    return substitute(system("cygpath -m " . a:path), '\n', '', 'g')
endfunction

if !exists("g:syntastic_java_javac_temp_dir")
    if has('win32') || has('win64')
        let g:syntastic_java_javac_temp_dir = $TEMP."\\vim-syntastic-javac"
    elseif has('win32unix')
        let g:syntastic_java_javac_temp_dir = s:CygwinPath('/tmp/vim-syntastic-javac')
    else
        let g:syntastic_java_javac_temp_dir = '/tmp/vim-syntastic-javac'
    endif
endif

if !exists("g:syntastic_java_javac_autoload_maven_classpath")
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

if !exists("g:syntastic_java_javac_maven_pom_ftime")
    let g:syntastic_java_javac_maven_pom_ftime = {}
endif

if !exists("g:syntastic_java_javac_maven_pom_classpath")
    let g:syntastic_java_javac_maven_pom_classpath = {}
endif

function! s:RemoveCarriageReturn(line)
    return substitute(a:line, '\r', '', 'g')
endfunction

" recursively remove directory and all it's sub-directories
function! s:RemoveDir(dir)
    if isdirectory(a:dir)
        for f in split(globpath(a:dir, '*'), "\n")
            call s:RemoveDir(f)
        endfor
        silent! call system('rmdir ' . a:dir)
    else
        silent! call delete(a:dir)
    endif
endfunction

function! s:AddToClasspath(classpath, path)
    if a:path == ''
        return a:classpath
    endif
    if a:classpath != '' && a:path != ''
        if has('win32') || has('win32unix') || has('win64')
            return a:classpath . ";" . a:path
        else
            return a:classpath . ":" . a:path
        endif
    else
        return a:path
    endif
endfunction

function! s:SplitClasspath(classpath)
    if a:classpath == ''
        return []
    endif
    if has('win32') || has('win32unix') || has('win64')
        return split(a:classpath, ";")
    else
        return split(a:classpath, ":")
    endif
endfunction

function! s:LoadConfigFile()
    if filereadable(g:syntastic_java_javac_config_file)
        exe 'source '.g:syntastic_java_javac_config_file
    endif
endfunction

function! s:SaveClasspath()
    " build classpath from lines
    let path = ''
    let lines = getline(1, line('$'))
    for l in lines
        let path = s:AddToClasspath(path, l)
    endfor
    " save classpath to config file
    if g:syntastic_java_javac_config_file_enabled
        if filereadable(g:syntastic_java_javac_config_file)
            " load lines from config file
            let lines = readfile(g:syntastic_java_javac_config_file)
            " strip g:syntastic_java_javac_classpath options from config file lines
            let i = 0
            while i < len(lines)
                if match(lines[i], 'g:syntastic_java_javac_classpath') != -1
                    call remove(lines, i)
                    let i -= 1
                endif
                let i += 1
            endwhile
        else
            let lines = []
        endif
        " add new g:syntastic_java_javac_classpath option to config
        call add(lines, 'let g:syntastic_java_javac_classpath = "'.path.'"')
        " save config file lines
        call writefile(lines, g:syntastic_java_javac_config_file)
    endif
    " set new classpath
    let g:syntastic_java_javac_classpath = path
    let &modified = 0
endfunction

function! s:EditClasspath()
    let command = 'syntastic javac classpath'
    let winnr = bufwinnr('^' . command . '$')
    if winnr < 0
        let path = []
        let pathlines = split(g:syntastic_java_javac_classpath, "\n")
        for p in pathlines
            let path += s:SplitClasspath(p)
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
endfunction

function! s:SaveConfig()
    " get lines
    let lines = getline(1, line('$'))
    if g:syntastic_java_javac_config_file_enabled
        " save config file lines
        call writefile(lines, g:syntastic_java_javac_config_file)
    endif
    let &modified = 0
endfunction

function! s:EditConfig()
    let command = 'syntastic javac config'
    let winnr = bufwinnr('^' . command . '$')
    if winnr < 0
        let lines = []
        if filereadable(g:syntastic_java_javac_config_file)
            let lines = readfile(g:syntastic_java_javac_config_file)
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
endfunction

function! s:GetMavenProperties()
    let mvn_properties = {}
    let pom = findfile("pom.xml", ".;")
    if s:has_maven && filereadable(pom)
        if !has_key(g:syntastic_java_javac_maven_pom_properties, pom)
            let mvn_cmd = syntastic#util#shexpand(g:syntastic_java_maven_executable) . ' -f ' . pom
            let mvn_is_managed_tag = 1
            let mvn_settings_output = split(system(mvn_cmd . ' help:effective-pom'), "\n")
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
                        let current_path  = substitute(current_path, '\m\.' . matches[1] . "$", '', '')
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
endfunction

command! SyntasticJavacEditClasspath call s:EditClasspath()

if g:syntastic_java_javac_config_file_enabled
    command! SyntasticJavacEditConfig call s:EditConfig()
endif

function! s:GetMavenClasspath()
    let pom = findfile("pom.xml", ".;")
    if s:has_maven && filereadable(pom)
        if !has_key(g:syntastic_java_javac_maven_pom_ftime, pom) || g:syntastic_java_javac_maven_pom_ftime[pom] != getftime(pom)
            let mvn_cmd = syntastic#util#shexpand(g:syntastic_java_maven_executable) . ' -f ' . pom
            let mvn_classpath_output = split(system(mvn_cmd . ' dependency:build-classpath'), "\n")
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

            let output_dir = 'target/classes'
            if has_key(mvn_properties, 'project.build.outputDirectory')
                let output_dir = mvn_properties['project.build.outputDirectory']
            endif
            let mvn_classpath = s:AddToClasspath(mvn_classpath, output_dir)

            let test_output_dir = 'target/test-classes'
            if has_key(mvn_properties, 'project.build.testOutputDirectory')
                let test_output_dir = mvn_properties['project.build.testOutputDirectory']
            endif
            let mvn_classpath = s:AddToClasspath(mvn_classpath, test_output_dir)

            let g:syntastic_java_javac_maven_pom_ftime[pom] = getftime(pom)
            let g:syntastic_java_javac_maven_pom_classpath[pom] = mvn_classpath
        endif
        return g:syntastic_java_javac_maven_pom_classpath[pom]
    endif
    return ''
endfunction

function! SyntaxCheckers_java_javac_IsAvailable() dict
    let s:has_maven = executable(expand(g:syntastic_java_maven_executable))
    return executable(expand(g:syntastic_java_javac_executable))
endfunction

function! s:MavenOutputDirectory()
    let pom = findfile("pom.xml", ".;")
    if s:has_maven && filereadable(pom)
        let mvn_properties = s:GetMavenProperties()
        let output_dir = getcwd()
        if has_key(mvn_properties, 'project.properties.build.dir')
            let output_dir = mvn_properties['project.properties.build.dir']
        endif
        if stridx(expand( '%:p:h' ), "src.main.java") >= 0
            let output_dir .= '/target/classes'
            if has_key(mvn_properties, 'project.build.outputDirectory')
                let output_dir = mvn_properties['project.build.outputDirectory']
            endif
        endif
        if stridx(expand( '%:p:h' ), "src.test.java") >= 0
            let output_dir .= '/target/test-classes'
            if has_key(mvn_properties, 'project.build.testOutputDirectory')
                let output_dir = mvn_properties['project.build.testOutputDirectory']
            endif
        endif

        if has('win32unix')
            let output_dir=s:CygwinPath(output_dir)
        endif
        return output_dir
    endif
    return '.'
endfunction

function! SyntaxCheckers_java_javac_GetLocList() dict

    let javac_opts = g:syntastic_java_javac_options

    let output_dir = ""
    if g:syntastic_java_javac_delete_output
        let output_dir = g:syntastic_java_javac_temp_dir
        let javac_opts .= ' -d ' . output_dir
    endif

    " load classpath from config file
    if g:syntastic_java_javac_config_file_enabled
        call s:LoadConfigFile()
    endif

    let javac_classpath = ''

    " add classpathes to javac_classpath
    if has('win32') || has('win32unix') || has('win64')
        let javac_classpath_split = ';'
    else
        let javac_classpath_split = ':'
    endif
    for path in split(g:syntastic_java_javac_classpath, javac_classpath_split)
        if path != ''
            try
                let ps = glob(path, 0, 1)
            catch
                let ps = split(glob(path, 0), "\n")
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
            let javac_opts .= ' -d ' . s:MavenOutputDirectory()
        endif
        let javac_classpath = s:AddToClasspath(javac_classpath, s:GetMavenClasspath())
    endif

    " load custom classpath
    if g:syntastic_java_javac_custom_classpath_command != ''
        let lines = system(g:syntastic_java_javac_custom_classpath_command)
        if has('win32') || has('win32unix') || has('win64')
            let lines = substitute(lines, "\r\n", "\n", 'g')
        endif
        for l in split(lines, "\n")
            let javac_classpath = s:AddToClasspath(javac_classpath, l)
        endfor
    endif

    if javac_classpath != ''
        let javac_opts .= ' -cp "' . fnameescape(javac_classpath) . '"'
    endif

    " path seperator
    if has('win32') || has('win32unix') || has('win64')
        let sep = "\\"
    else
        let sep = '/'
    endif

    let fname = fnameescape(expand ( '%:p:h' ) . sep . expand ( '%:t' ))

    if has('win32unix')
        let fname =  s:CygwinPath(fname)
    endif

    let makeprg = self.makeprgBuild({
        \ 'args': javac_opts,
        \ 'fname': fname,
        \ 'tail': '2>&1' })

    " unashamedly stolen from *errorformat-javac* (quickfix.txt) and modified to include error types
    let errorformat =
        \ '%E%f:%l:\ error:\ %m,'.
        \ '%W%f:%l:\ warning:\ %m,'.
        \ '%A%f:%l:\ %m,'.
        \ '%+Z%p^,'.
        \ '%+C%.%#,'.
        \ '%-G%.%#'

    if g:syntastic_java_javac_delete_output
        silent! call mkdir(output_dir, 'p')
    endif
    let errors = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['cygwinRemoveCR'] })

    if g:syntastic_java_javac_delete_output
        call s:RemoveDir(output_dir)
    endif
    return errors

endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'java',
    \ 'name': 'javac'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
