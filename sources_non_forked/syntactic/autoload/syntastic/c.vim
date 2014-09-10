if exists("g:loaded_syntastic_c_autoload") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_c_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

" Public functions {{{1

" convenience function to determine the 'null device' parameter
" based on the current operating system
function! syntastic#c#NullOutput() " {{{2
    let known_os = has('unix') || has('mac') || syntastic#util#isRunningWindows()
    return known_os ? '-o ' . syntastic#util#DevNull() : ''
endfunction " }}}2

" read additional compiler flags from the given configuration file
" the file format and its parsing mechanism is inspired by clang_complete
function! syntastic#c#ReadConfig(file) " {{{2
    " search in the current file's directory upwards
    let config = findfile(a:file, '.;')
    if config == '' || !filereadable(config)
        return ''
    endif

    " convert filename into absolute path
    let filepath = fnamemodify(config, ':p:h')

    " try to read config file
    try
        let lines = readfile(config)
    catch /\m^Vim\%((\a\+)\)\=:E48[45]/
        return ''
    endtry

    " filter out empty lines and comments
    call filter(lines, 'v:val !~ ''\v^(\s*#|$)''')

    " remove leading and trailing spaces
    call map(lines, 'substitute(v:val, ''\m^\s\+'', "", "")')
    call map(lines, 'substitute(v:val, ''\m\s\+$'', "", "")')

    let parameters = []
    for line in lines
        let matches = matchstr(line, '\m\C^\s*-I\s*\zs.\+')
        if matches != ''
            " this one looks like an absolute path
            if match(matches, '\m^\%(/\|\a:\)') != -1
                call add(parameters, '-I' . matches)
            else
                call add(parameters, '-I' . filepath . syntastic#util#Slash() . matches)
            endif
        else
            call add(parameters, line)
        endif
    endfor

    return join(map(parameters, 'syntastic#util#shescape(v:val)'))
endfunction " }}}2

" GetLocList() for C-like compilers
function! syntastic#c#GetLocList(filetype, subchecker, options) " {{{2
    try
        let flags = s:getCflags(a:filetype, a:subchecker, a:options)
    catch /\m\C^Syntastic: skip checks$/
        return []
    endtry

    let makeprg = syntastic#util#shexpand(g:syntastic_{a:filetype}_compiler) .
        \ ' ' . flags . ' ' . syntastic#util#shexpand('%')

    let errorformat = s:getCheckerVar('g', a:filetype, a:subchecker, 'errorformat', a:options['errorformat'])

    let postprocess = s:getCheckerVar('g', a:filetype, a:subchecker, 'remove_include_errors', 0) ?
        \ ['filterForeignErrors'] : []

    " process makeprg
    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': postprocess })
endfunction " }}}2

" }}}1

" Private functions {{{1

" initialize c/cpp syntax checker handlers
function! s:init() " {{{2
    let s:handlers = []
    let s:cflags = {}

    call s:regHandler('\m\<cairo',       'syntastic#c#checkPKG', ['cairo', 'cairo'])
    call s:regHandler('\m\<freetype',    'syntastic#c#checkPKG', ['freetype', 'freetype2', 'freetype'])
    call s:regHandler('\m\<glade',       'syntastic#c#checkPKG', ['glade', 'libglade-2.0', 'libglade'])
    call s:regHandler('\m\<glib',        'syntastic#c#checkPKG', ['glib', 'glib-2.0', 'glib'])
    call s:regHandler('\m\<gtk',         'syntastic#c#checkPKG', ['gtk', 'gtk+-2.0', 'gtk+', 'glib-2.0', 'glib'])
    call s:regHandler('\m\<libsoup',     'syntastic#c#checkPKG', ['libsoup', 'libsoup-2.4', 'libsoup-2.2'])
    call s:regHandler('\m\<libxml',      'syntastic#c#checkPKG', ['libxml', 'libxml-2.0', 'libxml'])
    call s:regHandler('\m\<pango',       'syntastic#c#checkPKG', ['pango', 'pango'])
    call s:regHandler('\m\<SDL',         'syntastic#c#checkPKG', ['sdl', 'sdl'])
    call s:regHandler('\m\<opengl',      'syntastic#c#checkPKG', ['opengl', 'gl'])
    call s:regHandler('\m\<webkit',      'syntastic#c#checkPKG', ['webkit', 'webkit-1.0'])

    call s:regHandler('\m\<php\.h\>',    'syntastic#c#checkPHP',    [])
    call s:regHandler('\m\<Python\.h\>', 'syntastic#c#checkPython', [])
    call s:regHandler('\m\<ruby',        'syntastic#c#checkRuby',   [])
endfunction " }}}2

" return a handler dictionary object
function! s:regHandler(regex, function, args) " {{{2
    let handler = {}
    let handler["regex"] = a:regex
    let handler["func"] = function(a:function)
    let handler["args"] = a:args
    call add(s:handlers, handler)
endfunction " }}}2

" resolve checker-related user variables
function! s:getCheckerVar(scope, filetype, subchecker, name, default) " {{{2
    let prefix = a:scope . ':' . 'syntastic_'
    if exists(prefix . a:filetype . '_' . a:subchecker . '_' . a:name)
        return {a:scope}:syntastic_{a:filetype}_{a:subchecker}_{a:name}
    elseif exists(prefix . a:filetype . '_' . a:name)
        return {a:scope}:syntastic_{a:filetype}_{a:name}
    else
        return a:default
    endif
endfunction " }}}2

" resolve user CFLAGS
function! s:getCflags(ft, ck, opts) " {{{2
    " determine whether to parse header files as well
    if has_key(a:opts, 'header_names') && expand('%') =~? a:opts['header_names']
        if s:getCheckerVar('g', a:ft, a:ck, 'check_header', 0)
            let flags = get(a:opts, 'header_flags', '') . ' -c ' . syntastic#c#NullOutput()
        else
            " checking headers when check_header is unset: bail out
            throw 'Syntastic: skip checks'
        endif
    else
        let flags = get(a:opts, 'main_flags', '')
    endif

    let flags .= ' ' . s:getCheckerVar('g', a:ft, a:ck, 'compiler_options', '') . ' ' . s:getIncludeDirs(a:ft)

    " check if the user manually set some cflags
    let b_cflags = s:getCheckerVar('b', a:ft, a:ck, 'cflags', '')
    if b_cflags == ''
        " check whether to search for include files at all
        if !s:getCheckerVar('g', a:ft, a:ck, 'no_include_search', 0)
            if a:ft ==# 'c' || a:ft ==# 'cpp'
                " refresh the include file search if desired
                if s:getCheckerVar('g', a:ft, a:ck, 'auto_refresh_includes', 0)
                    let flags .= ' ' . s:searchHeaders()
                else
                    " search for header includes if not cached already
                    if !exists('b:syntastic_' . a:ft . '_includes')
                        let b:syntastic_{a:ft}_includes = s:searchHeaders()
                    endif
                    let flags .= ' ' . b:syntastic_{a:ft}_includes
                endif
            endif
        endif
    else
        " user-defined cflags
        let flags .= ' ' . b_cflags
    endif

    " add optional config file parameters
    let config_file = s:getCheckerVar('g', a:ft, a:ck, 'config_file', '.syntastic_' . a:ft . '_config')
    let flags .= ' ' . syntastic#c#ReadConfig(config_file)

    return flags
endfunction " }}}2

" get the gcc include directory argument depending on the default
" includes and the optional user-defined 'g:syntastic_c_include_dirs'
function! s:getIncludeDirs(filetype) " {{{2
    let include_dirs = []

    if a:filetype =~# '\v^%(c|cpp|d|objc|objcpp)$' &&
                \ (!exists('g:syntastic_'.a:filetype.'_no_default_include_dirs') ||
                \ !g:syntastic_{a:filetype}_no_default_include_dirs)
        let include_dirs = copy(s:default_includes)
    endif

    if exists('g:syntastic_'.a:filetype.'_include_dirs')
        call extend(include_dirs, g:syntastic_{a:filetype}_include_dirs)
    endif

    return join(map(syntastic#util#unique(include_dirs), 'syntastic#util#shescape("-I" . v:val)'))
endfunction " }}}2

" search the first 100 lines for include statements that are
" given in the handlers dictionary
function! s:searchHeaders() " {{{2
    let includes = ''
    let files = []
    let found = []
    let lines = filter(getline(1, 100), 'v:val =~# ''\m^\s*#\s*include''')

    " search current buffer
    for line in lines
        let file = matchstr(line, '\m"\zs\S\+\ze"')
        if file != ''
            call add(files, file)
            continue
        endif

        for handler in s:handlers
            if line =~# handler["regex"]
                let includes .= call(handler["func"], handler["args"])
                call add(found, handler["regex"])
                break
            endif
        endfor
    endfor

    " search included headers
    for hfile in files
        if hfile != ''
            let filename = expand('%:p:h') . syntastic#util#Slash() . hfile

            try
                let lines = readfile(filename, '', 100)
            catch /\m^Vim\%((\a\+)\)\=:E484/
                continue
            endtry

            call filter(lines, 'v:val =~# ''\m^\s*#\s*include''')

            for handler in s:handlers
                if index(found, handler["regex"]) != -1
                    continue
                endif

                for line in lines
                    if line =~# handler["regex"]
                        let includes .= call(handler["func"], handler["args"])
                        call add(found, handler["regex"])
                        break
                    endif
                endfor
            endfor
        endif
    endfor

    return includes
endfunction " }}}2

" try to find library with 'pkg-config'
" search possible libraries from first to last given
" argument until one is found
function! syntastic#c#checkPKG(name, ...) " {{{2
    if executable('pkg-config')
        if !has_key(s:cflags, a:name)
            for pkg in a:000
                let pkg_flags = system('pkg-config --cflags ' . pkg)
                " since we cannot necessarily trust the pkg-config exit code
                " we have to check for an error output as well
                if v:shell_error == 0 && pkg_flags !~? 'not found'
                    let pkg_flags = ' ' . substitute(pkg_flags, "\n", '', '')
                    let s:cflags[a:name] = pkg_flags
                    return pkg_flags
                endif
            endfor
        else
            return s:cflags[a:name]
        endif
    endif
    return ''
endfunction " }}}2

" try to find PHP includes with 'php-config'
function! syntastic#c#checkPHP() " {{{2
    if executable('php-config')
        if !has_key(s:cflags, 'php')
            let s:cflags['php'] = system('php-config --includes')
            let s:cflags['php'] = ' ' . substitute(s:cflags['php'], "\n", '', '')
        endif
        return s:cflags['php']
    endif
    return ''
endfunction " }}}2

" try to find the ruby headers with 'rbconfig'
function! syntastic#c#checkRuby() " {{{2
    if executable('ruby')
        if !has_key(s:cflags, 'ruby')
            let s:cflags['ruby'] = system('ruby -r rbconfig -e ' .
                \ '''puts RbConfig::CONFIG["rubyhdrdir"] || RbConfig::CONFIG["archdir"]''')
            let s:cflags['ruby'] = substitute(s:cflags['ruby'], "\n", '', '')
            let s:cflags['ruby'] = ' -I' . s:cflags['ruby']
        endif
        return s:cflags['ruby']
    endif
    return ''
endfunction " }}}2

" try to find the python headers with distutils
function! syntastic#c#checkPython() " {{{2
    if executable('python')
        if !has_key(s:cflags, 'python')
            let s:cflags['python'] = system('python -c ''from distutils import ' .
                \ 'sysconfig; import sys; sys.stdout.write(sysconfig.get_python_inc())''')
            let s:cflags['python'] = substitute(s:cflags['python'], "\n", '', '')
            let s:cflags['python'] = ' -I' . s:cflags['python']
        endif
        return s:cflags['python']
    endif
    return ''
endfunction " }}}2

" }}}1

" default include directories
let s:default_includes = [
    \ '.',
    \ '..',
    \ 'include',
    \ 'includes',
    \ '..' . syntastic#util#Slash() . 'include',
    \ '..' . syntastic#util#Slash() . 'includes' ]

call s:init()

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
