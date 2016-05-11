if !exists("g:go_gocode_bin")
    let g:go_gocode_bin = "gocode"
endif


fu! s:gocodeCurrentBuffer()
    let buf = getline(1, '$')
    if &encoding != 'utf-8'
        let buf = map(buf, 'iconv(v:val, &encoding, "utf-8")')
    endif
    if &l:fileformat == 'dos'
        " XXX: line2byte() depend on 'fileformat' option.
        " so if fileformat is 'dos', 'buf' must include '\r'.
        let buf = map(buf, 'v:val."\r"')
    endif
    let file = tempname()
    call writefile(buf, file)

    return file
endf

if go#vimproc#has_vimproc()
    let s:vim_system = get(g:, 'gocomplete#system_function', 'vimproc#system2')
    let s:vim_shell_error = get(g:, 'gocomplete#shell_error_function', 'vimproc#get_last_status')
else
    let s:vim_system = get(g:, 'gocomplete#system_function', 'system')
    let s:vim_shell_error = ''
endif

fu! s:shell_error()
    if empty(s:vim_shell_error)
        return v:shell_error
    endif
    return call(s:vim_shell_error, [])
endf

fu! s:system(str, ...)
    return call(s:vim_system, [a:str] + a:000)
endf

fu! s:gocodeShellescape(arg)
    if go#vimproc#has_vimproc()
        return vimproc#shellescape(a:arg)
    endif
    try
        let ssl_save = &shellslash
        set noshellslash
        return shellescape(a:arg)
    finally
        let &shellslash = ssl_save
    endtry
endf

fu! s:gocodeCommand(cmd, preargs, args)
    for i in range(0, len(a:args) - 1)
        let a:args[i] = s:gocodeShellescape(a:args[i])
    endfor
    for i in range(0, len(a:preargs) - 1)
        let a:preargs[i] = s:gocodeShellescape(a:preargs[i])
    endfor

    let bin_path = go#path#CheckBinPath(g:go_gocode_bin)
    if empty(bin_path)
        return
    endif

    " we might hit cache problems, as gocode doesn't handle well different
    " GOPATHS: https://github.com/nsf/gocode/issues/239
    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()

    let result = s:system(printf('%s %s %s %s', s:gocodeShellescape(bin_path), join(a:preargs), s:gocodeShellescape(a:cmd), join(a:args)))

    let $GOPATH = old_gopath

    if s:shell_error() != 0
        return "[\"0\", []]"
    else
        if &encoding != 'utf-8'
            let result = iconv(result, 'utf-8', &encoding)
        endif
        return result
    endif
endf

fu! s:gocodeCurrentBufferOpt(filename)
    return '-in=' . a:filename
endf

fu! s:gocodeAutocomplete()
    let filename = s:gocodeCurrentBuffer()
    let result = s:gocodeCommand('autocomplete',
                \ [s:gocodeCurrentBufferOpt(filename), '-f=vim'],
                \ [expand('%:p'), go#util#OffsetCursor()])
    call delete(filename)
    return result
endf

function! go#complete#GetInfo()
    let offset = go#util#OffsetCursor()+1
    let filename = s:gocodeCurrentBuffer()
    let result = s:gocodeCommand('autocomplete',
                \ [s:gocodeCurrentBufferOpt(filename), '-f=godit'],
                \ [expand('%:p'), offset])
    call delete(filename)

    " first line is: Charcount,,NumberOfCandidates, i.e: 8,,1
    " following lines are candiates, i.e:  func foo(name string),,foo(
    let out = split(result, '\n')

    " no candidates are found
    if len(out) == 1
        return ""
    endif

    " only one candiate is found
    if len(out) == 2
        return split(out[1], ',,')[0]
    endif

    " to many candidates are available, pick one that maches the word under the
    " cursor
    let infos = []
    for info in out[1:]
        call add(infos, split(info, ',,')[0])
    endfor

    let wordMatch = '\<' . expand("<cword>") . '\>'
    " escape single quotes in wordMatch before passing it to filter
    let wordMatch = substitute(wordMatch, "'", "''", "g")
    let filtered =  filter(infos, "v:val =~ '".wordMatch."'")

    if len(filtered) == 1
        return filtered[0]
    endif

    return ""
endfunction

function! go#complete#Info(auto)
    " auto is true if we were called by g:go_auto_type_info's autocmd
    let result = go#complete#GetInfo()
    if !empty(result)
        " if auto, and the result is a PANIC by gocode, hide it
        if a:auto && result ==# 'PANIC PANIC PANIC' | return | endif
        echo "vim-go: " | echohl Function | echon result | echohl None
    endif
endfunction

function! s:trim_bracket(val)
    let a:val.word = substitute(a:val.word, '[(){}\[\]]\+$', '', '')
    return a:val
endfunction

fu! go#complete#Complete(findstart, base)
    "findstart = 1 when we need to get the text length
    if a:findstart == 1
        execute "silent let g:gocomplete_completions = " . s:gocodeAutocomplete()
        return col('.') - g:gocomplete_completions[0] - 1
        "findstart = 0 when we need to return the list of completions
    else
        let s = getline(".")[col('.') - 1]
        if s =~ '[(){}\{\}]'
            return map(copy(g:gocomplete_completions[1]), 's:trim_bracket(v:val)')
        endif
        return g:gocomplete_completions[1]
    endif
endf

" vim:ts=4:sw=4:et
