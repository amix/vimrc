" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" fmt.vim: Vim command to format Go files with gofmt.
"
" This filetype plugin add a new commands for go buffers:
"
"   :Fmt
"
"       Filter the current Go buffer through gofmt.
"       It tries to preserve cursor position and avoids
"       replacing the buffer with stderr output.
"
" Options:
"
"   g:go_fmt_command [default="gofmt"]
"
"       Flag naming the gofmt executable to use.
"
"   g:go_fmt_autosave [default=1]
"
"       Flag to auto call :Fmt when saved file
"

if !exists("g:go_fmt_command")
    let g:go_fmt_command = "gofmt"
endif

if !exists("g:go_goimports_bin")
    let g:go_goimports_bin = "goimports"
endif

if !exists('g:go_fmt_fail_silently')
    let g:go_fmt_fail_silently = 0
endif

if !exists('g:go_fmt_options')
    let g:go_fmt_options = ''
endif

if !exists("g:go_fmt_experimental")
    let g:go_fmt_experimental = 0
endif

"  we have those problems : 
"  http://stackoverflow.com/questions/12741977/prevent-vim-from-updating-its-undo-tree
"  http://stackoverflow.com/questions/18532692/golang-formatter-and-vim-how-to-destroy-history-record?rq=1
"
"  The below function is an improved version that aims to fix all problems.
"  it doesn't undo changes and break undo history.  If you are here reading
"  this and have VimL experience, please look at the function for
"  improvements, patches are welcome :)
function! go#fmt#Format(withGoimport)
    if g:go_fmt_experimental == 1
        " Using winsaveview to save/restore cursor state has the problem of
        " closing folds on save:
        "   https://github.com/fatih/vim-go/issues/502
        " One fix is to use mkview instead. Unfortunately, this sometimes causes
        " other bad side effects:
        "   https://github.com/fatih/vim-go/issues/728
        " and still closes all folds if foldlevel>0:
        "   https://github.com/fatih/vim-go/issues/732
        let l:curw = {}
        try
            mkview!
        catch
            let l:curw = winsaveview()
        endtry
    else
        " Save cursor position and many other things.
        let l:curw = winsaveview()
    endif

    " Write current unsaved buffer to a temp file
    let l:tmpname = tempname()
    call writefile(getline(1, '$'), l:tmpname)

    if g:go_fmt_experimental == 1
        " save our undo file to be restored after we are done. This is needed to
        " prevent an additional undo jump due to BufWritePre auto command and also
        " restore 'redo' history because it's getting being destroyed every
        " BufWritePre
        let tmpundofile = tempname()
        exe 'wundo! ' . tmpundofile
    endif

    " get the command first so we can test it
    let fmt_command = g:go_fmt_command
    if a:withGoimport  == 1 
        let fmt_command  = g:go_goimports_bin
    endif

    " check if the user has installed command binary.
    " For example if it's goimports, let us check if it's installed,
    " if not the user get's a warning via go#path#CheckBinPath()
    let bin_path = go#path#CheckBinPath(fmt_command)
    if empty(bin_path)
        return
    endif

    if fmt_command != "gofmt"
        " change GOPATH too, so goimports can pick up the correct library
        let old_gopath = $GOPATH
        let $GOPATH = go#path#Detect()

        let fmt_command = bin_path
    endif

    " populate the final command with user based fmt options
    let command = fmt_command . ' -w '
    if a:withGoimport  != 1 
        let command  = command . g:go_fmt_options
    endif

    if fmt_command == "goimports"
        if !exists('b:goimports_vendor_compatible')
            let out = go#util#System("goimports --help")
            if out !~ "-srcdir"
                echohl WarningMsg
                echomsg "vim-go: goimports does not support srcdir."
                echomsg "  update with: :GoUpdateBinaries"
                echohl None
            else
               let b:goimports_vendor_compatible = 1
            endif
        endif

        if exists('b:goimports_vendor_compatible') && b:goimports_vendor_compatible
            let ssl_save = &shellslash
            set noshellslash
            let command  = command . '-srcdir ' . shellescape(expand("%:p:h"))
            let &shellslash = ssl_save
        endif
    endif

    " execute our command...
    if go#util#IsWin()
        let l:tmpname = tr(l:tmpname, '\', '/')
    endif
    let out = go#util#System(command . " " . l:tmpname)

    if fmt_command != "gofmt"
        let $GOPATH = old_gopath
    endif

    let l:listtype = "locationlist"
    "if there is no error on the temp file replace the output with the current
    "file (if this fails, we can always check the outputs first line with:
    "splitted =~ 'package \w\+')
    if go#util#ShellError() == 0
        " remove undo point caused via BufWritePre
        try | silent undojoin | catch | endtry

        " Replace current file with temp file, then reload buffer
        let old_fileformat = &fileformat
        call rename(l:tmpname, expand('%'))
        silent edit!
        let &fileformat = old_fileformat
        let &syntax = &syntax

        " clean up previous location list, but only if it's due to fmt
        if exists('b:got_fmt_error') && b:got_fmt_error
            let b:got_fmt_error = 0
            call go#list#Clean(l:listtype)
            call go#list#Window(l:listtype)
        endif
    elseif g:go_fmt_fail_silently == 0
        let splitted = split(out, '\n')
        "otherwise get the errors and put them to location list
        let errors = []
        for line in splitted
            let tokens = matchlist(line, '^\(.\{-}\):\(\d\+\):\(\d\+\)\s*\(.*\)')
            if !empty(tokens)
                call add(errors, {"filename": @%,
                            \"lnum":     tokens[2],
                            \"col":      tokens[3],
                            \"text":     tokens[4]})
            endif
        endfor
        if empty(errors)
            % | " Couldn't detect gofmt error format, output errors
        endif
        if !empty(errors)
            call go#list#Populate(l:listtype, errors)
            echohl Error | echomsg "Gofmt returned error" | echohl None
        endif

        let b:got_fmt_error = 1
        call go#list#Window(l:listtype, len(errors))

        " We didn't use the temp file, so clean up
        call delete(l:tmpname)
    endif

    if g:go_fmt_experimental == 1
        " restore our undo history
        silent! exe 'rundo ' . tmpundofile
        call delete(tmpundofile)
    endif

    if g:go_fmt_experimental == 1
        " Restore our cursor/windows positions, folds, etc.
        if empty(l:curw)
            silent! loadview
        else
            call winrestview(l:curw)
        endif
    else
        " Restore our cursor/windows positions.
        call winrestview(l:curw)
    endif
endfunction


" vim:ts=4:sw=4:et
