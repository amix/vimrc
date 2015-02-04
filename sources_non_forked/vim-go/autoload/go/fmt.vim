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

let s:got_fmt_error = 0

"  we have those problems : 
"  http://stackoverflow.com/questions/12741977/prevent-vim-from-updating-its-undo-tree
"  http://stackoverflow.com/questions/18532692/golang-formatter-and-vim-how-to-destroy-history-record?rq=1
"
"  The below function is an improved version that aims to fix all problems.
"  it doesn't undo changes and break undo history.  If you are here reading
"  this and have VimL experience, please look at the function for
"  improvements, patches are welcome :)
function! go#fmt#Format(withGoimport)
    " save cursor position and many other things
    let l:curw=winsaveview()

    " needed for testing if gofmt fails or not
    let l:tmpname=tempname()
    call writefile(getline(1,'$'), l:tmpname)


    if g:go_fmt_experimental == 1
        " save our undo file to be restored after we are done. This is needed to
        " prevent an additional undo jump due to BufWritePre auto command and also
        " restore 'redo' history because it's getting being destroyed every
        " BufWritePre
        let tmpundofile=tempname()
        exe 'wundo! ' . Tmpundofile
    endif

    " get the command first so we can test it
    let fmt_command = g:go_fmt_command
    if a:withGoimport  == 1 
        let fmt_command  = g:go_goimports_bin
    endif

    " if it's something else than gofmt, we need to check the existing of that
    " binary. For example if it's goimports, let us check if it's installed,
    " if not the user get's a warning via go#tool#BinPath()
    if fmt_command != "gofmt"
        " check if the user has installed goimports
        let bin_path = go#tool#BinPath(fmt_command) 
        if empty(bin_path) 
            return 
        endif

        let fmt_command = bin_path
    endif

    " populate the final command with user based fmt options
    let command = fmt_command . ' ' . g:go_fmt_options

    " execute our command...
    let out = system(command . " " . l:tmpname)

    "if there is no error on the temp file, gofmt again our original file
    if v:shell_error == 0
        " remove undo point caused via BufWritePre
        try | silent undojoin | catch | endtry

        " do not include stderr to the buffer, this is due to goimports/gofmt
        " tha fails with a zero exit return value (sad yeah).
        let default_srr = &srr
        set srr=>%s 

        " execufe gofmt on the current buffer and replace it
        silent execute "%!" . command

        " only clear quickfix if it was previously set, this prevents closing
        " other quickfixes
        if s:got_fmt_error 
            let s:got_fmt_error = 0
            call setqflist([])
            cwindow
        endif

        " put back the users srr setting
        let &srr = default_srr
    elseif g:go_fmt_fail_silently == 0 
        "otherwise get the errors and put them to quickfix window
        let errors = []
        for line in split(out, '\n')
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
            call setqflist(errors, 'r')
            echohl Error | echomsg "Gofmt returned error" | echohl None
        endif
        let s:got_fmt_error = 1
        cwindow
    endif

    if g:go_fmt_experimental == 1
        " restore our undo history
        silent! exe 'rundo ' . tmpundofile
        call delete(tmpundofile)
    endif

    " restore our cursor/windows positions
    call delete(l:tmpname)
    call winrestview(l:curw)
endfunction


" vim:ts=4:sw=4:et
