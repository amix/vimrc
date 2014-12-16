function! go#tool#Files()
    if has ("win32")
        let command = 'go list -f "{{range $f := .GoFiles}}{{$.Dir}}/{{$f}}{{printf \"\n\"}}{{end}}"'
    else
        " let command = "go list -f $'{{range $f := .GoFiles}}{{$.Dir}}/{{$f}}\n{{end}}'"

        let command = "go list -f '{{range $f := .GoFiles}}{{$.Dir}}/{{$f}}{{printf \"\\n\"}}{{end}}'"

    endif
    let out = go#tool#ExecuteInDir(command)
    return split(out, '\n')
endfunction

function! go#tool#Deps()
    if has ("win32")
        let command = 'go list -f "{{range $f := .Deps}}{{$f}}{{printf \"\n\"}}{{end}}"'
    else
        let command = "go list -f $'{{range $f := .Deps}}{{$f}}\n{{end}}'"
    endif
    let out = go#tool#ExecuteInDir(command)
    return split(out, '\n')
endfunction

function! go#tool#Imports()
    let imports = {}
    if has ("win32")
        let command = 'go list -f "{{range $f := .Imports}}{{$f}}{{printf \"\n\"}}{{end}}"'
    else
        let command = "go list -f $'{{range $f := .Imports}}{{$f}}\n{{end}}'"
    endif
    let out = go#tool#ExecuteInDir(command)
    if v:shell_error
        echo out
        return imports
    endif

    for package_path in split(out, '\n')
        let package_name = fnamemodify(package_path, ":t")
        let imports[package_name] = package_path
    endfor

    return imports
endfunction

function! go#tool#ShowErrors(out)
    let errors = []
    for line in split(a:out, '\n')
        let tokens = matchlist(line, '^\s*\(.\{-}\):\(\d\+\):\s*\(.*\)')
        if !empty(tokens)
            call add(errors, {"filename" : expand("%:p:h:") . "/" . tokens[1],
                        \"lnum":     tokens[2],
                        \"text":     tokens[3]})
        elseif !empty(errors)
            " Preserve indented lines.
            " This comes up especially with multi-line test output.
            if match(line, '^\s') >= 0
                call add(errors, {"text": line})
            endif
        endif
    endfor

    if !empty(errors)
        call setqflist(errors, 'r')
        return
    endif

    if empty(errors)
        " Couldn't detect error format, output errors
        echo a:out
    endif
endfunction

function! go#tool#ExecuteInDir(cmd) abort
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
    let dir = getcwd()
    try
        execute cd.'`=expand("%:p:h")`'
        let out = system(a:cmd)
    finally
        execute cd.'`=dir`'
    endtry
    return out
endfunction

" Exists checks whether the given importpath exists or not. It returns 0 if
" the importpath exists under GOPATH.
function! go#tool#Exists(importpath)
    let command = "go list ". a:importpath
    let out = go#tool#ExecuteInDir(command)

    if v:shell_error
        return -1
    endif

    return 0
endfunction

" BinPath checks whether the given binary exists or not and returns the path
" of the binary. It returns an empty string doesn't exists.
function! go#tool#BinPath(binpath)
    " remove whitespaces if user applied something like 'goimports   '
    let binpath = substitute(a:binpath, '^\s*\(.\{-}\)\s*$', '\1', '')

    " if it's in PATH just return it
    if executable(binpath) 
        return binpath
    endif


    " just get the basename
    let basename = fnamemodify(binpath, ":t")

    " check if we have an appropriate bin_path
    let go_bin_path = GetBinPath()
    if empty(go_bin_path)
        echo "vim-go: could not find '" . basename . "'. Run :GoInstallBinaries to fix it."
        return ""
    endif

    " append our GOBIN and GOPATH paths and be sure they can be found there...
    " let us search in our GOBIN and GOPATH paths
    let old_path = $PATH
    let $PATH = $PATH . ":" .go_bin_path

    if !executable(binpath) 
        echo "vim-go: could not find '" . basename . "'. Run :GoInstallBinaries to fix it."
        return ""
    endif

    " restore back!
    if go_bin_path
        let $PATH = old_path
    endif

    return go_bin_path . '/' . basename
endfunction

" following two functions are from: https://github.com/mattn/gist-vim 
" thanks  @mattn
function! s:get_browser_command()
    let go_play_browser_command = get(g:, 'go_play_browser_command', '')
    if go_play_browser_command == ''
        if has('win32') || has('win64')
            let go_play_browser_command = '!start rundll32 url.dll,FileProtocolHandler %URL%'
        elseif has('mac') || has('macunix') || has('gui_macvim') || system('uname') =~? '^darwin'
            let go_play_browser_command = 'open %URL%'
        elseif executable('xdg-open')
            let go_play_browser_command = 'xdg-open %URL%'
        elseif executable('firefox')
            let go_play_browser_command = 'firefox %URL% &'
        else
            let go_play_browser_command = ''
        endif
    endif
    return go_play_browser_command
endfunction

function! go#tool#OpenBrowser(url)
    let cmd = s:get_browser_command()
    if len(cmd) == 0
        redraw
        echohl WarningMsg
        echo "It seems that you don't have general web browser. Open URL below."
        echohl None
        echo a:url
        return
    endif
    if cmd =~ '^!'
        let cmd = substitute(cmd, '%URL%', '\=shellescape(a:url)', 'g')
        silent! exec cmd
    elseif cmd =~ '^:[A-Z]'
        let cmd = substitute(cmd, '%URL%', '\=a:url', 'g')
        exec cmd
    else
        let cmd = substitute(cmd, '%URL%', '\=shellescape(a:url)', 'g')
        call system(cmd)
    endif
endfunction


" vim:ts=4:sw=4:et
