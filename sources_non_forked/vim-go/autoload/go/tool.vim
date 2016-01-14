function! go#tool#Files()
    if go#util#IsWin()
        let command = 'go list -f "{{range $f := .GoFiles}}{{$.Dir}}\{{$f}}{{printf \"\n\"}}{{end}}{{range $f := .CgoFiles}}{{$.Dir}}\{{$f}}{{printf \"\n\"}}{{end}}"'
    else
        let command = "go list -f '{{range $f := .GoFiles}}{{$.Dir}}/{{$f}}{{printf \"\\n\"}}{{end}}{{range $f := .CgoFiles}}{{$.Dir}}/{{$f}}{{printf \"\\n\"}}{{end}}'"
    endif
    let out = go#tool#ExecuteInDir(command)
    return split(out, '\n')
endfunction

function! go#tool#Deps()
    if go#util#IsWin()
        let command = 'go list -f "{{range $f := .Deps}}{{$f}}{{printf \"\n\"}}{{end}}"'
    else
        let command = "go list -f $'{{range $f := .Deps}}{{$f}}\n{{end}}'"
    endif
    let out = go#tool#ExecuteInDir(command)
    return split(out, '\n')
endfunction

function! go#tool#Imports()
    let imports = {}
    if go#util#IsWin()
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
        let cmd = "go list -f {{.Name}} " . package_path
        let package_name = substitute(go#tool#ExecuteInDir(cmd), '\n$', '', '')
        let imports[package_name] = package_path
    endfor

    return imports
endfunction

function! go#tool#ParseErrors(lines)
    let errors = []

    for line in a:lines
        let fatalerrors = matchlist(line, '^\(fatal error:.*\)$')
        let tokens = matchlist(line, '^\s*\(.\{-}\):\(\d\+\):\s*\(.*\)')

        if !empty(fatalerrors)
            call add(errors, {"text": fatalerrors[1]})
        elseif !empty(tokens)
            " strip endlines of form ^M
            let out=substitute(tokens[3], '\r$', '', '')

            call add(errors, {
                        \ "filename" : fnamemodify(tokens[1], ':p'),
                        \ "lnum"     : tokens[2],
                        \ "text"     : out,
                        \ })
        elseif !empty(errors)
            " Preserve indented lines.
            " This comes up especially with multi-line test output.
            if match(line, '^\s') >= 0
                call add(errors, {"text": line})
            endif
        endif
    endfor

    return errors
endfunction

"FilterValids filters the given items with only items that have a valid
"filename. Any non valid filename is filtered out.
function! go#tool#FilterValids(items)
    " Remove any nonvalid filename from the location list to avoid opening an
    " empty buffer. See https://github.com/fatih/vim-go/issues/287 for
    " details.
    let filtered = []
    let is_readable = {}

    for item in a:items
        if has_key(item, 'bufnr')
            let filename = bufname(item.bufnr)
        elseif has_key(item, 'filename')
            let filename = item.filename
        else
            " nothing to do, add item back to the list
            call add(filtered, item)
            continue
        endif

        if !has_key(is_readable, filename)
            let is_readable[filename] = filereadable(filename)
        endif
        if is_readable[filename]
            call add(filtered, item)
        endif
    endfor

    for k in keys(filter(is_readable, '!v:val'))
        echo "vim-go: " | echohl Identifier | echon "[run] Dropped " | echohl Constant | echon  '"' . k . '"'
        echohl Identifier | echon " from location list (nonvalid filename)" | echohl None
    endfor

    return filtered
endfunction

function! go#tool#ExecuteInDir(cmd) abort
    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()

    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
    let dir = getcwd()
    try
        execute cd . fnameescape(expand("%:p:h"))
        let out = system(a:cmd)
    finally
        execute cd . fnameescape(dir)
    endtry

    let $GOPATH = old_gopath
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


" following two functions are from: https://github.com/mattn/gist-vim 
" thanks  @mattn
function! s:get_browser_command()
    let go_play_browser_command = get(g:, 'go_play_browser_command', '')
    if go_play_browser_command == ''
        if go#util#IsWin()
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
