" From "go list -h".
function! go#tool#ValidFiles(...)
  let l:list = ["GoFiles", "CgoFiles", "IgnoredGoFiles", "CFiles", "CXXFiles",
    \ "MFiles", "HFiles", "FFiles", "SFiles", "SwigFiles", "SwigCXXFiles",
    \ "SysoFiles", "TestGoFiles", "XTestGoFiles"]

  " Used as completion
  if len(a:000) > 0
    let l:list = filter(l:list, 'strpart(v:val, 0, len(a:1)) == a:1')
  endif

  return l:list
endfunction

function! go#tool#Files(...) abort
  if len(a:000) > 0
    let source_files = a:000
  else
    let source_files = ['GoFiles']
  endif

  let combined = ''
  for sf in source_files
    " Strip dot in case people used ":GoFiles .GoFiles".
    let sf = substitute(sf, '^\.', '', '')

    " Make sure the passed options are valid.
    if index(go#tool#ValidFiles(), sf) == -1
      echoerr "unknown source file variable: " . sf
    endif

    if go#util#IsWin()
      let combined .= '{{range $f := .' . sf . '}}{{$.Dir}}\{{$f}}{{printf \"\n\"}}{{end}}{{range $f := .CgoFiles}}{{$.Dir}}\{{$f}}{{printf \"\n\"}}{{end}}'
    else
      let combined .= "{{range $f := ." . sf . "}}{{$.Dir}}/{{$f}}{{printf \"\\n\"}}{{end}}{{range $f := .CgoFiles}}{{$.Dir}}/{{$f}}{{printf \"\\n\"}}{{end}}"
    endif
  endfor

  let out = go#tool#ExecuteInDir('go list -f ' . shellescape(combined))
  return split(out, '\n')
endfunction

function! go#tool#Deps() abort
  if go#util#IsWin()
    let format = '{{range $f := .Deps}}{{$f}}{{printf \"\n\"}}{{end}}'
  else
    let format = "{{range $f := .Deps}}{{$f}}\n{{end}}"
  endif
  let command = 'go list -f '.shellescape(format)
  let out = go#tool#ExecuteInDir(command)
  return split(out, '\n')
endfunction

function! go#tool#Imports() abort
  let imports = {}
  if go#util#IsWin()
    let format = '{{range $f := .Imports}}{{$f}}{{printf \"\n\"}}{{end}}'
  else
    let format = "{{range $f := .Imports}}{{$f}}{{printf \"\\n\"}}{{end}}"
  endif
  let command = 'go list -f '.shellescape(format)
  let out = go#tool#ExecuteInDir(command)
  if go#util#ShellError() != 0
    echo out
    return imports
  endif

  for package_path in split(out, '\n')
    let cmd = "go list -f '{{.Name}}' " . shellescape(package_path)
    let package_name = substitute(go#tool#ExecuteInDir(cmd), '\n$', '', '')
    let imports[package_name] = package_path
  endfor

  return imports
endfunction

function! go#tool#Info(auto) abort
  let l:mode = get(g:, 'go_info_mode', 'gocode')
  if l:mode == 'gocode'
    call go#complete#Info(a:auto)
  elseif l:mode == 'guru'
    call go#guru#DescribeInfo()
  else
    call go#util#EchoError('go_info_mode value: '. l:mode .' is not valid. Valid values are: [gocode, guru]')
  endif
endfunction

function! go#tool#PackageName() abort
  let command = "go list -f \"{{.Name}}\""
  let out = go#tool#ExecuteInDir(command)
  if go#util#ShellError() != 0
      return -1
  endif

  return split(out, '\n')[0]
endfunction

function! go#tool#ParseErrors(lines) abort
  let errors = []

  for line in a:lines
    let fatalerrors = matchlist(line, '^\(fatal error:.*\)$')
    let tokens = matchlist(line, '^\s*\(.\{-}\):\(\d\+\):\s*\(.*\)')

    if !empty(fatalerrors)
      call add(errors, {"text": fatalerrors[1]})
    elseif !empty(tokens)
      " strip endlines of form ^M
      let out = substitute(tokens[3], '\r$', '', '')

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
function! go#tool#FilterValids(items) abort
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
  " Verify that the directory actually exists. If the directory does not
  " exist, then assume that the a:cmd should not be executed. Callers expect
  " to check v:shell_error (via go#util#ShellError()), so execute a command
  " that will return an error as if a:cmd was run and exited with an error.
  " This helps avoid errors when working with plugins that use virtual files
  " that don't actually exist on the file system (e.g. vim-fugitive's
  " GitDiff).
  if !isdirectory(expand("%:p:h"))
    let [out, err] = go#util#Exec(["false"])
    return ''
  endif

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  try
    execute cd . fnameescape(expand("%:p:h"))
    let out = go#util#System(a:cmd)
  finally
    execute cd . fnameescape(dir)
  endtry
  return out
endfunction

" Exists checks whether the given importpath exists or not. It returns 0 if
" the importpath exists under GOPATH.
function! go#tool#Exists(importpath) abort
    let command = "go list ". a:importpath
    let out = go#tool#ExecuteInDir(command)

    if go#util#ShellError() != 0
        return -1
    endif

    return 0
endfunction

" following two functions are from: https://github.com/mattn/gist-vim
" thanks  @mattn
function! s:get_browser_command() abort
    let go_play_browser_command = get(g:, 'go_play_browser_command', '')
    if go_play_browser_command == ''
        if go#util#IsWin()
            let go_play_browser_command = '!start rundll32 url.dll,FileProtocolHandler %URL%'
        elseif go#util#IsMac()
            let go_play_browser_command = 'open %URL%'
        elseif executable('xdg-open')
            let go_play_browser_command = 'xdg-open %URL%'
        elseif executable('firefox')
            let go_play_browser_command = 'firefox %URL% &'
        elseif executable('chromium')
            let go_play_browser_command = 'chromium %URL% &'
        else
            let go_play_browser_command = ''
        endif
    endif
    return go_play_browser_command
endfunction

function! go#tool#OpenBrowser(url) abort
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
        let cmd = substitute(cmd, '%URL%', '\=escape(shellescape(a:url),"#")', 'g')
        silent! exec cmd
    elseif cmd =~ '^:[A-Z]'
        let cmd = substitute(cmd, '%URL%', '\=escape(a:url,"#")', 'g')
        exec cmd
    else
        let cmd = substitute(cmd, '%URL%', '\=shellescape(a:url)', 'g')
        call go#util#System(cmd)
    endif
endfunction

" vim: sw=2 ts=2 et
