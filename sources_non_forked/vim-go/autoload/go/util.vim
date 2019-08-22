" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

" PathSep returns the appropriate OS specific path separator.
function! go#util#PathSep() abort
  if go#util#IsWin()
    return '\'
  endif
  return '/'
endfunction

" PathListSep returns the appropriate OS specific path list separator.
function! go#util#PathListSep() abort
  if go#util#IsWin()
    return ";"
  endif
  return ":"
endfunction

" LineEnding returns the correct line ending, based on the current fileformat
function! go#util#LineEnding() abort
  if &fileformat == 'dos'
    return "\r\n"
  elseif &fileformat == 'mac'
    return "\r"
  endif

  return "\n"
endfunction

" Join joins any number of path elements into a single path, adding a
" Separator if necessary and returns the result
function! go#util#Join(...) abort
  return join(a:000, go#util#PathSep())
endfunction

" IsWin returns 1 if current OS is Windows or 0 otherwise
" Note that has('win32') is always 1 when has('win64') is 1, so has('win32') is enough.
function! go#util#IsWin() abort
  return has('win32')
endfunction

" IsMac returns 1 if current OS is macOS or 0 otherwise.
function! go#util#IsMac() abort
  return has('mac') ||
        \ has('macunix') ||
        \ has('gui_macvim') ||
        \ go#util#Exec(['uname'])[0] =~? '^darwin'
endfunction

 " Checks if using:
 " 1) Windows system,
 " 2) And has cygpath executable,
 " 3) And uses *sh* as 'shell'
function! go#util#IsUsingCygwinShell()
  return go#util#IsWin() && executable('cygpath') && &shell =~ '.*sh.*'
endfunction

" Check if Vim jobs API is supported.
"
" The (optional) first parameter can be added to indicate the 'cwd' or 'env'
" parameters will be used, which wasn't added until a later version.
function! go#util#has_job(...) abort
  if has('nvim')
    return 1
  endif

  " cwd and env parameters to job_start was added in this version.
  if a:0 > 0 && a:1 is 1
    return has('job') && has("patch-8.0.0902")
  endif

  " job was introduced in 7.4.xxx however there are multiple bug fixes and one
  " of the latest is 8.0.0087 which is required for a stable async API.
  return has('job') && has("patch-8.0.0087")
endfunction

let s:env_cache = {}

" env returns the go environment variable for the given key. Where key can be
" GOARCH, GOOS, GOROOT, etc... It caches the result and returns the cached
" version.
function! go#util#env(key) abort
  let l:key = tolower(a:key)
  if has_key(s:env_cache, l:key)
    return s:env_cache[l:key]
  endif

  if executable('go')
    let l:var = call('go#util#'.l:key, [])
    if go#util#ShellError() != 0
      call go#util#EchoError(printf("'go env %s' failed", toupper(l:key)))
      return ''
    endif
  else
    let l:var = eval("$".toupper(a:key))
  endif

  let s:env_cache[l:key] = l:var
  return l:var
endfunction

" goarch returns 'go env GOARCH'. This is an internal function and shouldn't
" be used. Instead use 'go#util#env("goarch")'
function! go#util#goarch() abort
  return substitute(s:exec(['go', 'env', 'GOARCH'])[0], '\n', '', 'g')
endfunction

" goos returns 'go env GOOS'. This is an internal function and shouldn't
" be used. Instead use 'go#util#env("goos")'
function! go#util#goos() abort
  return substitute(s:exec(['go', 'env', 'GOOS'])[0], '\n', '', 'g')
endfunction

" goroot returns 'go env GOROOT'. This is an internal function and shouldn't
" be used. Instead use 'go#util#env("goroot")'
function! go#util#goroot() abort
  return substitute(s:exec(['go', 'env', 'GOROOT'])[0], '\n', '', 'g')
endfunction

" gopath returns 'go env GOPATH'. This is an internal function and shouldn't
" be used. Instead use 'go#util#env("gopath")'
function! go#util#gopath() abort
  return substitute(s:exec(['go', 'env', 'GOPATH'])[0], '\n', '', 'g')
endfunction

" gomod returns 'go env GOMOD'. gomod changes depending on the folder. Don't
" use go#util#env as it caches the value.
function! go#util#gomod() abort
  return substitute(s:exec(['go', 'env', 'GOMOD'])[0], '\n', '', 'g')
endfunction

function! go#util#osarch() abort
  return go#util#env("goos") . '_' . go#util#env("goarch")
endfunction

" go#util#ModuleRoot returns the root directory of the module of the current
" buffer.
function! go#util#ModuleRoot() abort
  let [l:out, l:err] = go#util#ExecInDir(['go', 'env', 'GOMOD'])
  if l:err != 0
    return -1
  endif

  let l:module = split(l:out, '\n', 1)[0]

  " When run with `GO111MODULE=on and not in a module directory, the module will be reported as /dev/null.
  let l:fakeModule = '/dev/null'
  if go#util#IsWin()
    let l:fakeModule = 'NUL'
  endif

  if l:fakeModule == l:module
    return expand('%:p:h')
  endif

  return fnamemodify(l:module, ':p:h')
endfunction

" Run a shell command.
"
" It will temporary set the shell to /bin/sh for Unix-like systems if possible,
" so that we always use a standard POSIX-compatible Bourne shell (and not e.g.
" csh, fish, etc.) See #988 and #1276.
function! s:system(cmd, ...) abort
  " Preserve original shell, shellredir and shellcmdflag values
  let l:shell = &shell
  let l:shellredir = &shellredir
  let l:shellcmdflag = &shellcmdflag

  if !go#util#IsWin() && executable('/bin/sh')
      set shell=/bin/sh shellredir=>%s\ 2>&1 shellcmdflag=-c
  endif

  try
    return call('system', [a:cmd] + a:000)
  finally
    " Restore original values
    let &shell = l:shell
    let &shellredir = l:shellredir
    let &shellcmdflag = l:shellcmdflag
  endtry
endfunction

" System runs a shell command "str". Every arguments after "str" is passed to
" stdin.
function! go#util#System(str, ...) abort
  return call('s:system', [a:str] + a:000)
endfunction

" Exec runs a shell command "cmd", which must be a list, one argument per item.
" Every list entry will be automatically shell-escaped
" Every other argument is passed to stdin.
function! go#util#Exec(cmd, ...) abort
  if len(a:cmd) == 0
    call go#util#EchoError("go#util#Exec() called with empty a:cmd")
    return ['', 1]
  endif

  let l:bin = a:cmd[0]

  " Lookup the full path, respecting settings such as 'go_bin_path'. On errors,
  " CheckBinPath will show a warning for us.
  let l:bin = go#path#CheckBinPath(l:bin)
  if empty(l:bin)
    return ['', 1]
  endif

  " Finally execute the command using the full, resolved path. Do not pass the
  " unmodified command as the correct program might not exist in $PATH.
  return call('s:exec', [[l:bin] + a:cmd[1:]] + a:000)
endfunction

function! go#util#ExecInDir(cmd, ...) abort
  if !isdirectory(expand("%:p:h"))
    return ['', 1]
  endif

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  try
    execute cd . fnameescape(expand("%:p:h"))
    let [l:out, l:err] = call('go#util#Exec', [a:cmd] + a:000)
  finally
    execute cd . fnameescape(l:dir)
  endtry
  return [l:out, l:err]
endfunction

function! s:exec(cmd, ...) abort
  let l:bin = a:cmd[0]
  let l:cmd = go#util#Shelljoin([l:bin] + a:cmd[1:])
  if go#util#HasDebug('shell-commands')
    call go#util#EchoInfo('shell command: ' . l:cmd)
  endif

  let l:out = call('s:system', [l:cmd] + a:000)
  return [l:out, go#util#ShellError()]
endfunction

function! go#util#ShellError() abort
  return v:shell_error
endfunction

" StripPath strips the path's last character if it's a path separator.
" example: '/foo/bar/'  -> '/foo/bar'
function! go#util#StripPathSep(path) abort
  let last_char = strlen(a:path) - 1
  if a:path[last_char] == go#util#PathSep()
    return strpart(a:path, 0, last_char)
  endif

  return a:path
endfunction

" StripTrailingSlash strips the trailing slash from the given path list.
" example: ['/foo/bar/']  -> ['/foo/bar']
function! go#util#StripTrailingSlash(paths) abort
  return map(copy(a:paths), 'go#util#StripPathSep(v:val)')
endfunction

" Shelljoin returns a shell-safe string representation of arglist. The
" {special} argument of shellescape() may optionally be passed.
function! go#util#Shelljoin(arglist, ...) abort
  try
    let ssl_save = &shellslash
    set noshellslash
    if a:0
      return join(map(copy(a:arglist), 'shellescape(v:val, ' . a:1 . ')'), ' ')
    endif

    return join(map(copy(a:arglist), 'shellescape(v:val)'), ' ')
  finally
    let &shellslash = ssl_save
  endtry
endfunction

fu! go#util#Shellescape(arg)
  try
    let ssl_save = &shellslash
    set noshellslash
    return shellescape(a:arg)
  finally
    let &shellslash = ssl_save
  endtry
endf

" Shelllist returns a shell-safe representation of the items in the given
" arglist. The {special} argument of shellescape() may optionally be passed.
function! go#util#Shelllist(arglist, ...) abort
  try
    let ssl_save = &shellslash
    set noshellslash
    if a:0
      return map(copy(a:arglist), 'shellescape(v:val, ' . a:1 . ')')
    endif
    return map(copy(a:arglist), 'shellescape(v:val)')
  finally
    let &shellslash = ssl_save
  endtry
endfunction

" Returns the byte offset for line and column
function! go#util#Offset(line, col) abort
  if &encoding != 'utf-8'
    let sep = go#util#LineEnding()
    let buf = a:line == 1 ? '' : (join(getline(1, a:line-1), sep) . sep)
    let buf .= a:col == 1 ? '' : getline('.')[:a:col-2]
    return len(iconv(buf, &encoding, 'utf-8'))
  endif
  return line2byte(a:line) + (a:col-2)
endfunction
"
" Returns the byte offset for the cursor
function! go#util#OffsetCursor() abort
  return go#util#Offset(line('.'), col('.'))
endfunction

" Windo is like the built-in :windo, only it returns to the window the command
" was issued from
function! go#util#Windo(command) abort
  let s:currentWindow = winnr()
  try
    execute "windo " . a:command
  finally
    execute s:currentWindow. "wincmd w"
    unlet s:currentWindow
  endtry
endfunction

" snippetcase converts the given word to given preferred snippet setting type
" case.
function! go#util#snippetcase(word) abort
  let l:snippet_case = go#config#AddtagsTransform()
  if l:snippet_case == "snakecase"
    return go#util#snakecase(a:word)
  elseif l:snippet_case == "camelcase"
    return go#util#camelcase(a:word)
  else
    return a:word " do nothing
  endif
endfunction

" snakecase converts a string to snake case. i.e: FooBar -> foo_bar
" Copied from tpope/vim-abolish
function! go#util#snakecase(word) abort
  let word = substitute(a:word, '::', '/', 'g')
  let word = substitute(word, '\(\u\+\)\(\u\l\)', '\1_\2', 'g')
  let word = substitute(word, '\(\l\|\d\)\(\u\)', '\1_\2', 'g')
  let word = substitute(word, '[.-]', '_', 'g')
  let word = tolower(word)
  return word
endfunction

" camelcase converts a string to camel case. e.g. FooBar or foo_bar will become
" fooBar.
" Copied from tpope/vim-abolish.
function! go#util#camelcase(word) abort
  let word = substitute(a:word, '-', '_', 'g')
  if word !~# '_' && word =~# '\l'
    return substitute(word, '^.', '\l&', '')
  else
    return substitute(word, '\C\(_\)\=\(.\)', '\=submatch(1)==""?tolower(submatch(2)) : toupper(submatch(2))','g')
  endif
endfunction

" pascalcase converts a string to 'PascalCase'. e.g. fooBar or foo_bar will
" become FooBar.
function! go#util#pascalcase(word) abort
  let word = go#util#camelcase(a:word)
  return toupper(word[0]) . word[1:]
endfunction

" Echo a message to the screen and highlight it with the group in a:hi.
"
" The message can be a list or string; every line with be :echomsg'd separately.
function! s:echo(msg, hi)
  let l:msg = []
  if type(a:msg) != type([])
    let l:msg = split(a:msg, "\n")
  else
    let l:msg = a:msg
  endif

  " Tabs display as ^I or <09>, so manually expand them.
  let l:msg = map(l:msg, 'substitute(v:val, "\t", "        ", "")')

  exe 'echohl ' . a:hi
  for line in l:msg
    echom "vim-go: " . line
  endfor
  echohl None
endfunction

function! go#util#EchoSuccess(msg)
  call s:echo(a:msg, 'Function')
endfunction
function! go#util#EchoError(msg)
  call s:echo(a:msg, 'ErrorMsg')
endfunction
function! go#util#EchoWarning(msg)
  call s:echo(a:msg, 'WarningMsg')
endfunction
function! go#util#EchoProgress(msg)
  redraw
  call s:echo(a:msg, 'Identifier')
endfunction
function! go#util#EchoInfo(msg)
  call s:echo(a:msg, 'Debug')
endfunction

" Get all lines in the buffer as a a list.
function! go#util#GetLines()
  let buf = getline(1, '$')
  if &encoding != 'utf-8'
    let buf = map(buf, 'iconv(v:val, &encoding, "utf-8")')
  endif
  if &l:fileformat == 'dos'
    " XXX: line2byte() depend on 'fileformat' option.
    " so if fileformat is 'dos', 'buf' must include '\r'.
    let buf = map(buf, 'v:val."\r"')
  endif
  return buf
endfunction

" Convert the current buffer to the "archive" format of
" golang.org/x/tools/go/buildutil:
" https://godoc.org/golang.org/x/tools/go/buildutil#ParseOverlayArchive
"
" > The archive consists of a series of files. Each file consists of a name, a
" > decimal file size and the file contents, separated by newlinews. No newline
" > follows after the file contents.
function! go#util#archive()
    let l:buffer = join(go#util#GetLines(), "\n")
    return expand("%:p:gs!\\!/!") . "\n" . strlen(l:buffer) . "\n" . l:buffer
endfunction

" Make a named temporary directory which starts with "prefix".
"
" Unfortunately Vim's tempname() is not portable enough across various systems;
" see: https://github.com/mattn/vim-go/pull/3#discussion_r138084911
function! go#util#tempdir(prefix) abort
  " See :help tempfile
  if go#util#IsWin()
    let l:dirs = [$TMP, $TEMP, 'c:\tmp', 'c:\temp']
  else
    let l:dirs = [$TMPDIR, '/tmp', './', $HOME]
  endif

  let l:dir = ''
  for l:d in dirs
    if !empty(l:d) && filewritable(l:d) == 2
      let l:dir = l:d
      break
    endif
  endfor

  if l:dir == ''
    call go#util#EchoError('Unable to find directory to store temporary directory in')
    return
  endif

  " Not great randomness, but "good enough" for our purpose here.
  let l:rnd = sha256(printf('%s%s', reltimestr(reltime()), fnamemodify(bufname(''), ":p")))
  let l:tmp = printf("%s/%s%s", l:dir, a:prefix, l:rnd)
  call mkdir(l:tmp, 'p', 0700)
  return l:tmp
endfunction

" Report if the user enabled a debug flag in g:go_debug.
function! go#util#HasDebug(flag)
  return index(go#config#Debug(), a:flag) >= 0
endfunction

function! go#util#OpenBrowser(url) abort
    let l:cmd = go#config#PlayBrowserCommand()
    if len(l:cmd) == 0
        redraw
        echohl WarningMsg
        echo "It seems that you don't have general web browser. Open URL below."
        echohl None
        echo a:url
        return
    endif

    " if setting starts with a !.
    if l:cmd =~ '^!'
        let l:cmd = substitute(l:cmd, '%URL%', '\=escape(shellescape(a:url), "#")', 'g')
        silent! exec l:cmd
    elseif cmd =~ '^:[A-Z]'
        let l:cmd = substitute(l:cmd, '%URL%', '\=escape(a:url,"#")', 'g')
        exec l:cmd
    else
        let l:cmd = substitute(l:cmd, '%URL%', '\=shellescape(a:url)', 'g')
        call go#util#System(l:cmd)
    endif
endfunction

function! go#util#ParseErrors(lines) abort
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
        call add(errors, {"text": substitute(line, '\r$', '', '')})
      endif
    endif
  endfor

  return errors
endfunction

function! go#util#ShowInfo(info)
  if empty(a:info)
    return
  endif

  echo "vim-go: " | echohl Function | echon a:info | echohl None
endfunction

" go#util#SetEnv takes the name of an environment variable and what its value
" should be and returns a function that will restore it to its original value.
function! go#util#SetEnv(name, value) abort
  let l:state = {}

  if len(a:name) == 0
    return function('s:noop', [], l:state)
  endif

  let l:remove = 0
  if exists('$' . a:name)
    let l:oldvalue = eval('$' . a:name)
  else
    let l:remove = 1
  endif

  " wrap the value in single quotes so that it will work on windows when there
  " are backslashes present in the value (e.g. $PATH).
  call execute('let $' . a:name . " = '" . a:value . "'")

  if l:remove
    function! s:remove(name) abort
      call execute('unlet $' . a:name)
    endfunction
    return function('s:remove', [a:name], l:state)
  endif

  return function('go#util#SetEnv', [a:name, l:oldvalue], l:state)
endfunction

function! s:noop(...) abort dict
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
