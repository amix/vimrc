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
function! go#util#IsWin() abort
  let win = ['win16', 'win32', 'win64', 'win95']
  for w in win
    if (has(w))
      return 1
    endif
  endfor

  return 0
endfunction

function! go#util#has_job() abort
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

function! go#util#goarch() abort
  return substitute(go#util#System('go env GOARCH'), '\n', '', 'g')
endfunction

function! go#util#goos() abort
  return substitute(go#util#System('go env GOOS'), '\n', '', 'g')
endfunction

function! go#util#goroot() abort
  return substitute(go#util#System('go env GOROOT'), '\n', '', 'g')
endfunction

function! go#util#gopath() abort
  return substitute(go#util#System('go env GOPATH'), '\n', '', 'g')
endfunction

function! go#util#osarch() abort
  return go#util#goos() . '_' . go#util#goarch()
endfunction

" System runs a shell command. It will reset the shell to /bin/sh for Unix-like
" systems if it is executable.
function! go#util#System(str, ...) abort
  let l:shell = &shell
  if !go#util#IsWin() && executable('/bin/sh')
    let &shell = '/bin/sh'
  endif

  try
    let l:output = call('system', [a:str] + a:000)
    return l:output
  finally
    let &shell = l:shell
  endtry
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
  let l:snippet_case = get(g:, 'go_snippet_case_type', "snakecase")
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
  let word = substitute(a:word,'::','/','g')
  let word = substitute(word,'\(\u\+\)\(\u\l\)','\1_\2','g')
  let word = substitute(word,'\(\l\|\d\)\(\u\)','\1_\2','g')
  let word = substitute(word,'[.-]','_','g')
  let word = tolower(word)
  return word
endfunction

" camelcase converts a string to camel case. i.e: FooBar -> fooBar
" Copied from tpope/vim-abolish
function! go#util#camelcase(word) abort
  let word = substitute(a:word, '-', '_', 'g')
  if word !~# '_' && word =~# '\l'
    return substitute(word,'^.','\l&','')
  else
    return substitute(word,'\C\(_\)\=\(.\)','\=submatch(1)==""?tolower(submatch(2)) : toupper(submatch(2))','g')
  endif
endfunction

function! go#util#AddTags(line1, line2, ...) abort
  " default is json
  let l:keys = ["json"]
  if a:0
    let l:keys = a:000
  endif

  let l:line1 = a:line1
  let l:line2 = a:line2

  " If we're inside a struct and just call this function let us add the tags
  " to all fields
  " TODO(arslan): I don't like using patterns. Check if we can move it to
  " `motion` and do it via AST based position
  let ln1 = searchpair('struct {', '', '}', 'bcnW')
  if ln1 == 0
    echon "vim-go: " | echohl ErrorMsg | echon "cursor is outside the struct" | echohl None
    return
  endif

  " searchpair only returns a single position
  let ln2 = search('}', "cnW")

  " if no range is given we apply for the whole struct
  if l:line1 == l:line2
    let l:line1 = ln1 + 1
    let l:line2 = ln2 - 1
  endif

  for line in range(l:line1, l:line2)
    " get the field name (word) that are not part of a commented line
    let l:matched = matchstr(getline(line), '\(\/\/.*\)\@<!\w\+')
    if empty(l:matched)
      continue
    endif

    let word = go#util#snippetcase(l:matched)
    let tags = map(copy(l:keys), 'printf("%s:%s", v:val,"\"'. word .'\"")')
    let updated_line = printf("%s `%s`", getline(line), join(tags, " "))

    " finally, update the line inplace
    call setline(line, updated_line)
  endfor
endfunction

" TODO(arslan): I couldn't parameterize the highlight types. Check if we can
" simplify the following functions
"
" NOTE(arslan): echon doesn't work well with redraw, thus echo doesn't print
" even though we order it. However echom seems to be work fine.
function! go#util#EchoSuccess(msg)
  redraw | echohl Function | echom "vim-go: " . a:msg | echohl None
endfunction

function! go#util#EchoError(msg)
  redraw | echohl ErrorMsg | echom "vim-go: " . a:msg | echohl None
endfunction

function! go#util#EchoWarning(msg)
  redraw | echohl WarningMsg | echom "vim-go: " . a:msg | echohl None
endfunction

function! go#util#EchoProgress(msg)
  redraw | echohl Identifier | echom "vim-go: " . a:msg | echohl None
endfunction

function! go#util#EchoInfo(msg)
  redraw | echohl Debug | echom "vim-go: " . a:msg | echohl None
endfunction

" vim: sw=2 ts=2 et
