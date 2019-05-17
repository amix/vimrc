" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

" initial_go_path is used to store the initial GOPATH that was set when Vim
" was started. It's used with :GoPathClear to restore the GOPATH when the user
" changed it explicitly via :GoPath. Initially it's empty. It's being set when
" :GoPath is used
let s:initial_go_path = ""

" GoPath sets or echos the current GOPATH. If no arguments are passed it
" echoes the current GOPATH, if an argument is passed it replaces the current
" GOPATH with it. If two double quotes are passed (the empty string in go),
" it'll clear the GOPATH and will restore to the initial GOPATH.
function! go#path#GoPath(...) abort
  " no argument, show GOPATH
  if len(a:000) == 0
    echo go#path#Default()
    return
  endif

  " we have an argument, replace GOPATH
  " clears the current manually set GOPATH and restores it to the
  " initial GOPATH, which was set when Vim was started.
  if len(a:000) == 1 && a:1 == '""'
    if !empty(s:initial_go_path)
      let $GOPATH = s:initial_go_path
      let s:initial_go_path = ""
    endif

    echon "vim-go: " | echohl Function | echon "GOPATH restored to ". $GOPATH | echohl None
    return
  endif

  echon "vim-go: " | echohl Function | echon "GOPATH changed to ". a:1 | echohl None
  let s:initial_go_path = $GOPATH
  let $GOPATH = a:1
endfunction

" Default returns the default GOPATH. If GOPATH is not set, it uses the
" default GOPATH set starting with Go 1.8. This GOPATH can be retrieved via
" 'go env GOPATH'
function! go#path#Default() abort
  if $GOPATH == ""
    " use default GOPATH via go env
    return go#util#env("gopath")
  endif

  return $GOPATH
endfunction

" s:HasPath checks whether the given path exists in GOPATH environment variable
" or not
function! s:HasPath(path) abort
  let go_paths = split(go#path#Default(), go#util#PathListSep())
  let last_char = strlen(a:path) - 1

  " check cases of '/foo/bar/' and '/foo/bar'
  if a:path[last_char] == go#util#PathSep()
    let withSep = a:path
    let noSep = strpart(a:path, 0, last_char)
  else
    let withSep = a:path . go#util#PathSep()
    let noSep = a:path
  endif

  let hasA = index(go_paths, withSep) != -1
  let hasB = index(go_paths, noSep) != -1
  return hasA || hasB
endfunction

" Detect returns the current GOPATH. If a package manager is used, such as
" Godeps, GB, it will modify the GOPATH so those directories take precedence
" over the current GOPATH. It also detects diretories whose are outside
" GOPATH.
function! go#path#Detect() abort
  let gopath = go#path#Default()

  let current_dir = fnameescape(expand('%:p:h'))

  " TODO(arslan): this should be changed so folders or files should be
  " fetched from a customizable list. The user should define any new package
  " management tool by it's own.

  " src folders outside $GOPATH
  let src_roots = finddir("src", current_dir .";", -1)

  " for cases like GOPATH/src/foo/src/bar, pick up GOPATH/src instead of
  " GOPATH/src/foo/src
  let src_root = ""
  if len(src_roots) > 0
    let src_root = src_roots[-1]
  endif

  if !empty(src_root)
    let src_path = fnamemodify(src_root, ':p:h:h') . go#util#PathSep()

    " gb vendor plugin
    " (https://github.com/constabulary/gb/tree/master/cmd/gb-vendor)
    let gb_vendor_root = src_path . "vendor" . go#util#PathSep()
    if isdirectory(gb_vendor_root) && !s:HasPath(gb_vendor_root)
      let gopath = gb_vendor_root . go#util#PathListSep() . gopath
    endif

    if !s:HasPath(src_path)
      let gopath =  src_path . go#util#PathListSep() . gopath
    endif
  endif

  " Godeps
  let godeps_root = finddir("Godeps", current_dir .";")
  if !empty(godeps_root)
    let godeps_path = join([fnamemodify(godeps_root, ':p:h:h'), "Godeps", "_workspace" ], go#util#PathSep())

    if !s:HasPath(godeps_path)
      let gopath =  godeps_path . go#util#PathListSep() . gopath
    endif
  endif

  " Fix up the case where initial $GOPATH is empty,
  " and we end up with a trailing :
  let gopath = substitute(gopath, ":$", "", "")
  return gopath
endfunction

" BinPath returns the binary path of installed go tools.
function! go#path#BinPath() abort
  let bin_path = go#config#BinPath()
  if bin_path != ""
    return bin_path
  endif

  " check if our global custom path is set, if not check if $GOBIN is set so
  " we can use it, otherwise use default GOPATH
  if $GOBIN != ""
    let bin_path = $GOBIN
  else
    let go_paths = split(go#path#Default(), go#util#PathListSep())
    if len(go_paths) == 0
      return "" "nothing found
    endif
    let bin_path = expand(go_paths[0] . "/bin/")
  endif

  return bin_path
endfunction

" CheckBinPath checks whether the given binary exists or not and returns the
" path of the binary, respecting the go_bin_path and go_search_bin_path_first
" settings. It returns an empty string if the binary doesn't exist.
function! go#path#CheckBinPath(binpath) abort
  " remove whitespaces if user applied something like 'goimports   '
  let binpath = substitute(a:binpath, '^\s*\(.\{-}\)\s*$', '\1', '')

  " save original path
  let old_path = $PATH

  " check if we have an appropriate bin_path
  let go_bin_path = go#path#BinPath()
  if !empty(go_bin_path)
    " append our GOBIN and GOPATH paths and be sure they can be found there...
    " let us search in our GOBIN and GOPATH paths
    " respect the ordering specified by go_search_bin_path_first
    if go#config#SearchBinPathFirst()
      let $PATH = go_bin_path . go#util#PathListSep() . $PATH
    else
      let $PATH = $PATH . go#util#PathListSep() . go_bin_path
    endif
  endif

  " if it's in PATH just return it
  if executable(binpath)
    if exists('*exepath')
      let binpath = exepath(binpath)
    endif
    let $PATH = old_path

    if go#util#IsUsingCygwinShell() == 1
      return s:CygwinPath(binpath)
    endif

    return binpath
  endif

  " just get the basename
  let basename = fnamemodify(binpath, ":t")
  if !executable(basename)
    call go#util#EchoError(printf("could not find '%s'. Run :GoInstallBinaries to fix it", basename))

    " restore back!
    let $PATH = old_path
    return ""
  endif

  let $PATH = old_path

  if go#util#IsUsingCygwinShell() == 1
    return s:CygwinPath(a:binpath)
  endif

  return go_bin_path . go#util#PathSep() . basename
endfunction

function! s:CygwinPath(path)
   return substitute(a:path, '\\', '/', "g")
endfunction

" go#path#ToURI converts path to a file URI. path should be an absolute path.
" Relative paths cannot be properly converted to a URI; when path is a
" relative path, the file scheme will not be prepended.
function! go#path#ToURI(path)
  let l:absolute = !go#util#IsWin() && a:path[0] is# '/'
  let l:prefix = ''
  let l:path = a:path

  if go#util#IsWin() && l:path[1:2] is# ':\'
    let l:absolute = 1
    let l:prefix = '/' . l:path[0:1]
    let l:path = l:path[2:]
  endif

  return substitute(
  \   (l:absolute ? 'file://' : '') . l:prefix . go#uri#EncodePath(l:path),
  \   '\\',
  \   '/',
  \   'g',
  \)
endfunction

function! go#path#FromURI(uri) abort
    let l:i = len('file://')
    let l:encoded_path = a:uri[: l:i - 1] is# 'file://' ? a:uri[l:i :] : a:uri

    let l:path = go#uri#Decode(l:encoded_path)

    " If the path is like /C:/foo/bar, it should be C:\foo\bar instead.
    if go#util#IsWin() && l:path =~# '^/[a-zA-Z]:'
        let l:path = substitute(l:path[1:], '/', '\\', 'g')
    endif

    return l:path
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
