" install necessary Go tools
if exists("g:go_loaded_install")
  finish
endif
let g:go_loaded_install = 1

" Not using the has('patch-7.4.1689') syntax because that wasn't added until
" 7.4.237, and we want to be sure this works for everyone (this is also why
" we're not using utils#EchoError()).
"
" Version 7.4.1689 was chosen because that's what the most recent Ubuntu LTS
" release (16.04) uses.
if
      \ get(g:, 'go_version_warning', 1) != 0 &&
      \ (v:version < 704 || (v:version == 704 && !has('patch1689')))
      \ && !has('nvim')
  echohl Error
  echom "vim-go requires Vim 7.4.1689 or Neovim, but you're using an older version."
  echom "Please update your Vim for the best vim-go experience."
  echom "If you really want to continue you can set this to make the error go away:"
  echom "    let g:go_version_warning = 0"
  echom "Note that some features may error out or behave incorrectly."
  echom "Please do not report bugs unless you're using Vim 7.4.1689 or newer."
  echohl None

  " Make sure people see this.
  sleep 2
endif

" these packages are used by vim-go and can be automatically installed if
" needed by the user with GoInstallBinaries
let s:packages = {
      \ 'asmfmt':        ['github.com/klauspost/asmfmt/cmd/asmfmt'],
      \ 'errcheck':      ['github.com/kisielk/errcheck'],
      \ 'fillstruct':    ['github.com/davidrjenni/reftools/cmd/fillstruct'],
      \ 'gocode':        ['github.com/nsf/gocode', {'windows': '-ldflags -H=windowsgui'}],
      \ 'godef':         ['github.com/rogpeppe/godef'],
      \ 'gogetdoc':      ['github.com/zmb3/gogetdoc'],
      \ 'goimports':     ['golang.org/x/tools/cmd/goimports'],
      \ 'golint':        ['github.com/golang/lint/golint'],
      \ 'gometalinter':  ['github.com/alecthomas/gometalinter'],
      \ 'gomodifytags':  ['github.com/fatih/gomodifytags'],
      \ 'gorename':      ['golang.org/x/tools/cmd/gorename'],
      \ 'gotags':        ['github.com/jstemmer/gotags'],
      \ 'guru':          ['golang.org/x/tools/cmd/guru'],
      \ 'impl':          ['github.com/josharian/impl'],
      \ 'keyify':        ['github.com/dominikh/go-tools/cmd/keyify'],
      \ 'motion':        ['github.com/fatih/motion'],
\ }

" These commands are available on any filetypes
command! -nargs=* -complete=customlist,s:complete GoInstallBinaries call s:GoInstallBinaries(-1, <f-args>)
command! -nargs=* -complete=customlist,s:complete GoUpdateBinaries  call s:GoInstallBinaries(1, <f-args>)
command! -nargs=? -complete=dir GoPath call go#path#GoPath(<f-args>)

fun! s:complete(lead, cmdline, cursor)
  return filter(keys(s:packages), 'strpart(v:val, 0, len(a:lead)) == a:lead')
endfun

" GoInstallBinaries downloads and installs binaries defined in s:packages to
" $GOBIN or $GOPATH/bin. GoInstallBinaries will update already installed
" binaries only if updateBinaries = 1. By default, all packages in s:packages
" will be installed, but the set can be limited by passing the desired
" packages in the unnamed arguments.
function! s:GoInstallBinaries(updateBinaries, ...)
  let err = s:CheckBinaries()
  if err != 0
    return
  endif

  if go#path#Default() == ""
    echohl Error
    echomsg "vim.go: $GOPATH is not set and 'go env GOPATH' returns empty"
    echohl None
    return
  endif

  let go_bin_path = go#path#BinPath()

  " change $GOBIN so go get can automatically install to it
  let $GOBIN = go_bin_path

  " old_path is used to restore users own path
  let old_path = $PATH

  " vim's executable path is looking in PATH so add our go_bin path to it
  let $PATH = go_bin_path . go#util#PathListSep() . $PATH

  " when shellslash is set on MS-* systems, shellescape puts single quotes
  " around the output string. cmd on Windows does not handle single quotes
  " correctly. Unsetting shellslash forces shellescape to use double quotes
  " instead.
  let resetshellslash = 0
  if has('win32') && &shellslash
    let resetshellslash = 1
    set noshellslash
  endif

  let cmd = "go get -v "
  if get(g:, "go_get_update", 1) != 0
    let cmd .= "-u "
  endif

  let s:go_version = matchstr(go#util#System("go version"), '\d.\d.\d')

  " https://github.com/golang/go/issues/10791
  if s:go_version > "1.4.0" && s:go_version < "1.5.0"
    let cmd .= "-f "
  endif

  " Filter packages from arguments (if any).
  let l:packages = {}
  if a:0 > 0
    for l:bin in a:000
      let l:pkg = get(s:packages, l:bin, [])
      if len(l:pkg) == 0
        call go#util#EchoError('unknown binary: ' . l:bin)
        return
      endif
      let l:packages[l:bin] = l:pkg
    endfor
  else
    let l:packages = s:packages
  endif

  let l:platform = ''
  if go#util#IsWin()
    let l:platform = 'windows'
  endif

  for [binary, pkg] in items(l:packages)
    let l:importPath = pkg[0]
    let l:goGetFlags = len(pkg) > 1 ? get(pkg[1], l:platform, '') : ''

    let binname = "go_" . binary . "_bin"

    let bin = binary
    if exists("g:{binname}")
      let bin = g:{binname}
    endif

    if !executable(bin) || a:updateBinaries == 1
      if a:updateBinaries == 1
        echo "vim-go: Updating " . binary . ". Reinstalling ". importPath . " to folder " . go_bin_path
      else
        echo "vim-go: ". binary ." not found. Installing ". importPath . " to folder " . go_bin_path
      endif

      let out = go#util#System(printf('%s %s %s', cmd, l:goGetFlags, shellescape(importPath)))
      if go#util#ShellError() != 0
        echom "Error installing " . importPath . ": " . out
      endif
    endif
  endfor

  " restore back!
  let $PATH = old_path
  if resetshellslash
    set shellslash
  endif
endfunction

" CheckBinaries checks if the necessary binaries to install the Go tool
" commands are available.
function! s:CheckBinaries()
  if !executable('go')
    echohl Error | echomsg "vim-go: go executable not found." | echohl None
    return -1
  endif

  if !executable('git')
    echohl Error | echomsg "vim-go: git executable not found." | echohl None
    return -1
  endif
endfunction

" Autocommands
" ============================================================================
"
function! s:echo_go_info()
  if !get(g:, "go_echo_go_info", 1)
    return
  endif

  if !exists('v:completed_item') || empty(v:completed_item)
    return
  endif
  let item = v:completed_item

  if !has_key(item, "info")
    return
  endif

  if empty(item.info)
    return
  endif

  redraws! | echo "vim-go: " | echohl Function | echon item.info | echohl None
endfunction

function! s:auto_type_info()
  " GoInfo automatic update
  if get(g:, "go_auto_type_info", 0)
    call go#tool#Info(1)
  endif
endfunction

function! s:auto_sameids()
  " GoSameId automatic update
  if get(g:, "go_auto_sameids", 0)
    call go#guru#SameIds()
  endif
endfunction

function! s:fmt_autosave()
  " Go code formatting on save
  if get(g:, "go_fmt_autosave", 1)
    call go#fmt#Format(-1)
  endif
endfunction

function! s:asmfmt_autosave()
  " Go asm formatting on save
  if get(g:, "go_asmfmt_autosave", 0)
    call go#asmfmt#Format()
  endif
endfunction

function! s:metalinter_autosave()
  " run gometalinter on save
  if get(g:, "go_metalinter_autosave", 0)
    call go#lint#Gometa(1)
  endif
endfunction

function! s:template_autocreate()
  " create new template from scratch
  if get(g:, "go_template_autocreate", 1)
    call go#template#create()
  endif
endfunction

augroup vim-go
  autocmd!

  autocmd CursorHold *.go call s:auto_type_info()
  autocmd CursorHold *.go call s:auto_sameids()

  " Echo the identifier information when completion is done. Useful to see
  " the signature of a function, etc...
  if exists('##CompleteDone')
    autocmd CompleteDone *.go call s:echo_go_info()
  endif

  autocmd BufWritePre *.go call s:fmt_autosave()
  autocmd BufWritePre *.s call s:asmfmt_autosave()
  autocmd BufWritePost *.go call s:metalinter_autosave()
  autocmd BufNewFile *.go call s:template_autocreate()
  " clear SameIds when the buffer is unloaded so that loading another buffer
  " in the same window doesn't highlight the most recently matched
  " identifier's positions.
  autocmd BufWinEnter *.go call go#guru#ClearSameIds()

  autocmd BufEnter *.go
        \  if get(g:, 'go_autodetect_gopath', 0) && !exists('b:old_gopath')
        \|   let b:old_gopath = exists('$GOPATH') ? $GOPATH : -1
        \|   let $GOPATH = go#path#Detect()
        \| endif
  autocmd BufLeave *.go
        \  if exists('b:old_gopath')
        \|   if b:old_gopath isnot -1
        \|     let $GOPATH = b:old_gopath
        \|   endif
        \|   unlet b:old_gopath
        \| endif
augroup end

" vim: sw=2 ts=2 et
