" install necessary Go tools
if exists("g:go_loaded_install")
  finish
endif
let g:go_loaded_install = 1

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! s:checkVersion() abort
  let l:unsupported = 0
  if go#config#VersionWarning() != 0
    if has('nvim')
      let l:unsupported = !has('nvim-0.4.0')
    else
      let l:unsupported = !has('patch-8.0.1453')
    endif

    if l:unsupported == 1
      echohl Error
      echom "vim-go requires at least Vim 8.0.1453 or Neovim 0.4.0, but you're using an older version."
      echom "Please update your Vim for the best vim-go experience."
      echom "If you really want to continue you can set this to make the error go away:"
      echom "    let g:go_version_warning = 0"
      echom "Note that some features may error out or behave incorrectly."
      echom "Please do not report bugs unless you're using at least Vim 8.0.1453 or Neovim 0.4.0."
      echohl None

      " Make sure people see this.
      sleep 2
    endif
  endif
endfunction

call s:checkVersion()

" these packages are used by vim-go and can be automatically installed if
" needed by the user with GoInstallBinaries.

" NOTE(bc): varying the binary name and the tail of the import path does not yet work in module aware mode.
let s:packages = {
      \ 'asmfmt':        ['github.com/klauspost/asmfmt/cmd/asmfmt@latest'],
      \ 'dlv':           ['github.com/go-delve/delve/cmd/dlv@latest'],
      \ 'errcheck':      ['github.com/kisielk/errcheck@latest'],
      \ 'fillstruct':    ['github.com/davidrjenni/reftools/cmd/fillstruct@master'],
      \ 'godef':         ['github.com/rogpeppe/godef@latest'],
      \ 'goimports':     ['golang.org/x/tools/cmd/goimports@master'],
      \ 'revive':        ['github.com/mgechev/revive@latest'],
      \ 'gopls':         ['golang.org/x/tools/gopls@latest', {}, {'after': function('go#lsp#Restart', [])}],
      \ 'golangci-lint': ['github.com/golangci/golangci-lint/cmd/golangci-lint@latest'],
      \ 'staticcheck':   ['honnef.co/go/tools/cmd/staticcheck@latest'],
      \ 'gomodifytags':  ['github.com/fatih/gomodifytags@latest'],
      \ 'gorename':      ['golang.org/x/tools/cmd/gorename@master'],
      \ 'gotags':        ['github.com/jstemmer/gotags@master'],
      \ 'guru':          ['golang.org/x/tools/cmd/guru@master'],
      \ 'impl':          ['github.com/josharian/impl@master'],
      \ 'keyify':        ['honnef.co/go/tools/cmd/keyify@master'],
      \ 'motion':        ['github.com/fatih/motion@latest'],
      \ 'iferr':         ['github.com/koron/iferr@master'],
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
    call go#util#EchoError('$GOPATH is not set and `go env GOPATH` returns empty')
    return
  endif

  let go_bin_path = go#path#BinPath()

  let [l:goos, l:goarch] = go#util#hostosarch()
  let Restore_goos = go#util#SetEnv('GOOS', l:goos)
  let Restore_goarch = go#util#SetEnv('GOARCH', l:goarch)

  " change $GOBIN so go can automatically install to it
  let Restore_gobin = go#util#SetEnv('GOBIN', go_bin_path)

  " vim's executable path is looking in PATH so add our go_bin path to it
  let Restore_path = go#util#SetEnv('PATH', go_bin_path . go#util#PathListSep() . $PATH)

  " when shellslash is set on MS-* systems, shellescape puts single quotes
  " around the output string. cmd on Windows does not handle single quotes
  " correctly. Unsetting shellslash forces shellescape to use double quotes
  " instead.
  let resetshellslash = 0
  if has('win32') && &shellslash
    let resetshellslash = 1
    set noshellslash
  endif

  let l:get_base_cmd = ['go', 'install', '-v', '-mod=readonly']

  " Filter packages from arguments (if any).
  let l:packages = {}
  if a:0 > 0
    for l:bin in a:000
      let l:version = substitute(l:bin, '.*@', '', '')
      if l:version == l:bin
        let l:version = ''
      endif
      let l:bin = substitute(l:bin, '@.*', '', '')

      let l:pkg = get(s:packages, l:bin, [])
      if len(l:pkg) == 0
        call go#util#EchoError('unknown binary: ' . l:bin)
        return
      endif

      if l:version isnot ''
        let l:pkg[0] = substitute(l:pkg[0], '@\zs.*', l:version, '')
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

  let l:oldmore = &more
  let &more = 0

  let Restore_modules = go#util#SetEnv('GO111MODULE', 'on')
  for [l:binary, l:pkg] in items(l:packages)
    let l:importPath = l:pkg[0]

    " TODO(bc): how to support this with modules? Do we have to clone and then
    " install manually? Probably not. I suspect that we can just use GOPATH
    " mode and then do the legacy method.
    let bin_setting_name = "go_" . l:binary . "_bin"

    let l:extension = ''
    if go#util#IsWin()
      let l:extension = '.exe'
    endif

    let bin = get(g:, bin_setting_name, l:binary . l:extension)

    if !executable(bin) || a:updateBinaries == 1
      if a:updateBinaries == 1
        echo "vim-go: Updating " . l:binary . ". Reinstalling ". importPath . " to folder " . go_bin_path
      else
        echo "vim-go: ". l:binary ." not found. Installing ". importPath . " to folder " . go_bin_path
      endif

      let l:tmpdir = go#util#tempdir('vim-go')
      try
        let l:dir = go#util#Chdir(l:tmpdir)
        let l:get_cmd = copy(l:get_base_cmd)

        if len(l:pkg) > 1 && get(l:pkg[1], l:platform, []) isnot []
          let l:get_cmd += get(l:pkg[1], l:platform, [])
        endif

        " TODO(bc): how to install the bin to a different name than the binary
        " path? go install does not support -o
        "let l:get_cmd += ['-o', printf('%s%s%s', go_bin_path, go#util#PathSep(), bin)]

        let [l:out, l:err] = go#util#Exec(l:get_cmd + [l:importPath])
        if l:err
          call go#util#EchoError(printf('Error installing %s: %s', l:importPath, l:out))
        endif
      finally
        call go#util#Chdir(l:dir)
      endtry

      if len(l:pkg) > 2
        call call(get(l:pkg[2], 'after', function('s:noop', [])), [])
      endif
    endif
  endfor

  " restore back!
  call call(Restore_modules, [])
  call call(Restore_path, [])
  call call(Restore_gobin, [])
  call call(Restore_goarch, [])
  call call(Restore_goos, [])

  if resetshellslash
    set shellslash
  endif

  if a:updateBinaries == 1
    call go#util#EchoInfo('updating finished!')
  else
    call go#util#EchoInfo('installing finished!')
  endif

  let &more = l:oldmore
endfunction

" CheckBinaries checks if the necessary binaries to install the Go tool
" commands are available.
function! s:CheckBinaries()
  if !executable('go')
    call go#util#EchoError('go executable not found.')
    return -1
  endif

  if !executable('git')
    call go#util#EchoError('git executable not found.')
    return -1
  endif
endfunction

" Autocommands
" ============================================================================
"

" We take care to preserve the user's fileencodings and fileformats,
" because those settings are global (not buffer local), yet we want
" to override them for loading Go files, which are defined to be UTF-8.
let s:current_fileformats = ''
let s:current_fileencodings = ''

" define fileencodings to open as utf-8 encoding even if it's ascii.
function! s:gofiletype_pre()
  let s:current_fileformats = &g:fileformats
  let s:current_fileencodings = &g:fileencodings
  set fileencodings=utf-8 fileformats=unix
endfunction

" restore fileencodings as others
function! s:gofiletype_post()
  let &g:fileformats = s:current_fileformats
  let &g:fileencodings = s:current_fileencodings
endfunction

function! s:register()
  if !(&modifiable && expand('<amatch>') ==# 'go')
    return
  endif

  " Resolve any symlinks in <afile> so that the filename will match what Vim
  " will ultimately and usually produce.
  call go#lsp#DidOpen(resolve(expand('<afile>:p')))
endfunction

function! s:noop(...) abort
endfunction

augroup vim-go
  autocmd!

  autocmd BufNewFile *.go if &modifiable | setlocal fileencoding=utf-8 fileformat=unix | endif
  autocmd BufNewFile *.go call go#auto#template_autocreate()
  autocmd BufRead *.go call s:gofiletype_pre()
  autocmd BufReadPost *.go call s:gofiletype_post()

  autocmd BufNewFile *.s if &modifiable | setlocal fileencoding=utf-8 fileformat=unix | endif
  autocmd BufRead *.s call s:gofiletype_pre()
  autocmd BufReadPost *.s call s:gofiletype_post()

  if go#util#has_job()
    autocmd FileType * call s:register()
  endif
augroup end

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
