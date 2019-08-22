" install necessary Go tools
if exists("g:go_loaded_install")
  finish
endif
let g:go_loaded_install = 1

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! s:checkVersion() abort
  " Not using the has('patch-7.4.2009') syntax because that wasn't added until
  " 7.4.237, and we want to be sure this works for everyone (this is also why
  " we're not using utils#EchoError()).
  "
  " Version 7.4.2009 was chosen because that's greater than what the most recent Ubuntu LTS
  " release (16.04) uses and has a couple of features we need (e.g. execute()
  " and :message clear).

  let l:unsupported = 0
  if go#config#VersionWarning() != 0
    if has('nvim')
      let l:unsupported = !has('nvim-0.3.2')
    else
      let l:unsupported = (v:version < 704 || (v:version == 704 && !has('patch2009')))
    endif

    if l:unsupported == 1
      echohl Error
      echom "vim-go requires Vim 7.4.2009 or Neovim 0.3.2, but you're using an older version."
      echom "Please update your Vim for the best vim-go experience."
      echom "If you really want to continue you can set this to make the error go away:"
      echom "    let g:go_version_warning = 0"
      echom "Note that some features may error out or behave incorrectly."
      echom "Please do not report bugs unless you're using Vim 7.4.2009 or newer or Neovim 0.3.2."
      echohl None

      " Make sure people see this.
      sleep 2
    endif
  endif
endfunction

call s:checkVersion()

" these packages are used by vim-go and can be automatically installed if
" needed by the user with GoInstallBinaries.
let s:packages = {
      \ 'asmfmt':        ['github.com/klauspost/asmfmt/cmd/asmfmt'],
      \ 'dlv':           ['github.com/go-delve/delve/cmd/dlv'],
      \ 'errcheck':      ['github.com/kisielk/errcheck'],
      \ 'fillstruct':    ['github.com/davidrjenni/reftools/cmd/fillstruct'],
      \ 'gocode':        ['github.com/mdempsky/gocode', {'windows': ['-ldflags', '-H=windowsgui']}],
      \ 'gocode-gomod':  ['github.com/stamblerre/gocode'],
      \ 'godef':         ['github.com/rogpeppe/godef'],
      \ 'gogetdoc':      ['github.com/zmb3/gogetdoc'],
      \ 'goimports':     ['golang.org/x/tools/cmd/goimports'],
      \ 'golint':        ['golang.org/x/lint/golint'],
      \ 'gopls':         ['golang.org/x/tools/gopls@latest', {}, {'after': function('go#lsp#Restart', [])}],
      \ 'gometalinter':  ['github.com/alecthomas/gometalinter'],
      \ 'golangci-lint': ['github.com/golangci/golangci-lint/cmd/golangci-lint'],
      \ 'gomodifytags':  ['github.com/fatih/gomodifytags'],
      \ 'gorename':      ['golang.org/x/tools/cmd/gorename'],
      \ 'gotags':        ['github.com/jstemmer/gotags'],
      \ 'guru':          ['golang.org/x/tools/cmd/guru'],
      \ 'impl':          ['github.com/josharian/impl'],
      \ 'keyify':        ['honnef.co/go/tools/cmd/keyify'],
      \ 'motion':        ['github.com/fatih/motion'],
      \ 'iferr':         ['github.com/koron/iferr'],
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

  let l:get_base_cmd = ['go', 'get', '-v']

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

  let l:oldmore = &more
  let &more = 0

  for [l:binary, l:pkg] in items(l:packages)
    let l:importPath = l:pkg[0]

    " TODO(bc): how to support this with modules? Do we have to clone and then
    " install manually? Probably not. I suspect that we can just use GOPATH
    " mode and then do the legacy method.
    let bin_setting_name = "go_" . l:binary . "_bin"

    if exists("g:{bin_setting_name}")
      let bin = g:{bin_setting_name}
    else
      if go#util#IsWin()
        let bin = l:binary . '.exe'
      else
        let bin = l:binary
      endif
    endif

    if !executable(bin) || a:updateBinaries == 1
      if a:updateBinaries == 1
        echo "vim-go: Updating " . l:binary . ". Reinstalling ". importPath . " to folder " . go_bin_path
      else
        echo "vim-go: ". l:binary ." not found. Installing ". importPath . " to folder " . go_bin_path
      endif

      if l:importPath =~ "@"
        let Restore_modules = go#util#SetEnv('GO111MODULE', 'on')
        let l:tmpdir = go#util#tempdir('vim-go')
        let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
        let l:dir = getcwd()
        try
          execute l:cd . fnameescape(l:tmpdir)
          let l:get_cmd = copy(l:get_base_cmd)

          " first download the binary
          let [l:out, l:err] = go#util#Exec(l:get_cmd + [l:importPath])
          if l:err
            echom "Error installing " . l:importPath . ": " . l:out
          endif

          call call(Restore_modules, [])
        finally
          execute l:cd . fnameescape(l:dir)
        endtry
        call call(Restore_modules, [])
      else
        let l:get_cmd = copy(l:get_base_cmd)
        let l:get_cmd += ['-d']
        if get(g:, "go_get_update", 1) != 0
          let l:get_cmd += ['-u']
        endif

        " GO111MODULE must be off to install gometalinter.
        let Restore_modules = go#util#SetEnv('GO111MODULE', 'off')

        " first download the binary
        let [l:out, l:err] = go#util#Exec(l:get_cmd + [l:importPath])
        if l:err
          echom "Error downloading " . l:importPath . ": " . l:out
        endif

        " and then build and install it
        let l:build_cmd = ['go', 'build', '-o', go_bin_path . go#util#PathSep() . bin, l:importPath]
        if len(l:pkg) > 1 && get(l:pkg[1], l:platform, '') isnot ''
          let l:build_cmd += get(l:pkg[1], l:platform, '')
        endif

        let [l:out, l:err] = go#util#Exec(l:build_cmd)
        if l:err
          echom "Error installing " . l:importPath . ": " . l:out
        endif


        call call(Restore_modules, [])
      endif

      if len(l:pkg) > 2
        call call(get(l:pkg[2], 'after', function('s:noop', [])), [])
      endif
    endif
  endfor

  " restore back!
  call call(Restore_path, [])

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

  let l:RestoreGopath = function('s:noop')
  if go#config#AutodetectGopath()
    let l:RestoreGopath = go#util#SetEnv('GOPATH', go#path#Detect())
  endif
  call go#lsp#DidOpen(expand('<afile>:p'))
  call call(l:RestoreGopath, [])
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
