" install necessary Go tools
if exists("g:go_loaded_install")
    finish
endif
let g:go_loaded_install = 1

" these packages are used by vim-go and can be automatically installed if
" needed by the user with GoInstallBinaries
let s:packages = [
            \ "github.com/nsf/gocode",
            \ "github.com/alecthomas/gometalinter", 
            \ "golang.org/x/tools/cmd/goimports",
            \ "golang.org/x/tools/cmd/guru",
            \ "golang.org/x/tools/cmd/gorename",
            \ "github.com/golang/lint/golint",
            \ "github.com/kisielk/errcheck",
            \ "github.com/jstemmer/gotags",
            \ "github.com/klauspost/asmfmt/cmd/asmfmt",
            \ "github.com/fatih/motion",
            \ "github.com/zmb3/gogetdoc",
            \ "github.com/josharian/impl",
            \ ]

" These commands are available on any filetypes
command! GoInstallBinaries call s:GoInstallBinaries(-1)
command! GoUpdateBinaries call s:GoInstallBinaries(1)
command! -nargs=? -complete=dir GoPath call go#path#GoPath(<f-args>)


" GoInstallBinaries downloads and install all necessary binaries stated in the
" packages variable. It uses by default $GOBIN or $GOPATH/bin as the binary
" target install directory. GoInstallBinaries doesn't install binaries if they
" exist, to update current binaries pass 1 to the argument.
function! s:GoInstallBinaries(updateBinaries)
    if $GOPATH == ""
        echohl Error
        echomsg "vim.go: $GOPATH is not set"
        echohl None
        return
    endif

    let err = s:CheckBinaries()
    if err != 0
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

    let cmd = "go get -u -v "

    let s:go_version = matchstr(go#util#System("go version"), '\d.\d.\d')

    " https://github.com/golang/go/issues/10791
    if s:go_version > "1.4.0" && s:go_version < "1.5.0"
        let cmd .= "-f " 
    endif

    for pkg in s:packages
        let basename = fnamemodify(pkg, ":t")
        let binname = "go_" . basename . "_bin"

        let bin = basename
        if exists("g:{binname}")
            let bin = g:{binname}
        endif

        if !executable(bin) || a:updateBinaries == 1
            if a:updateBinaries == 1
                echo "vim-go: Updating ". basename .". Reinstalling ". pkg . " to folder " . go_bin_path
            else
                echo "vim-go: ". basename ." not found. Installing ". pkg . " to folder " . go_bin_path
            endif


            let out = go#util#System(cmd . shellescape(pkg))
            if go#util#ShellError() != 0
                echo "Error installing ". pkg . ": " . out
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

augroup vim-go
    autocmd!

    " GoInfo automatic update
    if get(g:, "go_auto_type_info", 0)
        autocmd CursorHold *.go nested call go#complete#Info(1)
    endif

    " Echo the identifier information when completion is done. Useful to see
    " the signature of a function, etc...
    if exists('##CompleteDone')
        autocmd CompleteDone *.go nested call s:echo_go_info()
    endif

    " Go code formatting on save
    if get(g:, "go_fmt_autosave", 1)
        autocmd BufWritePre *.go call go#fmt#Format(-1)
    endif

    " Go asm formatting on save
    if get(g:, "go_asmfmt_autosave", 1)
        autocmd BufWritePre *.s call go#asmfmt#Format()
    endif

    " run gometalinter on save
    if get(g:, "go_metalinter_autosave", 0)
        autocmd BufWritePost *.go call go#lint#Gometa(1)
    endif
augroup END


" vim:ts=4:sw=4:et
