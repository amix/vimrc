" initial_go_path is used to store the initial GOPATH that was set when Vim
" was started. It's used with :GoPathClear to restore the GOPATH when the user
" changed it explicitly via :GoPath. Initially it's empty. It's being set when
" :GoPath is used
let s:initial_go_path = ""

" GoPath sets or returns the current GOPATH. If no arguments are passed it
" echoes the current GOPATH, if an argument is passed it replaces the current
" GOPATH with it. If two double quotes are passed (the empty string in go),
" it'll clear the GOPATH and will restore to the initial GOPATH.
function! go#path#GoPath(...)
    " we have an argument, replace GOPATH
    if len(a:000)
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
        return
    endif

    echo go#path#Detect()
endfunction

" Default returns the default GOPATH. If there is a single GOPATH it returns
" it. For multiple GOPATHS separated with a the OS specific separator, only
" the first one is returned
function! go#path#Default()
    let go_paths = split($GOPATH, go#util#PathListSep())

    if len(go_paths) == 1
        return $GOPATH
    endif

    return go_paths[0]
endfunction

" HasPath checks whether the given path exists in GOPATH environment variable
" or not
function! go#path#HasPath(path)
    let go_paths = split($GOPATH, go#util#PathListSep())
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
function! go#path#Detect()
    let gopath = $GOPATH

    " don't lookup for godeps if autodetect is disabled.
    if !get(g:, "go_autodetect_gopath", 1)
        return gopath
    endif

    let current_dir = fnameescape(expand('%:p:h'))

    " TODO(arslan): this should be changed so folders or files should be
    " fetched from a customizable list. The user should define any new package
    " management tool by it's own.

    " src folder outside $GOPATH
    let src_root = finddir("src", current_dir .";")
    if !empty(src_root)
        let src_path = fnamemodify(src_root, ':p:h:h') . go#util#PathSep()

        " gb vendor plugin
        " (https://github.com/constabulary/gb/tree/master/cmd/gb-vendor)
        let gb_vendor_root = src_path . "vendor" . go#util#PathSep()
        if isdirectory(gb_vendor_root) && !go#path#HasPath(gb_vendor_root)
            let gopath = gb_vendor_root . go#util#PathListSep() . gopath
        endif

        if !go#path#HasPath(src_path)
            let gopath =  src_path . go#util#PathListSep() . gopath
        endif
    endif

    " Godeps
    let godeps_root = finddir("Godeps", current_dir .";")
    if !empty(godeps_root)
        let godeps_path = join([fnamemodify(godeps_root, ':p:h:h'), "Godeps", "_workspace" ], go#util#PathSep())

        if !go#path#HasPath(godeps_path)
            let gopath =  godeps_path . go#util#PathListSep() . gopath
        endif
    endif

    return gopath
endfunction


" BinPath returns the binary path of installed go tools.
function! go#path#BinPath()
    let bin_path = ""

    " check if our global custom path is set, if not check if $GOBIN is set so
    " we can use it, otherwise use $GOPATH + '/bin'
    if exists("g:go_bin_path")
        let bin_path = g:go_bin_path
    elseif $GOBIN != ""
        let bin_path = $GOBIN
    elseif $GOPATH != ""
        let bin_path = expand(go#path#Default() . "/bin/")
    else
        " could not find anything
    endif

    return bin_path
endfunction

" CheckBinPath checks whether the given binary exists or not and returns the
" path of the binary. It returns an empty string doesn't exists.
function! go#path#CheckBinPath(binpath)
    " remove whitespaces if user applied something like 'goimports   '
    let binpath = substitute(a:binpath, '^\s*\(.\{-}\)\s*$', '\1', '')

    " if it's in PATH just return it
    if executable(binpath) 
        return binpath
    endif

    " just get the basename
    let basename = fnamemodify(binpath, ":t")

    " check if we have an appropriate bin_path
    let go_bin_path = go#path#BinPath()
    if empty(go_bin_path)
        echo "vim-go: could not find '" . basename . "'. Run :GoInstallBinaries to fix it."
        return ""
    endif

    " append our GOBIN and GOPATH paths and be sure they can be found there...
    " let us search in our GOBIN and GOPATH paths
    let old_path = $PATH
    let $PATH = $PATH . go#util#PathListSep() .go_bin_path

    if !executable(basename)
        echo "vim-go: could not find '" . basename . "'. Run :GoInstallBinaries to fix it."
        " restore back!
        let $PATH = old_path
        return ""
    endif

    let $PATH = old_path

    return go_bin_path . go#util#PathSep() . basename
endfunction

" vim:ts=4:sw=4:et
