"
" Python filetype plugin for running pyflakes
" Language:     Python (ft=python)
" Maintainer:   Vincent Driessen <vincent@datafox.nl>
" Version:      Vim 7 (may work with lower Vim versions, but not tested)
" URL:          http://github.com/nvie/vim-pyflakes
"
" Only do this when not done yet for this buffer
if exists("b:loaded_pyflakes_ftplugin")
    finish
endif
let b:loaded_pyflakes_ftplugin=1

let s:pyflakes_cmd="pyflakes"

if !exists("*Pyflakes()")
    function Pyflakes()
        if !executable(s:pyflakes_cmd)
            echoerr "File " . s:pyflakes_cmd . " not found. Please install it first."
            return
        endif

        set lazyredraw   " delay redrawing
        cclose           " close any existing cwindows

        " store old grep settings (to restore later)
        let l:old_gfm=&grepformat
        let l:old_gp=&grepprg

        " write any changes before continuing
        if &readonly == 0
            update
        endif

        " perform the grep itself
        let &grepformat="%f:%l: %m"
        let &grepprg=s:pyflakes_cmd
        silent! grep! %

        " restore grep settings
        let &grepformat=l:old_gfm
        let &grepprg=l:old_gp

        " open cwindow
        let has_results=getqflist() != []
        if has_results
            execute 'belowright copen'
            nnoremap <buffer> <silent> c :cclose<CR>
            nnoremap <buffer> <silent> q :cclose<CR>
        endif

        set nolazyredraw
        redraw!

        if has_results == 0
            " Show OK status
            hi Green ctermfg=green
            echohl Green
            echon "Static analysis OK"
            echohl
        endif
    endfunction
endif

" Add mappings, unless the user didn't want this.
" The default mapping is registered under to <F7> by default, unless the user
" remapped it already (or a mapping exists already for <F7>)
if !exists("no_plugin_maps") && !exists("no_pyflakes_maps")
    if !hasmapto('Pyflakes()')
        noremap <buffer> <F7> :call Pyflakes()<CR>
        noremap! <buffer> <F7> :call Pyflakes()<CR>
    endif
endif
