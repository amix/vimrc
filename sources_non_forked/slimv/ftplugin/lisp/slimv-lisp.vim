" slimv-lisp.vim:
"               Lisp filetype plugin for Slimv
" Version:      0.9.13
" Last Change:  04 May 2014
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:did_ftplugin") || exists("g:slimv_disable_lisp")
    finish
endif

" Handle cases when lisp dialects explicitly use the lisp filetype plugins
if &ft == "clojure" && exists("g:slimv_disable_clojure")
    finish
endif

if &ft == "scheme" && exists("g:slimv_disable_scheme")
    finish
endif

" ---------- Begin part loaded once ----------
if !exists( 'g:slimv_lisp_loaded' )

let g:slimv_lisp_loaded = 1

" Descriptor array for various lisp implementations
" The structure of an array element is:
"     [ executable, implementation, platform, search path]
" where:
"     executable  - may contain wildcards but only if a search path is present
"     platform    - 'w' (Windows) or 'l' (Linux = non-Windows), '' for all
"     search path - commma separated list, may contain wildcard characters
let s:lisp_desc = [
\ [ 'sbcl',         'sbcl',      '',    '' ],
\ [ 'clisp',        'clisp',     '',    '' ],
\ [ 'gcl',          'clisp',     '',    '' ],
\ [ 'cmucl',        'cmu',       '',    '' ],
\ [ 'ecl',          'ecl',       '',    '' ],
\ [ 'acl',          'allegro',   '',    '' ],
\ [ 'mlisp',        'allegro',   '',    '' ],
\ [ 'mlisp8',       'allegro',   '',    '' ],
\ [ 'alisp',        'allegro',   '',    '' ],
\ [ 'alisp8',       'allegro',   '',    '' ],
\ [ 'lwl',          'lispworks', '',    '' ],
\ [ 'ccl',          'clozure',   '',    '' ],
\ [ 'wx86cl64',     'clozure',   'w64', '' ],
\ [ 'wx86cl',       'clozure',   'w',   '' ],
\ [ 'lx86cl',       'clozure',   'l',   '' ],
\ [ '*lisp.exe',    'clisp',     'w',
\   'c:/*lisp*,c:/*lisp*/*,c:/*lisp*/bin/*,c:/Program Files/*lisp*,c:/Program Files/*lisp*/*,c:/Program Files/*lisp*/bin/*' ],
\ [ 'gcl.exe',      'clisp',     'w',   'c:/gcl*,c:/Program Files/gcl*' ],
\ [ 'cmucl.exe',    'cmu',       'w',   'c:/cmucl*,c:/Program Files/cmucl*' ],
\ [ '*lisp*.exe',   'allegro',   'w',   'c:/acl*,c:/Program Files/acl*,c:/Program Files/*lisp*/bin/acl*' ],
\ [ 'ecl.exe',      'ecl',       'w',   'c:/ecl*,c:/Program Files/ecl*' ],
\ [ 'wx86cl64.exe', 'clozure',   'w64', 'c:/ccl*,c:/Program Files/ccl*,c:/Program Files/*lisp*/bin/ccl*' ],
\ [ 'wx86cl.exe',   'clozure',   'w',   'c:/ccl*,c:/Program Files/ccl*,c:/Program Files/*lisp*/bin/ccl*' ],
\ [ 'sbcl.exe',     'sbcl',      'w',   'c:/sbcl*,c:/Program Files/sbcl*,c:/Program Files/*lisp*/bin/sbcl*'] ]

" Try to autodetect Lisp executable
" Returns list [Lisp executable, Lisp implementation]
function! SlimvAutodetect( preferred )
    for lisp in s:lisp_desc
        if     lisp[2] =~ 'w' && !g:slimv_windows
            " Valid only on Windows
        elseif lisp[2] == 'w64' && $ProgramW6432 == ''
            " Valid only on 64 bit Windows
        elseif lisp[2] == 'l' &&  g:slimv_windows
            " Valid only on Linux
        elseif a:preferred != '' && a:preferred != lisp[1]
            " Not the preferred implementation
        elseif lisp[3] != ''
            " A search path is given
            let lisps = split( globpath( lisp[3], lisp[0] ), '\n' )
            if len( lisps ) > 0
                return [lisps[0], lisp[1]]
            endif
        else
            " Single executable is given without path
            if executable( lisp[0] )
                return lisp[0:1]
            endif
        endif
    endfor
    return ['', '']
endfunction

" Try to find out the Lisp implementation
function! SlimvImplementation()
    if exists( 'g:slimv_impl' ) && g:slimv_impl != ''
        " Return Lisp implementation if defined
        return tolower( g:slimv_impl )
    endif

    let lisp = tolower( g:slimv_lisp )
    if match( lisp, 'sbcl' ) >= 0
        return 'sbcl'
    endif
    if match( lisp, 'cmu' ) >= 0
        return 'cmu'
    endif
    if match( lisp, 'acl' ) >= 0 || match( lisp, 'alisp' ) >= 0 || match( lisp, 'mlisp' ) >= 0
        return 'allegro'
    endif
    if match( lisp, 'ecl' ) >= 0
        return 'ecl'
    endif
    if match( lisp, 'x86cl' ) >= 0
        return 'clozure'
    endif
    if match( lisp, 'lwl' ) >= 0
        return 'lispworks'
    endif

    return 'clisp'
endfunction

" Try to autodetect SWANK and build the command to load the SWANK server
function! SlimvSwankLoader()
    " First check if SWANK is bundled with Slimv
    let swanks = split( globpath( &runtimepath, 'slime/start-swank.lisp'), '\n' )
    if len( swanks ) == 0
        " Try to find SWANK in the standard SLIME installation locations
        if g:slimv_windows || g:slimv_cygwin
            let swanks = split( globpath( 'c:/slime/,c:/*lisp*/slime/,c:/*lisp*/site/lisp/slime/,c:/Program Files/*lisp*/site/lisp/slime/', 'start-swank.lisp' ), '\n' )
        else
            let swanks = split( globpath( '/usr/share/common-lisp/source/slime/', 'start-swank.lisp' ), '\n' )
        endif
    endif
    if len( swanks ) == 0
        return ''
    endif

    " Build proper SWANK loader command for the Lisp implementation used
    if g:slimv_impl == 'sbcl' || g:slimv_impl == 'ecl'
        return '"' . g:slimv_lisp . '" --load "' . swanks[0] . '"'
    elseif g:slimv_impl == 'clisp'
        return '"' . g:slimv_lisp . '" -i "' . swanks[0] . '"'
    elseif g:slimv_impl == 'allegro'
        return '"' . g:slimv_lisp . '" -L "' . swanks[0] . '"'
    elseif g:slimv_impl == 'cmu'
        return '"' . g:slimv_lisp . '" -load "' . swanks[0] . '"'
    else
        return '"' . g:slimv_lisp . '" -l "' . swanks[0] . '"'
    endif
endfunction

" Filetype specific initialization for the REPL buffer
function! SlimvInitRepl()
    set filetype=lisp
endfunction

" Lookup symbol in the list of Lisp Hyperspec symbol databases
function! SlimvHyperspecLookup( word, exact, all )
    if !exists( 'g:slimv_clhs_loaded' )
        runtime ftplugin/**/slimv-clhs.vim
    endif

    let symbol = []
    if exists( 'g:slimv_clhs_loaded' )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_clhs,          g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_issues,        g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_chapters,      g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_control_chars, g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_macro_chars,   g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_loop,          g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_arguments,     g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_glossary,      g:slimv_clhs_root, symbol )
    endif
    if exists( 'g:slimv_clhs_user_db' )
        " Give a choice for the user to extend the symbol database
        if exists( 'g:slimv_clhs_user_root' )
            let user_root = g:slimv_clhs_user_root
        else
            let user_root = ''
        endif
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_clhs_user_db, user_root, symbol )
    endif
    return symbol
endfunction

" Source Slimv general part
runtime ftplugin/**/slimv.vim

endif "!exists( 'g:slimv_lisp_loaded' )
" ---------- End of part loaded once ----------

runtime ftplugin/**/lisp.vim

" Must be called for each lisp buffer
call SlimvInitBuffer()

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

