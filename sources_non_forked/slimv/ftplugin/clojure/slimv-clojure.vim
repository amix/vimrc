" slimv-clojure.vim:
"               Clojure filetype plugin for Slimv
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
if exists("b:slimv_did_ftplugin") || exists("g:slimv_disable_clojure")
    finish
endif

" ---------- Begin part loaded once ----------
if !exists( 'g:slimv_clojure_loaded' )

let g:slimv_clojure_loaded = 1

" Transform filename so that it will not contain spaces
function! s:TransformFilename( name )
    if match( a:name, ' ' ) >= 0
        return fnamemodify( a:name , ':8' )
    else
        return a:name
    endif
endfunction

" Build a Clojure startup command by adding
" all clojure*.jar files found to the classpath
function! s:BuildStartCmd( lisps )
    let cp = s:TransformFilename( a:lisps[0] )
    let cp_delim = g:slimv_windows ? ';' : ':'
    let i = 1
    while i < len( a:lisps )
        let cp = cp . cp_delim . s:TransformFilename( a:lisps[i] )
        let i = i + 1
    endwhile

    " Try to find swank-clojure and add it to classpath
    let swanks = split( globpath( &runtimepath, 'swank-clojure'), '\n' )
    if len( swanks ) > 0
        let cp = cp . cp_delim . s:TransformFilename( swanks[0] )
    endif
    return ['java -cp ' . cp . ' clojure.main', 'clojure']
endfunction

" Try to autodetect Clojure executable
" Returns list [Clojure executable, Clojure implementation]
function! SlimvAutodetect( preferred )
    " Firts try the most basic setup: everything in the path
    if executable( 'lein' )
        return ['"lein repl"', 'clojure']
    endif
    if executable( 'cake' )
        return ['"cake repl"', 'clojure']
    endif
    if executable( 'clojure' )
        return ['clojure', 'clojure']
    endif
    let lisps = []
    if executable( 'clojure.jar' )
        let lisps = ['clojure.jar']
    endif
    if executable( 'clojure-contrib.jar' )
        let lisps = lisps + 'clojure-contrib.jar'
    endif
    if len( lisps ) > 0
        return s:BuildStartCmd( lisps )
    endif

    " Check if Clojure is bundled with Slimv
    let lisps = split( globpath( &runtimepath, 'swank-clojure/clojure*.jar'), '\n' )
    if len( lisps ) > 0
        return s:BuildStartCmd( lisps )
    endif

    " Try to find Clojure in the PATH
    let path_delim = g:slimv_windows ? ';' : ':'
    let path = substitute( $PATH, path_delim, ',', 'g' )
    let lisps = split( globpath( path, 'clojure*.jar' ), '\n' )
    if len( lisps ) > 0
        return s:BuildStartCmd( lisps )
    endif

    if g:slimv_windows
        " Try to find Clojure on the standard installation places
        let lisps = split( globpath( 'c:/*clojure*,c:/*clojure*/lib', 'clojure*.jar' ), '\n' )
        if len( lisps ) > 0
            return s:BuildStartCmd( lisps )
        endif
    else
        " Try to find Clojure in the home directory
        let lisps = split( globpath( '/usr/local/bin/*clojure*', 'clojure*.jar' ), '\n' )
        if len( lisps ) > 0
            return s:BuildStartCmd( lisps )
        endif
        let lisps = split( globpath( '~/*clojure*', 'clojure*.jar' ), '\n' )
        if len( lisps ) > 0
            return s:BuildStartCmd( lisps )
        endif
    endif

    return ['', '']
endfunction

" Try to find out the Clojure implementation
function! SlimvImplementation()
    if exists( 'g:slimv_impl' ) && g:slimv_impl != ''
        " Return Lisp implementation if defined
        return tolower( g:slimv_impl )
    endif

    return 'clojure'
endfunction

" Try to autodetect SWANK and build the command to load the SWANK server
function! SlimvSwankLoader()
    " First autodetect Leiningen and Cake
    if executable( 'lein' )
        if globpath( '~/.lein/plugins', 'lein-ritz*.jar' ) != ''
            return '"lein ritz ' . g:swank_port . '"'
        else
            return '"lein swank"'
        endif
    elseif executable( 'cake' )
        return '"cake swank"'
    else
        " Check if swank-clojure is bundled with Slimv
        let swanks = split( globpath( &runtimepath, 'swank-clojure/swank/swank.clj'), '\n' )
        if len( swanks ) == 0
            return ''
        endif
        let sclj = substitute( swanks[0], '\', '/', "g" )
        return g:slimv_lisp . ' -i "' . sclj . '" -e "(swank.swank/start-repl)" -r'
    endif
endfunction

" Filetype specific initialization for the REPL buffer
function! SlimvInitRepl()
    set filetype=clojure
endfunction

" Lookup symbol in the list of Clojure Hyperspec symbol databases
function! SlimvHyperspecLookup( word, exact, all )
    if !exists( 'g:slimv_cljapi_loaded' )
        runtime ftplugin/**/slimv-cljapi.vim
    endif

    if !exists( 'g:slimv_javadoc_loaded' )
        runtime ftplugin/**/slimv-javadoc.vim
    endif

    let symbol = []
    if exists( 'g:slimv_cljapi_db' )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_cljapi_db,  g:slimv_cljapi_root,  symbol )
    endif
    if exists( 'g:slimv_javadoc_db' )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_javadoc_db, g:slimv_javadoc_root, symbol )
    endif
    if exists( 'g:slimv_cljapi_user_db' )
        " Give a choice for the user to extend the symbol database
        if exists( 'g:slimv_cljapi_user_root' )
            let user_root = g:slimv_cljapi_user_root
        else
            let user_root = ''
        endif
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_cljapi_user_db, user_root, symbol )
    endif
    return symbol
endfunction

" Implementation specific REPL initialization
function! SlimvReplInit( lisp_version )
    " Import functions commonly used in REPL but not present when not running in repl mode
    if a:lisp_version[0:2] >= '1.3'
        call SlimvSendSilent( ["(use '[clojure.repl :only (source apropos dir pst doc find-doc)])",
        \                      "(use '[clojure.java.javadoc :only (javadoc)])",
        \                      "(use '[clojure.pprint :only (pp pprint)])"] )
    elseif a:lisp_version[0:2] >= '1.2'
        call SlimvSendSilent( ["(use '[clojure.repl :only (source apropos)])",
        \                      "(use '[clojure.java.javadoc :only (javadoc)])",
        \                      "(use '[clojure.pprint :only (pp pprint)])"] )
    endif
endfunction

" Source Slimv general part
runtime ftplugin/**/slimv.vim

endif "!exists( 'g:slimv_clojure_loaded' )
" ---------- End of part loaded once ----------

runtime ftplugin/**/lisp.vim

" Must be called for each lisp buffer
call SlimvInitBuffer()

" Don't initiate Slimv again for this buffer
let b:slimv_did_ftplugin = 1

