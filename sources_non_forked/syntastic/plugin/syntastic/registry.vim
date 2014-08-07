if exists("g:loaded_syntastic_registry") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_registry = 1

" Initialisation {{{1

let s:defaultCheckers = {
        \ 'actionscript':['mxmlc'],
        \ 'ada':         ['gcc'],
        \ 'applescript': ['osacompile'],
        \ 'arduino':     ['avrgcc'],
        \ 'asciidoc':    ['asciidoc'],
        \ 'asm':         ['gcc'],
        \ 'bro':         ['bro'],
        \ 'bemhtml':     ['bemhtmllint'],
        \ 'c':           ['gcc'],
        \ 'cabal':       ['cabal'],
        \ 'chef':        ['foodcritic'],
        \ 'co':          ['coco'],
        \ 'cobol':       ['cobc'],
        \ 'coffee':      ['coffee', 'coffeelint'],
        \ 'coq':         ['coqtop'],
        \ 'cpp':         ['gcc'],
        \ 'cs':          ['mcs'],
        \ 'css':         ['csslint'],
        \ 'cucumber':    ['cucumber'],
        \ 'cuda':        ['nvcc'],
        \ 'd':           ['dmd'],
        \ 'dart':        ['dartanalyzer'],
        \ 'docbk':       ['xmllint'],
        \ 'dustjs':      ['swiffer'],
        \ 'elixir':      [],
        \ 'erlang':      ['escript'],
        \ 'eruby':       ['ruby'],
        \ 'fortran':     ['gfortran'],
        \ 'glsl':        ['cgc'],
        \ 'go':          ['go'],
        \ 'haml':        ['haml'],
        \ 'handlebars':  ['handlebars'],
        \ 'haskell':     ['ghc_mod', 'hdevtools', 'hlint'],
        \ 'haxe':        ['haxe'],
        \ 'hss':         ['hss'],
        \ 'html':        ['tidy'],
        \ 'java':        ['javac'],
        \ 'javascript':  ['jshint', 'jslint'],
        \ 'json':        ['jsonlint', 'jsonval'],
        \ 'less':        ['lessc'],
        \ 'lex':         ['flex'],
        \ 'limbo':       ['limbo'],
        \ 'lisp':        ['clisp'],
        \ 'llvm':        ['llvm'],
        \ 'lua':         ['luac'],
        \ 'matlab':      ['mlint'],
        \ 'nasm':        ['nasm'],
        \ 'nroff':       ['mandoc'],
        \ 'objc':        ['gcc'],
        \ 'objcpp':      ['gcc'],
        \ 'ocaml':       ['camlp4o'],
        \ 'perl':        ['perlcritic'],
        \ 'php':         ['php', 'phpcs', 'phpmd'],
        \ 'po':          ['msgfmt'],
        \ 'pod':         ['podchecker'],
        \ 'puppet':      ['puppet', 'puppetlint'],
        \ 'python':      ['python', 'flake8', 'pylint'],
        \ 'r':           [],
        \ 'racket':      ['racket'],
        \ 'rst':         ['rst2pseudoxml'],
        \ 'ruby':        ['mri'],
        \ 'sass':        ['sass'],
        \ 'scala':       ['fsc', 'scalac'],
        \ 'scss':        ['sass', 'scss_lint'],
        \ 'sh':          ['sh', 'shellcheck'],
        \ 'slim':        ['slimrb'],
        \ 'tcl':         ['nagelfar'],
        \ 'tex':         ['lacheck', 'chktex'],
        \ 'texinfo':     ['makeinfo'],
        \ 'text':        ['atdtool'],
        \ 'twig':        ['twiglint'],
        \ 'typescript':  ['tsc'],
        \ 'vala':        ['valac'],
        \ 'verilog':     ['verilator'],
        \ 'vhdl':        ['ghdl'],
        \ 'vim':         ['vimlint'],
        \ 'xhtml':       ['tidy'],
        \ 'xml':         ['xmllint'],
        \ 'xslt':        ['xmllint'],
        \ 'yacc':        ['bison'],
        \ 'yaml':        ['jsyaml'],
        \ 'z80':         ['z80syntaxchecker'],
        \ 'zpt':         ['zptlint'],
        \ 'zsh':         ['zsh', 'shellcheck']
    \ }
lockvar! s:defaultCheckers

let s:defaultFiletypeMap = {
        \ 'gentoo-metadata': 'xml',
        \ 'lhaskell': 'haskell',
        \ 'litcoffee': 'coffee'
    \ }
lockvar! s:defaultFiletypeMap

let g:SyntasticRegistry = {}

" }}}1

" Public methods {{{1

" Note: Handling of filetype aliases: all public methods take aliases as
" parameters, all private methods take normalized filetypes.  Public methods
" are thus supposed to normalize filetypes before calling private methods.

function! g:SyntasticRegistry.Instance() " {{{2
    if !exists('s:SyntasticRegistryInstance')
        let s:SyntasticRegistryInstance = copy(self)
        let s:SyntasticRegistryInstance._checkerMap = {}
    endif

    return s:SyntasticRegistryInstance
endfunction " }}}2

function! g:SyntasticRegistry.CreateAndRegisterChecker(args) " {{{2
    let checker = g:SyntasticChecker.New(a:args)
    let registry = g:SyntasticRegistry.Instance()
    call registry._registerChecker(checker)
endfunction " }}}2

" Given a list of checker names hints_list, return a map name --> checker.
" If hints_list is empty, user settings are are used instead. Checkers are
" not checked for availability (that is, the corresponding IsAvailable() are
" not run).
function! g:SyntasticRegistry.getCheckers(ftalias, hints_list) " {{{2
    let ft = s:normaliseFiletype(a:ftalias)
    call self._loadCheckersFor(ft)

    let checkers_map = self._checkerMap[ft]
    if empty(checkers_map)
        return []
    endif

    call self._checkDeprecation(ft)

    let names =
        \ !empty(a:hints_list) ? syntastic#util#unique(a:hints_list) :
        \ exists('b:syntastic_checkers') ? b:syntastic_checkers :
        \ exists('g:syntastic_' . ft . '_checkers') ? g:syntastic_{ft}_checkers :
        \ get(s:defaultCheckers, ft, 0)

    return type(names) == type([]) ?
        \ self._filterCheckersByName(checkers_map, names) : [checkers_map[keys(checkers_map)[0]]]
endfunction " }}}2

" Same as getCheckers(), but keep only the checkers available.  This runs the
" corresponding IsAvailable() functions for all checkers.
function! g:SyntasticRegistry.getCheckersAvailable(ftalias, hints_list) " {{{2
    return filter(self.getCheckers(a:ftalias, a:hints_list), 'v:val.isAvailable()')
endfunction " }}}2

function! g:SyntasticRegistry.getKnownFiletypes() " {{{2
    let types = keys(s:defaultCheckers)

    call extend(types, keys(s:defaultFiletypeMap))

    if exists('g:syntastic_filetype_map')
        call extend(types, keys(g:syntastic_filetype_map))
    endif

    if exists('g:syntastic_extra_filetypes') && type(g:syntastic_extra_filetypes) == type([])
        call extend(types, g:syntastic_extra_filetypes)
    endif

    return syntastic#util#unique(types)
endfunction " }}}2

function! g:SyntasticRegistry.getNamesOfAvailableCheckers(ftalias) " {{{2
    let ft = s:normaliseFiletype(a:ftalias)
    call self._loadCheckersFor(ft)
    return keys(filter( copy(self._checkerMap[ft]), 'v:val.isAvailable()' ))
endfunction " }}}2

function! g:SyntasticRegistry.echoInfoFor(ftalias_list) " {{{2
    let ft_list = syntastic#util#unique(map( copy(a:ftalias_list), 's:normaliseFiletype(v:val)' ))
    if len(ft_list) != 1
        let available = []
        let active = []

        for ft in ft_list
            call extend(available, map( self.getNamesOfAvailableCheckers(ft), 'ft . "/" . v:val' ))
            call extend(active, map( self.getCheckersAvailable(ft, []), 'ft . "/" . v:val.getName()' ))
        endfor
    else
        let ft = ft_list[0]
        let available = self.getNamesOfAvailableCheckers(ft)
        let active = map(self.getCheckersAvailable(ft, []), 'v:val.getName()')
    endif

    let cnt = len(available)
    let plural = cnt != 1 ? 's' : ''
    let cklist = cnt ? join(sort(available)) : '-'
    echomsg 'Available checker' . plural . ': ' . cklist

    let cnt = len(active)
    let plural = cnt != 1 ? 's' : ''
    let cklist = cnt ? join(active) : '-'
    echomsg 'Currently enabled checker' . plural . ': ' . cklist
endfunction " }}}2

" }}}1

" Private methods {{{1

function! g:SyntasticRegistry._registerChecker(checker) abort " {{{2
    let ft = a:checker.getFiletype()
    if !has_key(self._checkerMap, ft)
        let self._checkerMap[ft] = {}
    endif

    let name = a:checker.getName()
    if has_key(self._checkerMap[ft], name)
        throw 'Syntastic: Duplicate syntax checker name: ' . ft . '/' . name
    endif

    let self._checkerMap[ft][name] = a:checker
endfunction " }}}2

function! g:SyntasticRegistry._filterCheckersByName(checkers_map, list) " {{{2
    return filter( map(copy(a:list), 'get(a:checkers_map, v:val, {})'), '!empty(v:val)' )
endfunction " }}}2

function! g:SyntasticRegistry._loadCheckersFor(filetype) " {{{2
    if has_key(self._checkerMap, a:filetype)
        return
    endif

    execute "runtime! syntax_checkers/" . a:filetype . "/*.vim"

    if !has_key(self._checkerMap, a:filetype)
        let self._checkerMap[a:filetype] = {}
    endif
endfunction " }}}2

" Check for obsolete variable g:syntastic_<filetype>_checker
function! g:SyntasticRegistry._checkDeprecation(filetype) " {{{2
    if exists('g:syntastic_' . a:filetype . '_checker') && !exists('g:syntastic_' . a:filetype . '_checkers')
        let g:syntastic_{a:filetype}_checkers = [g:syntastic_{a:filetype}_checker]
        call syntastic#log#oneTimeWarn('variable g:syntastic_' . a:filetype . '_checker is deprecated')
    endif
endfunction " }}}2

" }}}1

" Private functions {{{1

"resolve filetype aliases, and replace - with _ otherwise we cant name
"syntax checker functions legally for filetypes like "gentoo-metadata"
function! s:normaliseFiletype(ftalias) " {{{2
    let ft = get(s:defaultFiletypeMap, a:ftalias, a:ftalias)
    let ft = get(g:syntastic_filetype_map, ft, ft)
    let ft = substitute(ft, '\m-', '_', 'g')
    return ft
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
