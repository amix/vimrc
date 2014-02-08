if exists("g:loaded_syntastic_registry")
    finish
endif
let g:loaded_syntastic_registry = 1

let s:defaultCheckers = {
        \ 'actionscript':['mxmlc'],
        \ 'ada':         ['gcc'],
        \ 'applescript': ['osacompile'],
        \ 'asciidoc':    ['asciidoc'],
        \ 'asm':         ['gcc'],
        \ 'bemhtml':     ['bemhtmllint'],
        \ 'c':           ['gcc'],
        \ 'chef':        ['foodcritic'],
        \ 'co':          ['coco'],
        \ 'cobol':       ['cobc'],
        \ 'coffee':      ['coffee', 'coffeelint'],
        \ 'coq':         ['coqtop'],
        \ 'cpp':         ['gcc'],
        \ 'cs':          ['mcs'],
        \ 'css':         ['csslint', 'phpcs'],
        \ 'cucumber':    ['cucumber'],
        \ 'cuda':        ['nvcc'],
        \ 'd':           ['dmd'],
        \ 'dart':        ['dartanalyzer'],
        \ 'docbk':       ['xmllint'],
        \ 'dustjs':      ['swiffer'],
        \ 'elixir':      ['elixir'],
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
        \ 'perl':        ['perl', 'perlcritic'],
        \ 'php':         ['php', 'phpcs', 'phpmd'],
        \ 'po':          ['msgfmt'],
        \ 'pod':         ['podchecker'],
        \ 'puppet':      ['puppet', 'puppetlint'],
        \ 'python':      ['python', 'flake8', 'pylint'],
        \ 'racket':      ['racket'],
        \ 'rst':         ['rst2pseudoxml'],
        \ 'ruby':        ['mri'],
        \ 'rust':        ['rustc'],
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

let s:defaultFiletypeMap = {
        \ 'gentoo-metadata': 'xml',
        \ 'lhaskell': 'haskell',
        \ 'litcoffee': 'coffee'
    \ }

let g:SyntasticRegistry = {}

" TODO: Handling of filetype aliases: all public methods take aliases as
" parameters, all private methods take normalized filetypes.  Public methods
" are thus supposed to normalize filetypes before calling private methods.

" Public methods {{{1

function! g:SyntasticRegistry.Instance()
    if !exists('s:SyntasticRegistryInstance')
        let s:SyntasticRegistryInstance = copy(self)
        let s:SyntasticRegistryInstance._checkerMap = {}
        let s:SyntasticRegistryInstance._cachedCheckersFor = {}
    endif

    return s:SyntasticRegistryInstance
endfunction

function! g:SyntasticRegistry.CreateAndRegisterChecker(args)
    let checker = g:SyntasticChecker.New(a:args)
    let registry = g:SyntasticRegistry.Instance()
    call registry._registerChecker(checker)
endfunction

function! g:SyntasticRegistry.checkable(ftalias)
    return !empty(self.getActiveCheckers(a:ftalias))
endfunction

function! g:SyntasticRegistry.getActiveCheckers(ftalias)
    let filetype = s:SyntasticRegistryNormaliseFiletype(a:ftalias)
    let checkers = self.availableCheckersFor(a:ftalias)

    if self._userHasFiletypeSettings(filetype)
        return self._filterCheckersByUserSettings(checkers, filetype)
    endif

    if has_key(s:defaultCheckers, filetype)
        return self._filterCheckersByDefaultSettings(checkers, filetype)
    endif

    return checkers[0:0]
endfunction

function! g:SyntasticRegistry.getCheckers(ftalias, list)
    return self._filterCheckersByName(self.availableCheckersFor(a:ftalias), a:list)
endfunction

function! g:SyntasticRegistry.availableCheckersFor(ftalias)
    if !has_key(self._cachedCheckersFor, a:ftalias)
        let filetype = s:SyntasticRegistryNormaliseFiletype(a:ftalias)
        let checkers = self._allCheckersFor(filetype)
        let self._cachedCheckersFor[a:ftalias] = self._filterCheckersByAvailability(checkers)
    endif

    return self._cachedCheckersFor[a:ftalias]
endfunction

function! g:SyntasticRegistry.knownFiletypes()
    let types = keys(s:defaultCheckers)
    call extend(types, keys(s:defaultFiletypeMap))
    if exists('g:syntastic_filetype_map')
        call extend(types, keys(g:syntastic_filetype_map))
    endif
    if exists('g:syntastic_extra_filetypes') && type(g:syntastic_extra_filetypes) == type([])
        call extend(types, g:syntastic_extra_filetypes)
    endif
    return syntastic#util#unique(types)
endfunction

function! g:SyntasticRegistry.echoInfoFor(ftalias_list)
    echomsg "Syntastic info for filetype: " . join(a:ftalias_list, '.')

    let available = []
    let active = []
    for ftalias in a:ftalias_list
        call extend(available, self.availableCheckersFor(ftalias))
        call extend(active, self.getActiveCheckers(ftalias))
    endfor

    echomsg "Available checker(s): " . join(syntastic#util#unique(map(available, "v:val.getName()")))
    echomsg "Currently enabled checker(s): " . join(syntastic#util#unique(map(active, "v:val.getName()")))
endfunction

" Private methods {{{1

function! g:SyntasticRegistry._registerChecker(checker) abort
    let ft = a:checker.getFiletype()

    if !has_key(self._checkerMap, ft)
        let self._checkerMap[ft] = []
    endif

    call self._validateUniqueName(a:checker)

    call add(self._checkerMap[ft], a:checker)
endfunction

function! g:SyntasticRegistry._allCheckersFor(filetype)
    call self._loadCheckers(a:filetype)
    if empty(self._checkerMap[a:filetype])
        return []
    endif

    return self._checkerMap[a:filetype]
endfunction

function! g:SyntasticRegistry._filterCheckersByDefaultSettings(checkers, filetype)
    if has_key(s:defaultCheckers, a:filetype)
        return self._filterCheckersByName(a:checkers, s:defaultCheckers[a:filetype])
    endif

    return a:checkers
endfunction

function! g:SyntasticRegistry._filterCheckersByUserSettings(checkers, filetype)
    if exists("b:syntastic_checkers")
        let whitelist = b:syntastic_checkers
    else
        let whitelist = g:syntastic_{a:filetype}_checkers
    endif
    return self._filterCheckersByName(a:checkers, whitelist)
endfunction

function! g:SyntasticRegistry._filterCheckersByName(checkers, list)
    let checkers_by_name = {}
    for c in a:checkers
        let checkers_by_name[c.getName()] = c
    endfor

    let filtered = []
    for name in a:list
        if has_key(checkers_by_name, name)
            call add(filtered, checkers_by_name[name])
        endif
    endfor

    return filtered
endfunction

function! g:SyntasticRegistry._filterCheckersByAvailability(checkers)
    return filter(copy(a:checkers), "v:val.isAvailable()")
endfunction

function! g:SyntasticRegistry._loadCheckers(filetype)
    if self._haveLoadedCheckers(a:filetype)
        return
    endif

    execute "runtime! syntax_checkers/" . a:filetype . "/*.vim"

    if !has_key(self._checkerMap, a:filetype)
        let self._checkerMap[a:filetype] = []
    endif
endfunction

function! g:SyntasticRegistry._haveLoadedCheckers(filetype)
    return has_key(self._checkerMap, a:filetype)
endfunction

function! g:SyntasticRegistry._userHasFiletypeSettings(filetype)
    if exists("g:syntastic_" . a:filetype . "_checker") && !exists("g:syntastic_" . a:filetype . "_checkers")
        let g:syntastic_{a:filetype}_checkers = [g:syntastic_{a:filetype}_checker]
        call syntastic#log#deprecationWarn("variable g:syntastic_" . a:filetype . "_checker is deprecated")
    endif
    return exists("b:syntastic_checkers") || exists("g:syntastic_" . a:filetype . "_checkers")
endfunction

function! g:SyntasticRegistry._validateUniqueName(checker) abort
    for checker in self._allCheckersFor(a:checker.getFiletype())
        if checker.getName() ==# a:checker.getName()
            throw "Syntastic: Duplicate syntax checker name for: " . a:checker.getName()
        endif
    endfor
endfunction

" Private functions {{{1

"resolve filetype aliases, and replace - with _ otherwise we cant name
"syntax checker functions legally for filetypes like "gentoo-metadata"
function! s:SyntasticRegistryNormaliseFiletype(ftalias)
    let ft = get(s:defaultFiletypeMap, a:ftalias, a:ftalias)
    let ft = get(g:syntastic_filetype_map, ft, ft)
    let ft = substitute(ft, '\m-', '_', 'g')
    return ft
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
