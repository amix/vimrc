if exists('g:loaded_syntastic_registry') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_registry = 1

" Initialisation {{{1

let s:_DEFAULT_CHECKERS = {
        \ 'actionscript':  ['mxmlc'],
        \ 'ada':           ['gcc'],
        \ 'ansible':       ['ansible_lint'],
        \ 'apiblueprint':  ['drafter'],
        \ 'applescript':   ['osacompile'],
        \ 'asciidoc':      ['asciidoc'],
        \ 'asm':           ['gcc'],
        \ 'bro':           ['bro'],
        \ 'bemhtml':       ['bemhtmllint'],
        \ 'c':             ['gcc'],
        \ 'cabal':         ['cabal'],
        \ 'chef':          ['foodcritic'],
        \ 'co':            ['coco'],
        \ 'cobol':         ['cobc'],
        \ 'coffee':        ['coffee', 'coffeelint'],
        \ 'coq':           ['coqtop'],
        \ 'cpp':           ['gcc'],
        \ 'cs':            ['mcs'],
        \ 'css':           ['csslint'],
        \ 'cucumber':      ['cucumber'],
        \ 'cuda':          ['nvcc'],
        \ 'd':             ['dmd'],
        \ 'dart':          ['dartanalyzer'],
        \ 'docbk':         ['xmllint'],
        \ 'dockerfile':    ['dockerfile_lint'],
        \ 'dustjs':        ['swiffer'],
        \ 'elixir':        [],
        \ 'erlang':        ['escript'],
        \ 'eruby':         ['ruby'],
        \ 'fortran':       ['gfortran'],
        \ 'glsl':          ['cgc'],
        \ 'go':            ['go'],
        \ 'haml':          ['haml'],
        \ 'handlebars':    ['handlebars'],
        \ 'haskell':       ['hdevtools', 'hlint'],
        \ 'haxe':          ['haxe'],
        \ 'hss':           ['hss'],
        \ 'html':          ['tidy'],
        \ 'jade':          ['jade_lint'],
        \ 'java':          ['javac'],
        \ 'javascript':    ['jshint', 'jslint'],
        \ 'json':          ['jsonlint', 'jsonval'],
        \ 'less':          ['lessc'],
        \ 'lex':           ['flex'],
        \ 'limbo':         ['limbo'],
        \ 'lisp':          ['clisp'],
        \ 'llvm':          ['llvm'],
        \ 'lua':           ['luac'],
        \ 'markdown':      ['mdl'],
        \ 'matlab':        ['mlint'],
        \ 'mercury':       ['mmc'],
        \ 'nasm':          ['nasm'],
        \ 'nix':           ['nix'],
        \ 'nroff':         ['mandoc'],
        \ 'objc':          ['gcc'],
        \ 'objcpp':        ['gcc'],
        \ 'ocaml':         ['camlp4o'],
        \ 'perl':          ['perlcritic'],
        \ 'php':           ['php', 'phpcs', 'phpmd'],
        \ 'po':            ['msgfmt'],
        \ 'pod':           ['podchecker'],
        \ 'puppet':        ['puppet', 'puppetlint'],
        \ 'pug':           ['pug_lint'],
        \ 'python':        ['python', 'flake8', 'pylint'],
        \ 'qml':           ['qmllint'],
        \ 'r':             [],
        \ 'rmd':           [],
        \ 'racket':        ['racket'],
        \ 'rnc':           ['rnv'],
        \ 'rst':           ['rst2pseudoxml'],
        \ 'ruby':          ['mri'],
        \ 'sass':          ['sass'],
        \ 'scala':         ['fsc', 'scalac'],
        \ 'scss':          ['sass', 'scss_lint'],
        \ 'sh':            ['sh', 'shellcheck'],
        \ 'slim':          ['slimrb'],
        \ 'sml':           ['smlnj'],
        \ 'spec':          ['rpmlint'],
        \ 'sql':           ['sqlint'],
        \ 'stylus':        ['stylint'],
        \ 'tcl':           ['nagelfar'],
        \ 'tex':           ['lacheck', 'chktex'],
        \ 'texinfo':       ['makeinfo'],
        \ 'text':          [],
        \ 'twig':          ['twiglint'],
        \ 'typescript':    ['tsc'],
        \ 'vala':          ['valac'],
        \ 'verilog':       ['verilator'],
        \ 'vhdl':          ['ghdl'],
        \ 'vim':           ['vimlint'],
        \ 'xhtml':         ['tidy'],
        \ 'xml':           ['xmllint'],
        \ 'xslt':          ['xmllint'],
        \ 'xquery':        ['basex'],
        \ 'yacc':          ['bison'],
        \ 'yaml':          ['jsyaml'],
        \ 'z80':           ['z80syntaxchecker'],
        \ 'zpt':           ['zptlint'],
        \ 'zsh':           ['zsh'],
    \ }
lockvar! s:_DEFAULT_CHECKERS

let s:_DEFAULT_FILETYPE_MAP = {
        \ 'gentoo-metadata': 'xml',
        \ 'groff': 'nroff',
        \ 'lhaskell': 'haskell',
        \ 'litcoffee': 'coffee',
        \ 'mail': 'text',
        \ 'mkd': 'markdown',
        \ 'pe-puppet': 'puppet',
        \ 'sgml': 'docbk',
        \ 'sgmllnx': 'docbk',
    \ }
lockvar! s:_DEFAULT_FILETYPE_MAP

let s:_ECLIM_TYPES = [
        \ 'c',
        \ 'cpp',
        \ 'html',
        \ 'java',
        \ 'php',
        \ 'python',
        \ 'ruby',
    \ ]
lockvar! s:_ECLIM_TYPES

let s:_YCM_TYPES = [
        \ 'c',
        \ 'cpp',
        \ 'objc',
        \ 'objcpp',
    \ ]
lockvar! s:_YCM_TYPES

let g:SyntasticRegistry = {}

" }}}1

" Public methods {{{1

" Note: Handling of filetype aliases: all public methods take aliases as
" parameters, all private methods take normalized filetypes.  Public methods
" are thus supposed to normalize filetypes before calling private methods.

function! g:SyntasticRegistry.Instance() abort " {{{2
    if !exists('s:SyntasticRegistryInstance')
        let s:SyntasticRegistryInstance = copy(self)
        let s:SyntasticRegistryInstance._checkerMap = {}
    endif

    return s:SyntasticRegistryInstance
endfunction " }}}2

function! g:SyntasticRegistry.CreateAndRegisterChecker(args) abort " {{{2
    let registry = g:SyntasticRegistry.Instance()

    if has_key(a:args, 'redirect')
        let [ft, name] = split(a:args['redirect'], '/')
        call registry._loadCheckersFor(ft)

        let clone = get(registry._checkerMap[ft], name, {})
        if empty(clone)
            throw 'Syntastic: Checker ' . a:args['redirect'] . ' redirects to unregistered checker ' . ft . '/' . name
        endif

        let checker = g:SyntasticChecker.New(a:args, clone)
    else
        let checker = g:SyntasticChecker.New(a:args)
    endif
    call registry._registerChecker(checker)
endfunction " }}}2

" Given a list of checker names hints_list, return a map name --> checker.
" If hints_list is empty, user settings are are used instead. Checkers are
" not checked for availability (that is, the corresponding IsAvailable() are
" not run).
function! g:SyntasticRegistry.getCheckers(ftalias, hints_list) abort " {{{2
    let ft = s:_normalise_filetype(a:ftalias)
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
        \ get(s:_DEFAULT_CHECKERS, ft, 0)

    return type(names) == type([]) ?
        \ self._filterCheckersByName(checkers_map, names) : [checkers_map[keys(checkers_map)[0]]]
endfunction " }}}2

" Same as getCheckers(), but keep only the available checkers.  This runs the
" corresponding IsAvailable() functions for all checkers.
function! g:SyntasticRegistry.getCheckersAvailable(ftalias, hints_list) abort " {{{2
    return filter(self.getCheckers(a:ftalias, a:hints_list), 'v:val.isAvailable()')
endfunction " }}}2

" Same as getCheckers(), but keep only the checkers that are available and
" disabled.  This runs the corresponding IsAvailable() functions for all checkers.
function! g:SyntasticRegistry.getCheckersDisabled(ftalias, hints_list) abort " {{{2
    return filter(self.getCheckers(a:ftalias, a:hints_list), 'v:val.isDisabled() && v:val.isAvailable()')
endfunction " }}}2

function! g:SyntasticRegistry.getKnownFiletypes() abort " {{{2
    let types = keys(s:_DEFAULT_CHECKERS)

    call extend(types, keys(s:_DEFAULT_FILETYPE_MAP))

    if exists('g:syntastic_filetype_map')
        call extend(types, keys(g:syntastic_filetype_map))
    endif

    if exists('g:syntastic_extra_filetypes') && type(g:syntastic_extra_filetypes) == type([])
        call extend(types, g:syntastic_extra_filetypes)
    endif

    return syntastic#util#unique(types)
endfunction " }}}2

function! g:SyntasticRegistry.getNamesOfAvailableCheckers(ftalias) abort " {{{2
    let ft = s:_normalise_filetype(a:ftalias)
    call self._loadCheckersFor(ft)
    return keys(filter( copy(self._checkerMap[ft]), 'v:val.isAvailable()' ))
endfunction " }}}2

function! g:SyntasticRegistry.echoInfoFor(ftalias_list) abort " {{{2
    let ft_list = syntastic#util#unique(map( copy(a:ftalias_list), 's:_normalise_filetype(v:val)' ))
    if len(ft_list) != 1
        let available = []
        let active = []
        let disabled = []

        for ft in ft_list
            call extend(available, map( self.getNamesOfAvailableCheckers(ft), 'ft . "/" . v:val' ))
            call extend(active, map( self.getCheckersAvailable(ft, []), 'ft . "/" . v:val.getName()' ))
            call extend(disabled, map( self.getCheckersDisabled(ft, []), 'ft . "/" . v:val.getName()' ))
        endfor
    else
        let ft = ft_list[0]
        let available = self.getNamesOfAvailableCheckers(ft)
        let active = map(self.getCheckersAvailable(ft, []), 'v:val.getName()')
        let disabled = map(self.getCheckersDisabled(ft, []), 'v:val.getName()')
    endif

    let cnt = len(available)
    let plural = cnt != 1 ? 's' : ''
    let cklist = cnt ? join(sort(available)) : '-'
    echomsg 'Available checker' . plural . ': ' . cklist

    let cnt = len(active)
    let plural = cnt != 1 ? 's' : ''
    let cklist = cnt ? join(active) : '-'
    echomsg 'Currently enabled checker' . plural . ': ' . cklist

    let cnt = len(disabled)
    let plural = cnt != 1 ? 's' : ''
    if len(disabled)
        let cklist = join(sort(disabled))
        echomsg 'Checker' . plural . ' disabled for security reasons: ' . cklist
    endif

    " Eclim feels entitled to mess with syntastic's variables {{{3
    if exists(':EclimValidate') && get(g:, 'EclimFileTypeValidate', 1)
        let disabled = filter(copy(ft_list), 's:_disabled_by_eclim(v:val)')
        let cnt = len(disabled)
        if cnt
            let plural = cnt != 1 ? 's' : ''
            let cklist = join(disabled, ', ')
            echomsg 'Checkers for filetype' . plural . ' ' . cklist . ' possibly disabled by Eclim'
        endif
    endif
    " }}}3

    " So does YouCompleteMe {{{3
    if exists('g:loaded_youcompleteme') && get(g:, 'ycm_show_diagnostics_ui', get(g:, 'ycm_register_as_syntastic_checker', 1))
        let disabled = filter(copy(ft_list), 's:_disabled_by_ycm(v:val)')
        let cnt = len(disabled)
        if cnt
            let plural = cnt != 1 ? 's' : ''
            let cklist = join(disabled, ', ')
            echomsg 'Checkers for filetype' . plural . ' ' . cklist . ' possibly disabled by YouCompleteMe'
        endif
    endif
    " }}}3
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

function! g:SyntasticRegistry._filterCheckersByName(checkers_map, list) abort " {{{2
    return filter( map(copy(a:list), 'get(a:checkers_map, v:val, {})'), '!empty(v:val)' )
endfunction " }}}2

function! g:SyntasticRegistry._loadCheckersFor(filetype) abort " {{{2
    if has_key(self._checkerMap, a:filetype)
        return
    endif

    execute 'runtime! syntax_checkers/' . a:filetype . '/*.vim'

    if !has_key(self._checkerMap, a:filetype)
        let self._checkerMap[a:filetype] = {}
    endif
endfunction " }}}2

" Check for obsolete variable g:syntastic_<filetype>_checker
function! g:SyntasticRegistry._checkDeprecation(filetype) abort " {{{2
    if exists('g:syntastic_' . a:filetype . '_checker') && !exists('g:syntastic_' . a:filetype . '_checkers')
        let g:syntastic_{a:filetype}_checkers = [g:syntastic_{a:filetype}_checker]
        call syntastic#log#oneTimeWarn('variable g:syntastic_' . a:filetype . '_checker is deprecated')
    endif
endfunction " }}}2

" }}}1

" Utilities {{{1

"resolve filetype aliases, and replace - with _ otherwise we cant name
"syntax checker functions legally for filetypes like "gentoo-metadata"
function! s:_normalise_filetype(ftalias) abort " {{{2
    let ft = get(s:_DEFAULT_FILETYPE_MAP, a:ftalias, a:ftalias)
    let ft = get(g:syntastic_filetype_map, ft, ft)
    let ft = substitute(ft, '\m-', '_', 'g')
    return ft
endfunction " }}}2

function! s:_disabled_by_eclim(filetype) abort " {{{2
    if index(s:_ECLIM_TYPES, a:filetype) >= 0
        let lang = toupper(a:filetype[0]) . a:filetype[1:]
        let ft = a:filetype !=# 'cpp' ? lang : 'C'
        return get(g:, 'Eclim' . lang . 'Validate', 1) && !get(g:, 'Eclim' . ft . 'SyntasticEnabled', 0)
    endif

    return 0
endfunction " }}}2

function! s:_disabled_by_ycm(filetype) abort " {{{2
    return index(s:_YCM_TYPES, a:filetype) >= 0
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
