" Type definitions for Universal Ctags

function! tagbar#types#uctags#init(supported_types) abort
    let types = {}

    " Ada {{{1
    let type_ada = tagbar#prototypes#typeinfo#new()
    let type_ada.ctagstype = 'ada'
    let type_ada.kinds = [
        \ {'short' : 'P', 'long' : 'package specifications',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'packages',                      'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'types',                         'fold' : 0, 'stl' : 1},
        \ {'short' : 'u', 'long' : 'subtypes',                      'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'record type components',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'l', 'long' : 'enum type literals',            'fold' : 0, 'stl' : 0},
        \ {'short' : 'v', 'long' : 'variables',                     'fold' : 0, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'generic formal parameters',     'fold' : 0, 'stl' : 0},
        \ {'short' : 'n', 'long' : 'constants',                     'fold' : 0, 'stl' : 0},
        \ {'short' : 'x', 'long' : 'user defined exceptions',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'R', 'long' : 'subprogram specifications',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'r', 'long' : 'subprograms',                   'fold' : 0, 'stl' : 1},
        \ {'short' : 'K', 'long' : 'task specifications',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'k', 'long' : 'tasks',                         'fold' : 0, 'stl' : 1},
        \ {'short' : 'O', 'long' : 'protected data specifications', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'o', 'long' : 'protected data',                'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'task/protected data entries',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'b', 'long' : 'labels',                        'fold' : 0, 'stl' : 1},
        \ {'short' : 'i', 'long' : 'loop/declare identifiers',      'fold' : 0, 'stl' : 1},
    \ ]
    let type_ada.sro        = '.' " Not sure if possible
    let type_ada.kind2scope = {
        \ 'P' : 'packspec',
        \ 't' : 'type',
    \ }
    let type_ada.scope2kind = {
        \ 'packspec' : 'P',
        \ 'type'     : 't',
    \ }
    let types.ada = type_ada
    " Ant {{{1
    let type_ant = tagbar#prototypes#typeinfo#new()
    let type_ant.ctagstype = 'ant'
    let type_ant.kinds     = [
        \ {'short' : 'p', 'long' : 'projects',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'i', 'long' : 'antfiles',   'fold' : 0, 'stl' : 0},
        \ {'short' : 'P', 'long' : 'properties', 'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'targets',    'fold' : 0, 'stl' : 1}
    \ ]
    let types.ant = type_ant
    " Asm {{{1
    let type_asm = tagbar#prototypes#typeinfo#new()
    let type_asm.ctagstype = 'asm'
    let type_asm.kinds     = [
        \ {'short' : 'm', 'long' : 'macros',    'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'types',     'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'sections',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'd', 'long' : 'defines',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'l', 'long' : 'labels',    'fold' : 0, 'stl' : 1}
    \ ]
    let types.asm = type_asm
    " ASP {{{1
    let type_aspvbs = tagbar#prototypes#typeinfo#new()
    let type_aspvbs.ctagstype = 'asp'
    let type_aspvbs.kinds     = [
        \ {'short' : 'd', 'long' : 'constants',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'classes',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',   'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'subroutines', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables',   'fold' : 0, 'stl' : 1}
    \ ]
    let types.aspvbs = type_aspvbs
    " Asymptote {{{1
    " Asymptote gets parsed well using filetype = c
    let type_asy = tagbar#prototypes#typeinfo#new()
    let type_asy.ctagstype = 'c'
    let type_asy.kinds     = [
        \ {'short' : 'd', 'long' : 'macros',      'fold' : 1, 'stl' : 0},
        \ {'short' : 'p', 'long' : 'prototypes',  'fold' : 1, 'stl' : 0},
        \ {'short' : 'g', 'long' : 'enums',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enumerators', 'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'typedefs',    'fold' : 0, 'stl' : 0},
        \ {'short' : 's', 'long' : 'structs',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'u', 'long' : 'unions',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'members',     'fold' : 0, 'stl' : 0},
        \ {'short' : 'v', 'long' : 'variables',   'fold' : 0, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'functions',   'fold' : 0, 'stl' : 1}
    \ ]
    let type_asy.sro        = '::'
    let type_asy.kind2scope = {
        \ 'g' : 'enum',
        \ 's' : 'struct',
        \ 'u' : 'union'
    \ }
    let type_asy.scope2kind = {
        \ 'enum'   : 'g',
        \ 'struct' : 's',
        \ 'union'  : 'u'
    \ }
    let types.asy = type_asy
    " Autoconf {{{1
    let type_autoconf = tagbar#prototypes#typeinfo#new()
    let type_autoconf.ctagstype = 'autoconf'
    let type_autoconf.kinds = [
        \ {'short': 'p', 'long': 'packages',            'fold': 0, 'stl': 1},
        \ {'short': 't', 'long': 'templates',           'fold': 0, 'stl': 1},
        \ {'short': 'm', 'long': 'autoconf macros',     'fold': 0, 'stl': 1},
        \ {'short': 'w', 'long': '"with" options',      'fold': 0, 'stl': 1},
        \ {'short': 'e', 'long': '"enable" options',    'fold': 0, 'stl': 1},
        \ {'short': 's', 'long': 'substitution keys',   'fold': 0, 'stl': 1},
        \ {'short': 'c', 'long': 'automake conditions', 'fold': 0, 'stl': 1},
        \ {'short': 'd', 'long': 'definitions',         'fold': 0, 'stl': 1},
    \ ]
    let types.config = type_autoconf
    " Automake {{{1
    let type_automake = tagbar#prototypes#typeinfo#new()
    let type_automake.ctagstype = 'automake'
    let type_automake.kinds = [
        \ {'short' : 'I', 'long' : 'makefiles',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'd', 'long' : 'directories', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'P', 'long' : 'programs',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'M', 'long' : 'manuals',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'macros',      'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'targets',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'T', 'long' : 'ltlibraries', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'L', 'long' : 'libraries',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'S', 'long' : 'scripts',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'D', 'long' : 'datum',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'conditions',  'fold' : 0, 'stl' : 1},
    \ ]
    let types.automake = type_automake
    " Awk {{{1
    let type_awk = tagbar#prototypes#typeinfo#new()
    let type_awk.ctagstype = 'awk'
    let type_awk.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.awk = type_awk
    " Basic {{{1
    let type_basic = tagbar#prototypes#typeinfo#new()
    let type_basic.ctagstype = 'basic'
    let type_basic.kinds     = [
        \ {'short' : 'c', 'long' : 'constants',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'g', 'long' : 'enumerations', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'l', 'long' : 'labels',       'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'types',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables',    'fold' : 0, 'stl' : 1}
    \ ]
    let types.basic = type_basic
    " BETA {{{1
    let type_beta = tagbar#prototypes#typeinfo#new()
    let type_beta.ctagstype = 'beta'
    let type_beta.kinds     = [
        \ {'short' : 'f', 'long' : 'fragments', 'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'slots',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'patterns',  'fold' : 0, 'stl' : 1}
    \ ]
    let types.beta = type_beta
    " C {{{1
    let type_c = tagbar#prototypes#typeinfo#new()
    let type_c.ctagstype = 'c'
    let type_c.kinds     = [
        \ {'short' : 'h', 'long' : 'header files', 'fold' : 1, 'stl' : 0},
        \ {'short' : 'd', 'long' : 'macros',       'fold' : 1, 'stl' : 0},
        \ {'short' : 'p', 'long' : 'prototypes',   'fold' : 1, 'stl' : 0},
        \ {'short' : 'g', 'long' : 'enums',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enumerators',  'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'typedefs',     'fold' : 0, 'stl' : 0},
        \ {'short' : 's', 'long' : 'structs',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'u', 'long' : 'unions',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'members',      'fold' : 0, 'stl' : 0},
        \ {'short' : 'v', 'long' : 'variables',    'fold' : 0, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'functions',    'fold' : 0, 'stl' : 1}
    \ ]
    let type_c.sro        = '::'
    let type_c.kind2scope = {
        \ 'g' : 'enum',
        \ 's' : 'struct',
        \ 'u' : 'union'
    \ }
    let type_c.scope2kind = {
        \ 'enum'   : 'g',
        \ 'struct' : 's',
        \ 'union'  : 'u'
    \ }
    let types.c = type_c
    " C++ {{{1
    let type_cpp = tagbar#prototypes#typeinfo#new()
    let type_cpp.ctagstype = 'c++'
    let type_cpp.kinds     = [
        \ {'short' : 'h', 'long' : 'header files', 'fold' : 1, 'stl' : 0},
        \ {'short' : 'd', 'long' : 'macros',       'fold' : 1, 'stl' : 0},
        \ {'short' : 'p', 'long' : 'prototypes',   'fold' : 1, 'stl' : 0},
        \ {'short' : 'g', 'long' : 'enums',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enumerators',  'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'typedefs',     'fold' : 0, 'stl' : 0},
        \ {'short' : 'n', 'long' : 'namespaces',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'classes',      'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'structs',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'u', 'long' : 'unions',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'members',      'fold' : 0, 'stl' : 0},
        \ {'short' : 'v', 'long' : 'variables',    'fold' : 0, 'stl' : 0}
    \ ]
    let type_cpp.sro        = '::'
    let type_cpp.kind2scope = {
        \ 'g' : 'enum',
        \ 'n' : 'namespace',
        \ 'c' : 'class',
        \ 's' : 'struct',
        \ 'u' : 'union'
    \ }
    let type_cpp.scope2kind = {
        \ 'enum'      : 'g',
        \ 'namespace' : 'n',
        \ 'class'     : 'c',
        \ 'struct'    : 's',
        \ 'union'     : 'u'
    \ }
    let types.cpp = type_cpp
    let types.cuda = type_cpp
    " C# {{{1
    let type_cs = tagbar#prototypes#typeinfo#new()
    let type_cs.ctagstype = 'c#'
    let type_cs.kinds     = [
        \ {'short' : 'd', 'long' : 'macros',      'fold' : 1, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'fields',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'g', 'long' : 'enums',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enumerators', 'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'typedefs',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'n', 'long' : 'namespaces',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'i', 'long' : 'interfaces',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'classes',     'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'structs',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'E', 'long' : 'events',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'methods',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'properties',  'fold' : 0, 'stl' : 1}
    \ ]
    let type_cs.sro        = '.'
    let type_cs.kind2scope = {
        \ 'n' : 'namespace',
        \ 'i' : 'interface',
        \ 'c' : 'class',
        \ 's' : 'struct',
        \ 'g' : 'enum'
    \ }
    let type_cs.scope2kind = {
        \ 'namespace' : 'n',
        \ 'interface' : 'i',
        \ 'class'     : 'c',
        \ 'struct'    : 's',
        \ 'enum'      : 'g'
    \ }
    let types.cs = type_cs
    " Clojure {{{1
    let type_clojure = tagbar#prototypes#typeinfo#new()
    let type_clojure.ctagstype = 'clojure'
    let type_clojure.kinds     = [
        \ {'short': 'n', 'long': 'namespace', 'fold': 0, 'stl': 1},
        \ {'short': 'f', 'long': 'function',  'fold': 0, 'stl': 1},
    \ ]
    let type_clojure.sro = '.'
    let type_clojure.kind2scope = {
        \ 'n' : 'namespace',
    \ }
    let type_clojure.scope2kind = {
        \ 'namespace'  : 'n'
    \ }
    let types.clojure = type_clojure
    " Ctags config {{{1
    let type_ctags = tagbar#prototypes#typeinfo#new()
    let type_ctags.ctagstype = 'ctags'
    let type_ctags.kinds = [
        \ {'short' : 'l', 'long' : 'language definitions', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'k', 'long' : 'kind definitions',     'fold' : 0, 'stl' : 1},
    \ ]
    let type_ctags.sro        = '.' " Not actually possible
    let type_ctags.kind2scope = {
        \ 'l' : 'langdef',
    \ }
    let type_ctags.scope2kind = {
        \ 'langdef' : 'l',
    \ }
    let types.ctags = type_ctags
    " COBOL {{{1
    let type_cobol = tagbar#prototypes#typeinfo#new()
    let type_cobol.ctagstype = 'cobol'
    let type_cobol.kinds     = [
        \ {'short' : 'd', 'long' : 'data items',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'D', 'long' : 'divisions',         'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'file descriptions', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'g', 'long' : 'group items',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'paragraphs',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'P', 'long' : 'program ids',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'S', 'long' : 'source code file',  'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'sections',          'fold' : 0, 'stl' : 1}
    \ ]
    let types.cobol = type_cobol
    " CSS {{{1
    let type_css = tagbar#prototypes#typeinfo#new()
    let type_css.ctagstype = 'css'
    let type_css.kinds     = [
        \ {'short' : 's', 'long' : 'selector',   'fold' : 0, 'stl' : 0},
        \ {'short' : 'i', 'long' : 'identities', 'fold' : 1, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'classes',    'fold' : 1, 'stl' : 0}
    \ ]
    let types.css = type_css
    " D {{{1
    let type_d = tagbar#prototypes#typeinfo#new()
    let type_d.ctagstype = 'D'
    let type_d.kinds = [
        \ {'short' : 'M', 'long' : 'modules',              'fold' : 0, 'stl' : 1},
        \ {'short' : 'V', 'long' : 'version statements',   'fold' : 1, 'stl' : 0},
        \ {'short' : 'n', 'long' : 'namespaces',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'T', 'long' : 'templates',            'fold' : 0, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'classes',              'fold' : 0, 'stl' : 1},
        \ {'short' : 'i', 'long' : 'interfaces',           'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'structure names',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'g', 'long' : 'enumeration names',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enumerators',          'fold' : 0, 'stl' : 0},
        \ {'short' : 'u', 'long' : 'union names',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'function prototypes',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'function definitions', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'members',              'fold' : 0, 'stl' : 1},
        \ {'short' : 'a', 'long' : 'aliases',              'fold' : 1, 'stl' : 0},
        \ {'short' : 'X', 'long' : 'mixins',               'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variable definitions', 'fold' : 0, 'stl' : 0},
    \ ]
    let type_d.sro = '.'
    let type_d.kind2scope = {
        \ 'g' : 'enum',
        \ 'n' : 'namespace',
        \ 'i' : 'interface',
        \ 'c' : 'class',
        \ 's' : 'struct',
        \ 'u' : 'union'
    \ }
    let type_d.scope2kind = {
        \ 'enum'      : 'g',
        \ 'namespace' : 'n',
        \ 'interface' : 'i',
        \ 'class'     : 'c',
        \ 'struct'    : 's',
        \ 'union'     : 'u'
    \ }
    let types.d = type_d
    " DOS Batch {{{1
    let type_dosbatch = tagbar#prototypes#typeinfo#new()
    let type_dosbatch.ctagstype = 'dosbatch'
    let type_dosbatch.kinds     = [
        \ {'short' : 'l', 'long' : 'labels',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.dosbatch = type_dosbatch
    " Eiffel {{{1
    let type_eiffel = tagbar#prototypes#typeinfo#new()
    let type_eiffel.ctagstype = 'eiffel'
    let type_eiffel.kinds     = [
        \ {'short' : 'c', 'long' : 'classes',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'features', 'fold' : 0, 'stl' : 1}
    \ ]
    let type_eiffel.sro        = '.' " Not sure, is nesting even possible?
    let type_eiffel.kind2scope = {
        \ 'c' : 'class',
        \ 'f' : 'feature'
    \ }
    let type_eiffel.scope2kind = {
        \ 'class'   : 'c',
        \ 'feature' : 'f'
    \ }
    let types.eiffel = type_eiffel
    " Elm {{{1
    " based on https://github.com/bitterjug/vim-tagbar-ctags-elm/blob/master/ftplugin/elm/tagbar-elm.vim
    let type_elm = tagbar#prototypes#typeinfo#new()
    let type_elm.ctagstype = 'elm'
    let type_elm.kinds = [
        \ {'short' : 'm', 'long' : 'modules',           'fold' : 0, 'stl' : 0},
        \ {'short' : 'i', 'long' : 'imports',           'fold' : 1, 'stl' : 0},
        \ {'short' : 't', 'long' : 'types',             'fold' : 1, 'stl' : 0},
        \ {'short' : 'a', 'long' : 'type aliases',      'fold' : 0, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'type constructors', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'p', 'long' : 'ports',             'fold' : 0, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'functions',         'fold' : 1, 'stl' : 0},
    \ ]
    let type_elm.sro = ':'
    let type_elm.kind2scope = {
        \ 'f' : 'function',
        \ 'm' : 'module',
        \ 't' : 'type'
    \ }
    let type_elm.scope2kind = {
        \ 'function' : 'f',
        \ 'module'   : 'm',
        \ 'type'     : 't'
    \ }
    let types.elm = type_elm
    " Erlang {{{1
    let type_erlang = tagbar#prototypes#typeinfo#new()
    let type_erlang.ctagstype = 'erlang'
    let type_erlang.kinds     = [
        \ {'short' : 'm', 'long' : 'modules',            'fold' : 0, 'stl' : 1},
        \ {'short' : 'd', 'long' : 'macro definitions',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'r', 'long' : 'record definitions', 'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'type definitions',   'fold' : 0, 'stl' : 1}
    \ ]
    let type_erlang.sro        = '.' " Not sure, is nesting even possible?
    let type_erlang.kind2scope = {
        \ 'm' : 'module'
    \ }
    let type_erlang.scope2kind = {
        \ 'module' : 'm'
    \ }
    let types.erlang = type_erlang
    " Flex {{{1
    " Vim doesn't support Flex out of the box, this is based on rough
    " guesses and probably requires
    " http://www.vim.org/scripts/script.php?script_id=2909
    " Improvements welcome!
    let type_as = tagbar#prototypes#typeinfo#new()
    let type_as.ctagstype = 'flex'
    let type_as.kinds     = [
        \ {'short' : 'v', 'long' : 'global variables', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'classes',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'methods',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'properties',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'x', 'long' : 'mxtags',           'fold' : 0, 'stl' : 0}
    \ ]
    let type_as.sro        = '.'
    let type_as.kind2scope = {
        \ 'c' : 'class'
    \ }
    let type_as.scope2kind = {
        \ 'class' : 'c'
    \ }
    let types.mxml = type_as
    let types.actionscript = type_as
    " Fortran {{{1
    let type_fortran = tagbar#prototypes#typeinfo#new()
    let type_fortran.ctagstype = 'fortran'
    let type_fortran.kinds     = [
        \ {'short' : 'm', 'long' : 'modules',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'programs',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'k', 'long' : 'components', 'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'derived types and structures', 'fold' : 0,
         \ 'stl' : 1},
        \ {'short' : 'c', 'long' : 'common blocks', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'b', 'long' : 'block data',    'fold' : 0, 'stl' : 0},
        \ {'short' : 'E', 'long' : 'enumerations',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'N', 'long' : 'enumeration values', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'e', 'long' : 'entry points',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',     'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'subroutines',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'M', 'long' : 'type bound procedures',   'fold' : 0,
         \ 'stl' : 1},
        \ {'short' : 'l', 'long' : 'labels',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'n', 'long' : 'namelists',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables',     'fold' : 0, 'stl' : 0}
    \ ]
    let type_fortran.sro        = '.' " Not sure, is nesting even possible?
    let type_fortran.kind2scope = {
        \ 'm' : 'module',
        \ 'p' : 'program',
        \ 'f' : 'function',
        \ 's' : 'subroutine'
    \ }
    let type_fortran.scope2kind = {
        \ 'module'     : 'm',
        \ 'program'    : 'p',
        \ 'function'   : 'f',
        \ 'subroutine' : 's'
    \ }
    let types.fortran = type_fortran
    " Go {{{1
    let type_go = tagbar#prototypes#typeinfo#new()
    let type_go.ctagstype = 'go'
    let type_go.kinds = [
        \ {'short' : 'p', 'long' : 'packages',       'fold' : 0, 'stl' : 0},
        \ {'short' : 'i', 'long' : 'interfaces',     'fold' : 0, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'constants',      'fold' : 0, 'stl' : 0},
        \ {'short' : 's', 'long' : 'structs',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'struct members', 'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'types',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables',      'fold' : 0, 'stl' : 0}
    \ ]
    let type_go.sro = '.'
    let type_go.kind2scope = {
        \ 's' : 'struct'
    \ }
    let type_go.scope2kind = {
        \ 'struct' : 's'
    \ }
    let types.go = type_go
    " HTML {{{1
    let type_html = tagbar#prototypes#typeinfo#new()
    let type_html.ctagstype = 'html'
    let type_html.kinds = [
        \ {'short' : 'a', 'long' : 'named anchors', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'h', 'long' : 'H1 headings',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'i', 'long' : 'H2 headings',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'j', 'long' : 'H3 headings',   'fold' : 0, 'stl' : 1},
    \ ]
    let types.html = type_html
    " Java {{{1
    let type_java = tagbar#prototypes#typeinfo#new()
    let type_java.ctagstype = 'java'
    let type_java.kinds     = [
        \ {'short' : 'p', 'long' : 'packages',       'fold' : 1, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'fields',         'fold' : 0, 'stl' : 0},
        \ {'short' : 'g', 'long' : 'enum types',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enum constants', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'a', 'long' : 'annotations',    'fold' : 0, 'stl' : 0},
        \ {'short' : 'i', 'long' : 'interfaces',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'classes',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'methods',        'fold' : 0, 'stl' : 1}
    \ ]
    let type_java.sro        = '.'
    let type_java.kind2scope = {
        \ 'g' : 'enum',
        \ 'i' : 'interface',
        \ 'c' : 'class'
    \ }
    let type_java.scope2kind = {
        \ 'enum'      : 'g',
        \ 'interface' : 'i',
        \ 'class'     : 'c'
    \ }
    let types.java = type_java
    " JavaScript {{{1
    let type_javascript = tagbar#prototypes#typeinfo#new()
    let type_javascript.ctagstype = 'javascript'
    let type_javascript.kinds = [
        \ {'short': 'v', 'long': 'global variables', 'fold': 0, 'stl': 0},
        \ {'short': 'C', 'long': 'constants',        'fold': 0, 'stl': 0},
        \ {'short': 'c', 'long': 'classes',          'fold': 0, 'stl': 1},
        \ {'short': 'g', 'long': 'generators',       'fold': 0, 'stl': 0},
        \ {'short': 'p', 'long': 'properties',       'fold': 0, 'stl': 0},
        \ {'short': 'm', 'long': 'methods',          'fold': 0, 'stl': 1},
        \ {'short': 'f', 'long': 'functions',        'fold': 0, 'stl': 1},
    \ ]
    let type_javascript.sro        = '.'
    let type_javascript.kind2scope = {
        \ 'c' : 'class',
        \ 'f' : 'function',
        \ 'm' : 'method',
        \ 'p' : 'property',
    \ }
    let type_javascript.scope2kind = {
        \ 'class'    : 'c',
        \ 'function' : 'f',
    \ }
    let types.javascript = type_javascript
    " Lisp {{{1
    let type_lisp = tagbar#prototypes#typeinfo#new()
    let type_lisp.ctagstype = 'lisp'
    let type_lisp.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.lisp = type_lisp
    " Lua {{{1
    let type_lua = tagbar#prototypes#typeinfo#new()
    let type_lua.ctagstype = 'lua'
    let type_lua.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.lua = type_lua
    " Make {{{1
    let type_make = tagbar#prototypes#typeinfo#new()
    let type_make.ctagstype = 'make'
    let type_make.kinds     = [
        \ {'short' : 'I', 'long' : 'makefiles', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'm', 'long' : 'macros',    'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'targets',   'fold' : 0, 'stl' : 1}
    \ ]
    let types.make = type_make
    " Matlab {{{1
    let type_matlab = tagbar#prototypes#typeinfo#new()
    let type_matlab.ctagstype = 'matlab'
    let type_matlab.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables', 'fold' : 0, 'stl' : 0}
    \ ]
    let types.matlab = type_matlab
    " ObjectiveC {{{1
    let type_objc = tagbar#prototypes#typeinfo#new()
    let type_objc.ctagstype = 'objectivec'
    let type_objc.kinds = [
        \ {'short' : 'M', 'long' : 'preprocessor macros',   'fold' : 1, 'stl' : 0},
        \ {'short' : 't', 'long' : 'type aliases',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'global variables',      'fold' : 0, 'stl' : 0},
        \ {'short' : 'i', 'long' : 'class interfaces',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'I', 'long' : 'class implementations', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'class methods',         'fold' : 0, 'stl' : 1},
        \ {'short' : 'E', 'long' : 'object fields',         'fold' : 0, 'stl' : 0},
        \ {'short' : 'm', 'long' : 'object methods',        'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'type structures',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enumerations',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',             'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'properties',            'fold' : 0, 'stl' : 0},
        \ {'short' : 'P', 'long' : 'protocols',             'fold' : 0, 'stl' : 0},
    \ ]
    let type_objc.sro        = ':'
    let type_objc.kind2scope = {
        \ 'i' : 'interface',
        \ 'I' : 'implementation',
        \ 's' : 'struct',
        \ 'p' : 'protocol',
    \ }
    let type_objc.scope2kind = {
        \ 'interface' : 'i',
        \ 'implementation' : 'I',
        \ 'struct' : 's',
        \ 'protocol' : 'p',
    \ }
    let types.objc = type_objc
    let types.objcpp = type_objc
    " Ocaml {{{1
    let type_ocaml = tagbar#prototypes#typeinfo#new()
    let type_ocaml.ctagstype = 'ocaml'
    let type_ocaml.kinds     = [
        \ {'short' : 'M', 'long' : 'modules or functors', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'global variables',    'fold' : 0, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'classes',             'fold' : 0, 'stl' : 1},
        \ {'short' : 'C', 'long' : 'constructors',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'methods',             'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'exceptions',          'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'type names',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'r', 'long' : 'structure fields',    'fold' : 0, 'stl' : 0},
        \ {'short' : 'p', 'long' : 'signature items',     'fold' : 0, 'stl' : 0}
    \ ]
    let type_ocaml.sro        = '.' " Not sure, is nesting even possible?
    let type_ocaml.kind2scope = {
        \ 'M' : 'Module',
        \ 'c' : 'class',
        \ 't' : 'type'
    \ }
    let type_ocaml.scope2kind = {
        \ 'Module' : 'M',
        \ 'class'  : 'c',
        \ 'type'   : 't'
    \ }
    let types.ocaml = type_ocaml
    " Pascal {{{1
    let type_pascal = tagbar#prototypes#typeinfo#new()
    let type_pascal.ctagstype = 'pascal'
    let type_pascal.kinds     = [
        \ {'short' : 'f', 'long' : 'functions',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'procedures', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.pascal = type_pascal
    " Perl {{{1
    let type_perl = tagbar#prototypes#typeinfo#new()
    let type_perl.ctagstype = 'perl'
    let type_perl.kinds     = [
        \ {'short' : 'p', 'long' : 'packages',    'fold' : 1, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'constants',   'fold' : 0, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'formats',     'fold' : 0, 'stl' : 0},
        \ {'short' : 'l', 'long' : 'labels',      'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'subroutines', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.perl = type_perl
    " Perl 6 {{{1
    let type_perl6 = tagbar#prototypes#typeinfo#new()
    let type_perl6.ctagstype = 'perl6'
    let type_perl6.kinds     = [
        \ {'short' : 'o', 'long' : 'modules',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'packages',    'fold' : 1, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'classes',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'g', 'long' : 'grammars',    'fold' : 0, 'stl' : 0},
        \ {'short' : 'm', 'long' : 'methods',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'r', 'long' : 'roles',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'u', 'long' : 'rules',       'fold' : 0, 'stl' : 0},
        \ {'short' : 'b', 'long' : 'submethods',  'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'subroutines', 'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'tokens',      'fold' : 0, 'stl' : 0},
    \ ]
    let types.perl6 = type_perl6
    " PHP {{{1
    let type_php = tagbar#prototypes#typeinfo#new()
    let type_php.ctagstype = 'php'
    let type_php.kinds     = [
        \ {'short' : 'n', 'long' : 'namespaces',           'fold' : 0, 'stl' : 0},
        \ {'short' : 'a', 'long' : 'use aliases',          'fold' : 1, 'stl' : 0},
        \ {'short' : 'd', 'long' : 'constant definitions', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'i', 'long' : 'interfaces',           'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'traits',               'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'classes',              'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables',            'fold' : 1, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'functions',            'fold' : 0, 'stl' : 1}
    \ ]
    let type_php.sro        = '\\'
    let type_php.kind2scope = {
        \ 'c' : 'class',
        \ 'n' : 'namespace',
        \ 'i' : 'interface',
        \ 't' : 'trait',
    \ }
    let type_php.scope2kind = {
        \ 'class'     : 'c',
        \ 'namespace' : 'n',
        \ 'interface' : 'i',
        \ 'trait'     : 't',
    \ }
    let types.php = type_php
    " Protobuf {{{1
    let type_protobuf = tagbar#prototypes#typeinfo#new()
    let type_protobuf.ctagstype = 'Protobuf'
    let type_protobuf.kinds = [
        \ {'short' : 'p', 'long' : 'packages',       'fold' : 0, 'stl' : 0},
        \ {'short' : 'm', 'long' : 'messages',       'fold' : 0, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'fields',         'fold' : 0, 'stl' : 0},
        \ {'short' : 'e', 'long' : 'enum constants', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'g', 'long' : 'enum types',     'fold' : 0, 'stl' : 0},
        \ {'short' : 's', 'long' : 'services',       'fold' : 0, 'stl' : 0},
    \ ]
    let types.proto = type_protobuf
    " Python {{{1
    let type_python = tagbar#prototypes#typeinfo#new()
    let type_python.ctagstype = 'python'
    let type_python.kinds     = [
        \ {'short' : 'i', 'long' : 'modules',   'fold' : 1, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'classes',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'members',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables', 'fold' : 0, 'stl' : 0}
    \ ]
    let type_python.sro        = '.'
    let type_python.kind2scope = {
        \ 'c' : 'class',
        \ 'f' : 'function',
        \ 'm' : 'function'
    \ }
    let type_python.scope2kind = {
        \ 'class'    : 'c',
        \ 'function' : 'f'
    \ }
    let type_python.kind2scope.m = 'member'
    let type_python.scope2kind.member = 'm'
    let types.python = type_python
    let types.pyrex  = type_python
    let types.cython = type_python
    " R {{{1
    let type_r = tagbar#prototypes#typeinfo#new()
    let type_r.ctagstype = 'R'
    let type_r.kinds = [
        \ {'short' : 'l', 'long' : 'libraries',          'fold' : 1, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'functions',          'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'sources',            'fold' : 0, 'stl' : 0},
        \ {'short' : 'g', 'long' : 'global variables',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'function variables', 'fold' : 0, 'stl' : 0},
    \ ]
    let types.r = type_r
    " REXX {{{1
    let type_rexx = tagbar#prototypes#typeinfo#new()
    let type_rexx.ctagstype = 'rexx'
    let type_rexx.kinds     = [
        \ {'short' : 's', 'long' : 'subroutines', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.rexx = type_rexx
    " Ruby {{{1
    let type_ruby = tagbar#prototypes#typeinfo#new()
    let type_ruby.ctagstype = 'ruby'
    let type_ruby.kinds     = [
        \ {'short' : 'm', 'long' : 'modules',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'classes',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'methods',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'S', 'long' : 'singleton methods', 'fold' : 0, 'stl' : 1}
    \ ]
    let type_ruby.sro        = '.'
    let type_ruby.kind2scope = {
        \ 'c' : 'class',
        \ 'f' : 'method',
        \ 'm' : 'module'
    \ }
    let type_ruby.scope2kind = {
        \ 'class'  : 'c',
        \ 'method' : 'f',
        \ 'module' : 'm'
    \ }
    let types.ruby = type_ruby
    " Rust {{{1
    let type_rust = tagbar#prototypes#typeinfo#new()
    let type_rust.ctagstype = 'rust'
    let type_rust.kinds     = [
        \ {'short' : 'n', 'long' : 'module',          'fold' : 1, 'stl' : 0},
        \ {'short' : 's', 'long' : 'struct',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'i', 'long' : 'trait',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'implementation',  'fold' : 0, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'function',        'fold' : 0, 'stl' : 1},
        \ {'short' : 'g', 'long' : 'enum',            'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'type alias',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'global variable', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'M', 'long' : 'macro',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'struct field',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enum variant',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'F', 'long' : 'method',          'fold' : 0, 'stl' : 1}
    \ ]
    let type_rust.sro        = '::'
    let type_rust.kind2scope = {
        \ 'n' : 'module',
        \ 's' : 'struct',
        \ 'i' : 'interface',
        \ 'c' : 'implementation',
        \ 'f' : 'function',
        \ 'g' : 'enum',
        \ 'F' : 'method',
    \ }
    let type_rust.scope2kind = {
        \ 'module'        : 'n',
        \ 'struct'        : 's',
        \ 'interface'     : 'i',
        \ 'implementation': 'c',
        \ 'function'      : 'f',
        \ 'enum'          : 'g',
        \ 'method'        : 'F',
    \ }
    let types.rust = type_rust
    " Scheme {{{1
    let type_scheme = tagbar#prototypes#typeinfo#new()
    let type_scheme.ctagstype = 'scheme'
    let type_scheme.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'sets',      'fold' : 0, 'stl' : 1}
    \ ]
    let types.scheme = type_scheme
    let types.racket = type_scheme
    " Shell script {{{1
    let type_sh = tagbar#prototypes#typeinfo#new()
    let type_sh.ctagstype = 'sh'
    let type_sh.kinds     = [
        \ {'short' : 'f', 'long' : 'functions',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'a', 'long' : 'aliases',      'fold' : 0, 'stl' : 0},
        \ {'short' : 's', 'long' : 'script files', 'fold' : 0, 'stl' : 0}
    \ ]
    let types.sh = type_sh
    let types.csh = type_sh
    let types.zsh = type_sh
    " SLang {{{1
    let type_slang = tagbar#prototypes#typeinfo#new()
    let type_slang.ctagstype = 'slang'
    let type_slang.kinds     = [
        \ {'short' : 'n', 'long' : 'namespaces', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',  'fold' : 0, 'stl' : 1}
    \ ]
    let types.slang = type_slang
    " SML {{{1
    let type_sml = tagbar#prototypes#typeinfo#new()
    let type_sml.ctagstype = 'sml'
    let type_sml.kinds     = [
        \ {'short' : 'e', 'long' : 'exception declarations', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'function definitions',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'functor definitions',    'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'signature declarations', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'r', 'long' : 'structure declarations', 'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'type definitions',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'value bindings',         'fold' : 0, 'stl' : 0}
    \ ]
    let types.sml = type_sml
    " SQL {{{1
    " The SQL ctags parser seems to be buggy for me, so this just uses the
    " normal kinds even though scopes should be available. Improvements
    " welcome!
    let type_sql = tagbar#prototypes#typeinfo#new()
    let type_sql.ctagstype = 'sql'
    let type_sql.kinds     = [
        \ {'short' : 'P', 'long' : 'packages',               'fold' : 1, 'stl' : 1},
        \ {'short' : 'd', 'long' : 'prototypes',             'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'cursors',                'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',              'fold' : 0, 'stl' : 1},
        \ {'short' : 'E', 'long' : 'record fields',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'L', 'long' : 'block label',            'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'procedures',             'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'subtypes',               'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'tables',                 'fold' : 0, 'stl' : 1},
        \ {'short' : 'T', 'long' : 'triggers',               'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables',              'fold' : 0, 'stl' : 1},
        \ {'short' : 'i', 'long' : 'indexes',                'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'events',                 'fold' : 0, 'stl' : 1},
        \ {'short' : 'U', 'long' : 'publications',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'R', 'long' : 'services',               'fold' : 0, 'stl' : 1},
        \ {'short' : 'D', 'long' : 'domains',                'fold' : 0, 'stl' : 1},
        \ {'short' : 'V', 'long' : 'views',                  'fold' : 0, 'stl' : 1},
        \ {'short' : 'n', 'long' : 'synonyms',               'fold' : 0, 'stl' : 1},
        \ {'short' : 'x', 'long' : 'MobiLink Table Scripts', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'y', 'long' : 'MobiLink Conn Scripts',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'z', 'long' : 'MobiLink Properties',    'fold' : 0, 'stl' : 1}
    \ ]
    let types.sql = type_sql
    " Tcl {{{1
    let type_tcl = tagbar#prototypes#typeinfo#new()
    let type_tcl.ctagstype = 'tcl'
    let type_tcl.kinds     = [
        \ {'short' : 'n', 'long' : 'namespaces', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'procedures', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.tcl = type_tcl
    " LaTeX {{{1
    let type_tex = tagbar#prototypes#typeinfo#new()
    let type_tex.ctagstype = 'tex'
    let type_tex.kinds     = [
        \ {'short' : 'i', 'long' : 'includes',       'fold' : 1, 'stl' : 0},
        \ {'short' : 'p', 'long' : 'parts',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'chapters',       'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'sections',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'u', 'long' : 'subsections',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'b', 'long' : 'subsubsections', 'fold' : 0, 'stl' : 1},
        \ {'short' : 'P', 'long' : 'paragraphs',     'fold' : 0, 'stl' : 0},
        \ {'short' : 'G', 'long' : 'subparagraphs',  'fold' : 0, 'stl' : 0},
        \ {'short' : 'l', 'long' : 'labels',         'fold' : 0, 'stl' : 0}
    \ ]
    let type_tex.sro        = '""'
    let type_tex.kind2scope = {
        \ 'p' : 'part',
        \ 'c' : 'chapter',
        \ 's' : 'section',
        \ 'u' : 'subsection',
        \ 'b' : 'subsubsection'
    \ }
    let type_tex.scope2kind = {
        \ 'part'          : 'p',
        \ 'chapter'       : 'c',
        \ 'section'       : 's',
        \ 'subsection'    : 'u',
        \ 'subsubsection' : 'b'
    \ }
    let type_tex.sort = 0
    let types.tex = type_tex
    " Vala {{{1
    " Vala is supported by the ctags fork provided by Anjuta, so only add the
    " type if the fork is used to prevent error messages otherwise
    if has_key(a:supported_types, 'vala') || executable('anjuta-tags')
        let type_vala = tagbar#prototypes#typeinfo#new()
        let type_vala.ctagstype = 'vala'
        let type_vala.kinds     = [
            \ {'short' : 'e', 'long' : 'Enumerations',       'fold' : 0, 'stl' : 1},
            \ {'short' : 'v', 'long' : 'Enumeration values', 'fold' : 0, 'stl' : 0},
            \ {'short' : 's', 'long' : 'Structures',         'fold' : 0, 'stl' : 1},
            \ {'short' : 'i', 'long' : 'Interfaces',         'fold' : 0, 'stl' : 1},
            \ {'short' : 'd', 'long' : 'Delegates',          'fold' : 0, 'stl' : 1},
            \ {'short' : 'c', 'long' : 'Classes',            'fold' : 0, 'stl' : 1},
            \ {'short' : 'p', 'long' : 'Properties',         'fold' : 0, 'stl' : 0},
            \ {'short' : 'f', 'long' : 'Fields',             'fold' : 0, 'stl' : 0},
            \ {'short' : 'm', 'long' : 'Methods',            'fold' : 0, 'stl' : 1},
            \ {'short' : 'E', 'long' : 'Error domains',      'fold' : 0, 'stl' : 1},
            \ {'short' : 'r', 'long' : 'Error codes',        'fold' : 0, 'stl' : 1},
            \ {'short' : 'S', 'long' : 'Signals',            'fold' : 0, 'stl' : 1}
        \ ]
        let type_vala.sro = '.'
        " 'enum' doesn't seem to be used as a scope, but it can't hurt to have
        " it here
        let type_vala.kind2scope = {
            \ 's' : 'struct',
            \ 'i' : 'interface',
            \ 'c' : 'class',
            \ 'e' : 'enum'
        \ }
        let type_vala.scope2kind = {
            \ 'struct'    : 's',
            \ 'interface' : 'i',
            \ 'class'     : 'c',
            \ 'enum'      : 'e'
        \ }
        let types.vala = type_vala
    endif
    if !has_key(a:supported_types, 'vala') && executable('anjuta-tags')
        let types.vala.ctagsbin = 'anjuta-tags'
    endif
    " Vera {{{1
    " Why are variables 'virtual'?
    let type_vera = tagbar#prototypes#typeinfo#new()
    let type_vera.ctagstype = 'vera'
    let type_vera.kinds     = [
        \ {'short' : 'h', 'long' : 'header files', 'fold' : 1, 'stl' : 0},
        \ {'short' : 'd', 'long' : 'macros',      'fold' : 1, 'stl' : 0},
        \ {'short' : 'g', 'long' : 'enums',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'T', 'long' : 'typedefs',    'fold' : 0, 'stl' : 0},
        \ {'short' : 'i', 'long' : 'interfaces',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'classes',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'enumerators', 'fold' : 0, 'stl' : 0},
        \ {'short' : 'm', 'long' : 'members',     'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',   'fold' : 0, 'stl' : 1},
        \ {'short' : 's', 'long' : 'signals',     'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'tasks',       'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables',   'fold' : 0, 'stl' : 0},
        \ {'short' : 'p', 'long' : 'programs',    'fold' : 0, 'stl' : 1}
    \ ]
    let type_vera.sro        = '.' " Nesting doesn't seem to be possible
    let type_vera.kind2scope = {
        \ 'g' : 'enum',
        \ 'c' : 'class',
        \ 'v' : 'virtual'
    \ }
    let type_vera.scope2kind = {
        \ 'enum'      : 'g',
        \ 'class'     : 'c',
        \ 'virtual'   : 'v'
    \ }
    let types.vera = type_vera
    " Verilog {{{1
    let type_verilog = tagbar#prototypes#typeinfo#new()
    let type_verilog.ctagstype = 'verilog'
    let type_verilog.kinds     = [
        \ {'short' : 'c', 'long' : 'constants',           'fold' : 0, 'stl' : 0},
        \ {'short' : 'e', 'long' : 'events',              'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',           'fold' : 0, 'stl' : 1},
        \ {'short' : 'm', 'long' : 'modules',             'fold' : 0, 'stl' : 1},
        \ {'short' : 'b', 'long' : 'blocks',              'fold' : 0, 'stl' : 1},
        \ {'short' : 'n', 'long' : 'net data types',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'ports',               'fold' : 0, 'stl' : 1},
        \ {'short' : 'r', 'long' : 'register data types', 'fold' : 0, 'stl' : 1},
        \ {'short' : 't', 'long' : 'tasks',               'fold' : 0, 'stl' : 1}
    \ ]
    let types.verilog = type_verilog
    " VHDL {{{1
    " The VHDL ctags parser unfortunately doesn't generate proper scopes
    let type_vhdl = tagbar#prototypes#typeinfo#new()
    let type_vhdl.ctagstype = 'vhdl'
    let type_vhdl.kinds     = [
        \ {'short' : 'P', 'long' : 'packages',   'fold' : 1, 'stl' : 0},
        \ {'short' : 'c', 'long' : 'constants',  'fold' : 0, 'stl' : 0},
        \ {'short' : 't', 'long' : 'types',      'fold' : 0, 'stl' : 1},
        \ {'short' : 'T', 'long' : 'subtypes',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'r', 'long' : 'records',    'fold' : 0, 'stl' : 1},
        \ {'short' : 'e', 'long' : 'entities',   'fold' : 0, 'stl' : 1},
        \ {'short' : 'f', 'long' : 'functions',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'p', 'long' : 'procedures', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.vhdl = type_vhdl
    " Vim {{{1
    let type_vim = tagbar#prototypes#typeinfo#new()
    let type_vim.ctagstype = 'vim'
    let type_vim.kinds     = [
        \ {'short' : 'n', 'long' : 'vimball filenames',  'fold' : 0, 'stl' : 1},
        \ {'short' : 'v', 'long' : 'variables',          'fold' : 1, 'stl' : 0},
        \ {'short' : 'f', 'long' : 'functions',          'fold' : 0, 'stl' : 1},
        \ {'short' : 'a', 'long' : 'autocommand groups', 'fold' : 1, 'stl' : 1},
        \ {'short' : 'c', 'long' : 'commands',           'fold' : 0, 'stl' : 0},
        \ {'short' : 'm', 'long' : 'maps',               'fold' : 1, 'stl' : 0}
    \ ]
    let types.vim = type_vim
    " YACC {{{1
    let type_yacc = tagbar#prototypes#typeinfo#new()
    let type_yacc.ctagstype = 'yacc'
    let type_yacc.kinds     = [
        \ {'short' : 'l', 'long' : 'labels', 'fold' : 0, 'stl' : 1}
    \ ]
    let types.yacc = type_yacc
    " }}}1

    for [type, typeinfo] in items(types)
        let typeinfo.ftype = type
    endfor

    for typeinfo in values(types)
        call typeinfo.createKinddict()
    endfor

    return types
endfunction

" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
