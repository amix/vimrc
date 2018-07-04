" Author: w0rp <devw0rp@gmail.com>
" Description: A registry of functions for fixing things.

let s:default_registry = {
\   'add_blank_lines_for_python_control_statements': {
\       'function': 'ale#fixers#generic_python#AddLinesBeforeControlStatements',
\       'suggested_filetypes': ['python'],
\       'description': 'Add blank lines before control statements.',
\   },
\   'align_help_tags': {
\       'function': 'ale#fixers#help#AlignTags',
\       'suggested_filetypes': ['help'],
\       'description': 'Align help tags to the right margin',
\   },
\   'autopep8': {
\       'function': 'ale#fixers#autopep8#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Fix PEP8 issues with autopep8.',
\   },
\   'black': {
\       'function': 'ale#fixers#black#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Fix PEP8 issues with black.',
\   },
\   'tidy': {
\       'function': 'ale#fixers#tidy#Fix',
\       'suggested_filetypes': ['html'],
\       'description': 'Fix HTML files with tidy.',
\   },
\   'prettier_standard': {
\       'function': 'ale#fixers#prettier_standard#Fix',
\       'suggested_filetypes': ['javascript'],
\       'description': 'Apply prettier-standard to a file.',
\       'aliases': ['prettier-standard'],
\   },
\   'elm-format': {
\       'function': 'ale#fixers#elm_format#Fix',
\       'suggested_filetypes': ['elm'],
\       'description': 'Apply elm-format to a file.',
\       'aliases': ['format'],
\   },
\   'eslint': {
\       'function': 'ale#fixers#eslint#Fix',
\       'suggested_filetypes': ['javascript', 'typescript'],
\       'description': 'Apply eslint --fix to a file.',
\   },
\   'mix_format': {
\       'function': 'ale#fixers#mix_format#Fix',
\       'suggested_filetypes': ['elixir'],
\       'description': 'Apply mix format to a file.',
\   },
\   'isort': {
\       'function': 'ale#fixers#isort#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Sort Python imports with isort.',
\   },
\   'prettier': {
\       'function': 'ale#fixers#prettier#Fix',
\       'suggested_filetypes': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'json5', 'graphql', 'markdown', 'vue'],
\       'description': 'Apply prettier to a file.',
\   },
\   'prettier_eslint': {
\       'function': 'ale#fixers#prettier_eslint#Fix',
\       'suggested_filetypes': ['javascript'],
\       'description': 'Apply prettier-eslint to a file.',
\       'aliases': ['prettier-eslint'],
\   },
\   'importjs': {
\       'function': 'ale#fixers#importjs#Fix',
\       'suggested_filetypes': ['javascript'],
\       'description': 'automatic imports for javascript',
\   },
\   'puppetlint': {
\       'function': 'ale#fixers#puppetlint#Fix',
\       'suggested_filetypes': ['puppet'],
\       'description': 'Run puppet-lint -f on a file.',
\   },
\   'remove_trailing_lines': {
\       'function': 'ale#fixers#generic#RemoveTrailingBlankLines',
\       'suggested_filetypes': [],
\       'description': 'Remove all blank lines at the end of a file.',
\   },
\   'trim_whitespace': {
\       'function': 'ale#fixers#generic#TrimWhitespace',
\       'suggested_filetypes': [],
\       'description': 'Remove all trailing whitespace characters at the end of every line.',
\   },
\   'yapf': {
\       'function': 'ale#fixers#yapf#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Fix Python files with yapf.',
\   },
\   'rubocop': {
\       'function': 'ale#fixers#rubocop#Fix',
\       'suggested_filetypes': ['ruby'],
\       'description': 'Fix ruby files with rubocop --auto-correct.',
\   },
\   'rufo': {
\       'function': 'ale#fixers#rufo#Fix',
\       'suggested_filetypes': ['ruby'],
\       'description': 'Fix ruby files with rufo',
\   },
\   'scalafmt': {
\       'function': 'ale#fixers#scalafmt#Fix',
\       'suggested_filetypes': ['scala'],
\       'description': 'Fix Scala files using scalafmt',
\   },
\   'standard': {
\       'function': 'ale#fixers#standard#Fix',
\       'suggested_filetypes': ['javascript'],
\       'description': 'Fix JavaScript files using standard --fix',
\   },
\   'stylelint': {
\       'function': 'ale#fixers#stylelint#Fix',
\       'suggested_filetypes': ['css', 'sass', 'scss', 'stylus'],
\       'description': 'Fix stylesheet files using stylelint --fix.',
\   },
\   'swiftformat': {
\       'function': 'ale#fixers#swiftformat#Fix',
\       'suggested_filetypes': ['swift'],
\       'description': 'Apply SwiftFormat to a file.',
\   },
\   'phpcbf': {
\       'function': 'ale#fixers#phpcbf#Fix',
\       'suggested_filetypes': ['php'],
\       'description': 'Fix PHP files with phpcbf.',
\   },
\   'php_cs_fixer': {
\       'function': 'ale#fixers#php_cs_fixer#Fix',
\       'suggested_filetypes': ['php'],
\       'description': 'Fix PHP files with php-cs-fixer.',
\   },
\   'clang-format': {
\       'function': 'ale#fixers#clangformat#Fix',
\       'suggested_filetypes': ['c', 'cpp'],
\       'description': 'Fix C/C++ files with clang-format.',
\   },
\   'gofmt': {
\       'function': 'ale#fixers#gofmt#Fix',
\       'suggested_filetypes': ['go'],
\       'description': 'Fix Go files with go fmt.',
\   },
\   'goimports': {
\       'function': 'ale#fixers#goimports#Fix',
\       'suggested_filetypes': ['go'],
\       'description': 'Fix Go files imports with goimports.',
\   },
\   'tslint': {
\       'function': 'ale#fixers#tslint#Fix',
\       'suggested_filetypes': ['typescript'],
\       'description': 'Fix typescript files with tslint --fix.',
\   },
\   'rustfmt': {
\       'function': 'ale#fixers#rustfmt#Fix',
\       'suggested_filetypes': ['rust'],
\       'description': 'Fix Rust files with Rustfmt.',
\   },
\   'hackfmt': {
\       'function': 'ale#fixers#hackfmt#Fix',
\       'suggested_filetypes': ['php'],
\       'description': 'Fix Hack files with hackfmt.',
\   },
\   'hfmt': {
\       'function': 'ale#fixers#hfmt#Fix',
\       'suggested_filetypes': ['haskell'],
\       'description': 'Fix Haskell files with hfmt.',
\   },
\   'brittany': {
\       'function': 'ale#fixers#brittany#Fix',
\       'suggested_filetypes': ['haskell'],
\       'description': 'Fix Haskell files with brittany.',
\   },
\   'refmt': {
\       'function': 'ale#fixers#refmt#Fix',
\       'suggested_filetypes': ['reason'],
\       'description': 'Fix ReasonML files with refmt.',
\   },
\   'shfmt': {
\       'function': 'ale#fixers#shfmt#Fix',
\       'suggested_filetypes': ['sh'],
\       'description': 'Fix sh files with shfmt.',
\   },
\   'google_java_format': {
\       'function': 'ale#fixers#google_java_format#Fix',
\       'suggested_filetypes': ['java'],
\       'description': 'Fix Java files with google-java-format.',
\   },
\   'fixjson': {
\       'function': 'ale#fixers#fixjson#Fix',
\       'suggested_filetypes': ['json'],
\       'description': 'Fix JSON files with fixjson.',
\   },
\   'jq': {
\       'function': 'ale#fixers#jq#Fix',
\       'suggested_filetypes': ['json'],
\       'description': 'Fix JSON files with jq.',
\   },
\   'perltidy': {
\       'function': 'ale#fixers#perltidy#Fix',
\       'suggested_filetypes': ['perl'],
\       'description': 'Fix Perl files with perltidy.',
\   },
\   'xo': {
\       'function': 'ale#fixers#xo#Fix',
\       'suggested_filetypes': ['javascript'],
\       'description': 'Fix JavaScript files using xo --fix.',
\   },
\   'qmlfmt': {
\       'function': 'ale#fixers#qmlfmt#Fix',
\       'suggested_filetypes': ['qml'],
\       'description': 'Fix QML files with qmlfmt.',
\   },
\   'dartfmt': {
\       'function': 'ale#fixers#dartfmt#Fix',
\       'suggested_filetypes': ['dart'],
\       'description': 'Fix Dart files with dartfmt.',
\   },
\}

" Reset the function registry to the default entries.
function! ale#fix#registry#ResetToDefaults() abort
    let s:entries = deepcopy(s:default_registry)
    let s:aliases = {}

    " Set up aliases for fixers too.
    for [l:key, l:entry] in items(s:entries)
        for l:alias in get(l:entry, 'aliases', [])
            let s:aliases[l:alias] = l:key
        endfor
    endfor
endfunction

" Set up entries now.
call ale#fix#registry#ResetToDefaults()

" Remove everything from the registry, useful for tests.
function! ale#fix#registry#Clear() abort
    let s:entries = {}
    let s:aliases = {}
endfunction

" Add a function for fixing problems to the registry.
" (name, func, filetypes, desc, aliases)
function! ale#fix#registry#Add(name, func, filetypes, desc, ...) abort
    if type(a:name) != type('')
        throw '''name'' must be a String'
    endif

    if type(a:func) != type('')
        throw '''func'' must be a String'
    endif

    if type(a:filetypes) != type([])
        throw '''filetypes'' must be a List'
    endif

    for l:type in a:filetypes
        if type(l:type) != type('')
            throw 'Each entry of ''filetypes'' must be a String'
        endif
    endfor

    if type(a:desc) != type('')
        throw '''desc'' must be a String'
    endif

    let l:aliases = get(a:000, 0, [])

    if type(l:aliases) != type([])
    \|| !empty(filter(copy(l:aliases), 'type(v:val) != type('''')'))
        throw '''aliases'' must be a List of String values'
    endif

    let s:entries[a:name] = {
    \   'function': a:func,
    \   'suggested_filetypes': a:filetypes,
    \   'description': a:desc,
    \}

    " Set up aliases for the fixer.
    if !empty(l:aliases)
        let s:entries[a:name].aliases = l:aliases

        for l:alias in l:aliases
            let s:aliases[l:alias] = a:name
        endfor
    endif
endfunction

" Get a function from the registry by its short name.
function! ale#fix#registry#GetFunc(name) abort
    " Use the exact name, or an alias.
    let l:resolved_name = !has_key(s:entries, a:name)
    \   ? get(s:aliases, a:name, a:name)
    \   : a:name

    return get(s:entries, l:resolved_name, {'function': ''}).function
endfunction

function! s:ShouldSuggestForType(suggested_filetypes, type_list) abort
    for l:type in a:type_list
        if index(a:suggested_filetypes, l:type) >= 0
            return 1
        endif
    endfor

    return 0
endfunction

function! s:IsGenericFixer(suggested_filetypes) abort
    if empty(a:suggested_filetypes)
        return 1
    endif

    return 0
endfunction

function! s:FormatEntry(key, entry) abort
    let l:aliases_str = ''

    " Show aliases in :ALEFixSuggest if they are there.
    if !empty(get(a:entry, 'aliases', []))
        let l:aliases_str = ', ' . join(
        \   map(copy(a:entry.aliases), 'string(v:val)'),
        \   ','
        \)
    endif

    return printf(
    \   '%s%s - %s',
    \   string(a:key),
    \   l:aliases_str,
    \   a:entry.description,
    \)
endfunction

" Get list of applicable fixers for filetype, including generic fixers
function! ale#fix#registry#GetApplicableFixers(filetype) abort
    let l:type_list = split(a:filetype, '\.')
    let l:fixer_name_list = []

    for l:key in sort(keys(s:entries))
        let l:suggested_filetypes = s:entries[l:key].suggested_filetypes

        if s:IsGenericFixer(l:suggested_filetypes) || s:ShouldSuggestForType(l:suggested_filetypes, l:type_list)
            call add(l:fixer_name_list, l:key)
        endif
    endfor

    return l:fixer_name_list
endfunction

" Function that returns autocomplete candidates for ALEFix command
function! ale#fix#registry#CompleteFixers(ArgLead, CmdLine, CursorPos) abort
    return filter(ale#fix#registry#GetApplicableFixers(&filetype), 'v:val =~? a:ArgLead')
endfunction

function! ale#fix#registry#SuggestedFixers(filetype) abort
    let l:type_list = split(a:filetype, '\.')
    let l:filetype_fixer_list = []

    for l:key in sort(keys(s:entries))
        let l:suggested_filetypes = s:entries[l:key].suggested_filetypes

        if s:ShouldSuggestForType(l:suggested_filetypes, l:type_list)
            call add(
            \   l:filetype_fixer_list,
            \   s:FormatEntry(l:key, s:entries[l:key]),
            \)
        endif
    endfor

    let l:generic_fixer_list = []

    for l:key in sort(keys(s:entries))
        if s:IsGenericFixer(s:entries[l:key].suggested_filetypes)
            call add(
            \   l:generic_fixer_list,
            \   s:FormatEntry(l:key, s:entries[l:key]),
            \)
        endif
    endfor

    return [l:filetype_fixer_list, l:generic_fixer_list]
endfunction

" Suggest functions to use from the registry.
function! ale#fix#registry#Suggest(filetype) abort
    let l:suggested = ale#fix#registry#SuggestedFixers(a:filetype)
    let l:filetype_fixer_list = l:suggested[0]
    let l:generic_fixer_list = l:suggested[1]

    let l:filetype_fixer_header = !empty(l:filetype_fixer_list)
    \   ? ['Try the following fixers appropriate for the filetype:', '']
    \   : []
    let l:generic_fixer_header = !empty(l:generic_fixer_list)
    \   ? ['Try the following generic fixers:', '']
    \   : []

    let l:has_both_lists = !empty(l:filetype_fixer_list) && !empty(l:generic_fixer_list)

    let l:lines =
    \   l:filetype_fixer_header
    \   + l:filetype_fixer_list
    \   + (l:has_both_lists ? [''] : [])
    \   + l:generic_fixer_header
    \   + l:generic_fixer_list

    if empty(l:lines)
        let l:lines = ['There is nothing in the registry to suggest.']
    else
        let l:lines += ['', 'See :help ale-fix-configuration']
    endif

    let l:lines += ['', 'Press q to close this window']

    new +set\ filetype=ale-fix-suggest
    call setline(1, l:lines)
    setlocal nomodified
    setlocal nomodifiable
endfunction
