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
\   'autoimport': {
\       'function': 'ale#fixers#autoimport#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Fix import issues with autoimport.',
\   },
\   'autoflake': {
\       'function': 'ale#fixers#autoflake#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Fix flake issues with autoflake.',
\   },
\   'autopep8': {
\       'function': 'ale#fixers#autopep8#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Fix PEP8 issues with autopep8.',
\   },
\   'bibclean': {
\       'function': 'ale#fixers#bibclean#Fix',
\       'suggested_filetypes': ['bib'],
\       'description': 'Format bib files using bibclean.',
\   },
\   'black': {
\       'function': 'ale#fixers#black#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Fix PEP8 issues with black.',
\   },
\   'buildifier': {
\       'function': 'ale#fixers#buildifier#Fix',
\       'suggested_filetypes': ['bzl'],
\       'description': 'Format BUILD and .bzl files with buildifier.',
\   },
\   'deno': {
\       'function': 'ale#fixers#deno#Fix',
\       'suggested_filetypes': ['typescript'],
\       'description': 'Fix TypeScript using deno fmt.',
\   },
\   'dfmt': {
\       'function': 'ale#fixers#dfmt#Fix',
\       'suggested_filetypes': ['d'],
\       'description': 'Fix D files with dfmt.',
\   },
\   'dhall': {
\       'function': 'ale#fixers#dhall#Fix',
\       'suggested_filetypes': ['dhall'],
\       'description': 'Fix Dhall files with dhall-format.',
\   },
\   'dhall-format': {
\       'function': 'ale#fixers#dhall_format#Fix',
\       'suggested_filetypes': ['dhall'],
\       'description': 'Standard code formatter for the Dhall language',
\       'aliases': ['dhall'],
\   },
\   'dhall-freeze': {
\       'function': 'ale#fixers#dhall_freeze#Freeze',
\       'suggested_filetypes': ['dhall'],
\       'description': 'Add integrity checks to remote import statements of an expression for the Dhall language',
\   },
\   'dhall-lint': {
\       'function': 'ale#fixers#dhall_lint#Fix',
\       'suggested_filetypes': ['dhall'],
\       'description': 'Standard code formatter for the Dhall language and removing dead code',
\   },
\   'fecs': {
\       'function': 'ale#fixers#fecs#Fix',
\       'suggested_filetypes': ['javascript', 'css', 'html'],
\       'description': 'Apply fecs format to a file.',
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
\   'nimpretty': {
\       'function': 'ale#fixers#nimpretty#Fix',
\       'suggested_filetypes': ['nim'],
\       'description': 'Apply nimpretty to a file.',
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
\       'suggested_filetypes': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'json5', 'graphql', 'markdown', 'vue', 'svelte', 'html', 'yaml', 'openapi', 'ruby'],
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
\   'yamlfix': {
\       'function': 'ale#fixers#yamlfix#Fix',
\       'suggested_filetypes': ['yaml'],
\       'description': 'Fix yaml files with yamlfix.',
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
\       'suggested_filetypes': ['sbt', 'scala'],
\       'description': 'Fix Scala files using scalafmt',
\   },
\   'sorbet': {
\       'function': 'ale#fixers#sorbet#Fix',
\       'suggested_filetypes': ['ruby'],
\       'description': 'Fix ruby files with srb tc --autocorrect.',
\   },
\   'standard': {
\       'function': 'ale#fixers#standard#Fix',
\       'suggested_filetypes': ['javascript'],
\       'description': 'Fix JavaScript files using standard --fix',
\   },
\   'standardrb': {
\       'function': 'ale#fixers#standardrb#Fix',
\       'suggested_filetypes': ['ruby'],
\       'description': 'Fix ruby files with standardrb --fix',
\   },
\   'stylelint': {
\       'function': 'ale#fixers#stylelint#Fix',
\       'suggested_filetypes': ['css', 'sass', 'scss', 'sugarss', 'stylus'],
\       'description': 'Fix stylesheet files using stylelint --fix.',
\   },
\   'swiftformat': {
\       'function': 'ale#fixers#swiftformat#Fix',
\       'suggested_filetypes': ['swift'],
\       'description': 'Apply SwiftFormat to a file.',
\   },
\   'apple-swift-format': {
\       'function': 'ale#fixers#appleswiftformat#Fix',
\       'suggested_filetypes': ['swift'],
\       'description': 'Apply apple/swift-format to a file.',
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
\   'astyle': {
\       'function': 'ale#fixers#astyle#Fix',
\       'suggested_filetypes': ['c', 'cpp'],
\       'description': 'Fix C/C++ with astyle.',
\   },
\   'clangtidy': {
\       'function': 'ale#fixers#clangtidy#Fix',
\       'suggested_filetypes': ['c', 'cpp', 'objc'],
\       'description': 'Fix C/C++ and ObjectiveC files with clang-tidy.',
\   },
\   'clang-format': {
\       'function': 'ale#fixers#clangformat#Fix',
\       'suggested_filetypes': ['c', 'cpp', 'cuda'],
\       'description': 'Fix C/C++ and cuda files with clang-format.',
\   },
\   'cmakeformat': {
\       'function': 'ale#fixers#cmakeformat#Fix',
\       'suggested_filetypes': ['cmake'],
\       'description': 'Fix CMake files with cmake-format.',
\   },
\   'fish_indent': {
\       'function': 'ale#fixers#fish_indent#Fix',
\       'suggested_filetypes': ['fish'],
\       'description': 'Format fish scripts using fish_indent.',
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
\   'golines': {
\       'function': 'ale#fixers#golines#Fix',
\       'suggested_filetypes': ['go'],
\        'description': 'Fix Go file long lines with golines',
\   },
\   'gomod': {
\       'function': 'ale#fixers#gomod#Fix',
\       'suggested_filetypes': ['gomod'],
\       'description': 'Fix Go module files with go mod edit -fmt.',
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
\   'textlint': {
\       'function': 'ale#fixers#textlint#Fix',
\       'suggested_filetypes': ['text','markdown','asciidoc','tex'],
\       'description': 'Fix text files with textlint --fix',
\   },
\   'hackfmt': {
\       'function': 'ale#fixers#hackfmt#Fix',
\       'suggested_filetypes': ['hack'],
\       'description': 'Fix Hack files with hackfmt.',
\   },
\   'floskell': {
\       'function': 'ale#fixers#floskell#Fix',
\       'suggested_filetypes': ['haskell'],
\       'description': 'Fix Haskell files with floskell.',
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
\   'hindent': {
\       'function': 'ale#fixers#hindent#Fix',
\       'suggested_filetypes': ['haskell'],
\       'description': 'Fix Haskell files with hindent.',
\   },
\   'hlint': {
\       'function': 'ale#fixers#hlint#Fix',
\       'suggested_filetypes': ['haskell'],
\       'description': 'Refactor Haskell files with hlint.',
\   },
\   'stylish-haskell': {
\       'function': 'ale#fixers#stylish_haskell#Fix',
\       'suggested_filetypes': ['haskell'],
\       'description': 'Refactor Haskell files with stylish-haskell.',
\   },
\   'purs-tidy': {
\       'function': 'ale#fixers#purs_tidy#Fix',
\       'suggested_filetypes': ['purescript'],
\       'description': 'Format PureScript files with purs-tidy.',
\   },
\   'purty': {
\       'function': 'ale#fixers#purty#Fix',
\       'suggested_filetypes': ['purescript'],
\       'description': 'Format PureScript files with purty.',
\   },
\   'ocamlformat': {
\       'function': 'ale#fixers#ocamlformat#Fix',
\       'suggested_filetypes': ['ocaml', 'ocamlinterface'],
\       'description': 'Fix OCaml files with ocamlformat.',
\   },
\   'ocp-indent': {
\       'function': 'ale#fixers#ocp_indent#Fix',
\       'suggested_filetypes': ['ocaml', 'ocamlinterface'],
\       'description': 'Fix OCaml files with ocp-indent.',
\   },
\   'refmt': {
\       'function': 'ale#fixers#refmt#Fix',
\       'suggested_filetypes': ['reason'],
\       'description': 'Fix ReasonML files with refmt.',
\   },
\   'pandoc': {
\       'function': 'ale#fixers#pandoc#Fix',
\       'suggested_filetypes': ['markdown'],
\       'description': 'Fix markdown files with pandoc.',
\   },
\   'shfmt': {
\       'function': 'ale#fixers#shfmt#Fix',
\       'suggested_filetypes': ['sh'],
\       'description': 'Fix sh files with shfmt.',
\   },
\   'sqlfmt': {
\       'function': 'ale#fixers#sqlfmt#Fix',
\       'suggested_filetypes': ['sql'],
\       'description': 'Fix SQL files with sqlfmt.',
\   },
\   'sqlformat': {
\       'function': 'ale#fixers#sqlformat#Fix',
\       'suggested_filetypes': ['sql'],
\       'description': 'Fix SQL files with sqlformat.',
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
\   'protolint': {
\       'function': 'ale#fixers#protolint#Fix',
\       'suggested_filetypes': ['proto'],
\       'description': 'Fix Protocol Buffer files with protolint.',
\   },
\   'perltidy': {
\       'function': 'ale#fixers#perltidy#Fix',
\       'suggested_filetypes': ['perl'],
\       'description': 'Fix Perl files with perltidy.',
\   },
\   'xo': {
\       'function': 'ale#fixers#xo#Fix',
\       'suggested_filetypes': ['javascript', 'typescript'],
\       'description': 'Fix JavaScript/TypeScript files using xo --fix.',
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
\   'dart-format': {
\       'function': 'ale#fixers#dart_format#Fix',
\       'suggested_filetypes': ['dart'],
\       'description': 'Fix Dart files with dart format.',
\   },
\   'dotnet-format': {
\       'function': 'ale#fixers#dotnet_format#Fix',
\       'suggested_filetypes': ['cs'],
\       'description': 'Fix C# files with dotnet format.',
\   },
\   'xmllint': {
\       'function': 'ale#fixers#xmllint#Fix',
\       'suggested_filetypes': ['xml'],
\       'description': 'Fix XML files with xmllint.',
\   },
\   'uncrustify': {
\       'function': 'ale#fixers#uncrustify#Fix',
\       'suggested_filetypes': ['c', 'cpp', 'cs', 'objc', 'objcpp', 'd', 'java', 'p', 'vala' ],
\       'description': 'Fix C, C++, C#, ObjectiveC, ObjectiveC++, D, Java, Pawn, and VALA files with uncrustify.',
\   },
\   'terraform': {
\       'function': 'ale#fixers#terraform#Fix',
\       'suggested_filetypes': ['hcl', 'terraform'],
\       'description': 'Fix tf and hcl files with terraform fmt.',
\   },
\   'ktlint': {
\       'function': 'ale#fixers#ktlint#Fix',
\       'suggested_filetypes': ['kt', 'kotlin'],
\       'description': 'Fix Kotlin files with ktlint.',
\   },
\   'styler': {
\       'function': 'ale#fixers#styler#Fix',
\       'suggested_filetypes': ['r', 'rmarkdown', 'rmd'],
\       'description': 'Fix R files with styler.',
\   },
\   'latexindent': {
\       'function': 'ale#fixers#latexindent#Fix',
\       'suggested_filetypes': ['tex'],
\       'description' : 'Indent code within environments, commands, after headings and within special code blocks.',
\   },
\   'pgformatter': {
\       'function': 'ale#fixers#pgformatter#Fix',
\       'suggested_filetypes': ['sql'],
\       'description': 'A PostgreSQL SQL syntax beautifier',
\   },
\   'reorder-python-imports': {
\       'function': 'ale#fixers#reorder_python_imports#Fix',
\       'suggested_filetypes': ['python'],
\       'description': 'Sort Python imports with reorder-python-imports.',
\   },
\   'gnatpp': {
\       'function': 'ale#fixers#gnatpp#Fix',
\       'suggested_filetypes': ['ada'],
\       'description': 'Format Ada files with gnatpp.',
\   },
\   'nixfmt': {
\       'function': 'ale#fixers#nixfmt#Fix',
\       'suggested_filetypes': ['nix'],
\       'description': 'A nix formatter written in Haskell.',
\   },
\   'nixpkgs-fmt': {
\       'function': 'ale#fixers#nixpkgsfmt#Fix',
\       'suggested_filetypes': ['nix'],
\       'description': 'A formatter for Nix code',
\   },
\   'remark-lint': {
\       'function': 'ale#fixers#remark_lint#Fix',
\       'suggested_filetypes': ['markdown'],
\       'description': 'Fix markdown files with remark-lint',
\   },
\   'html-beautify': {
\       'function': 'ale#fixers#html_beautify#Fix',
\       'suggested_filetypes': ['html', 'htmldjango'],
\       'description': 'Fix HTML files with html-beautify.',
\   },
\   'lua-format': {
\       'function': 'ale#fixers#lua_format#Fix',
\       'suggested_filetypes': ['lua'],
\       'description': 'Fix Lua files with lua-format.',
\   },
\   'luafmt': {
\       'function': 'ale#fixers#luafmt#Fix',
\       'suggested_filetypes': ['lua'],
\       'description': 'Fix Lua files with luafmt.',
\   },
\   'stylua': {
\       'function': 'ale#fixers#stylua#Fix',
\       'suggested_filetypes': ['lua'],
\       'description': 'Fix Lua files with stylua.',
\   },
\   'ormolu': {
\       'function': 'ale#fixers#ormolu#Fix',
\       'suggested_filetypes': ['haskell'],
\       'description': 'A formatter for Haskell source code.',
\   },
\   'jsonnetfmt': {
\       'function': 'ale#fixers#jsonnetfmt#Fix',
\       'suggested_filetypes': ['jsonnet'],
\       'description': 'Fix jsonnet files with jsonnetfmt',
\   },
\   'ptop': {
\       'function': 'ale#fixers#ptop#Fix',
\       'suggested_filetypes': ['pascal'],
\       'description': 'Fix Pascal files with ptop.',
\   },
\   'vfmt': {
\       'function': 'ale#fixers#vfmt#Fix',
\       'suggested_filetypes': ['v'],
\       'description': 'A formatter for V source code.',
\   }
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
    " This command will throw from the sandbox.
    let &l:equalprg=&l:equalprg

    if type(a:name) isnot v:t_string
        throw '''name'' must be a String'
    endif

    if type(a:func) isnot v:t_string
        throw '''func'' must be a String'
    endif

    if type(a:filetypes) isnot v:t_list
        throw '''filetypes'' must be a List'
    endif

    for l:type in a:filetypes
        if type(l:type) isnot v:t_string
            throw 'Each entry of ''filetypes'' must be a String'
        endif
    endfor

    if type(a:desc) isnot v:t_string
        throw '''desc'' must be a String'
    endif

    let l:aliases = get(a:000, 0, [])

    if type(l:aliases) isnot v:t_list
    \|| !empty(filter(copy(l:aliases), 'type(v:val) isnot v:t_string'))
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
