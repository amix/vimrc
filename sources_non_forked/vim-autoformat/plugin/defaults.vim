
"
" This file contains default settings and all format program definitions and links these to filetypes
"


" Vim-autoformat configuration variables
if !exists('g:autoformat_autoindent')
    let g:autoformat_autoindent = 1
endif

if !exists('g:autoformat_retab')
    let g:autoformat_retab = 1
endif

if !exists('g:autoformat_remove_trailing_spaces')
    let g:autoformat_remove_trailing_spaces = 1
endif

if !exists('g:autoformat_verbosemode')
    let g:autoformat_verbosemode = 0
endif


" Ada
if !exists('g:formatdef_gnatpp')
    let g:formatdef_gnatpp = "'cat > /tmp/adafile; gnatpp --pipe /tmp/adafile; rm -f /tmp/adafile'"
endif

if !exists('g:formatters_ada')
    let g:formatters_ada = ['gnatpp']
endif

" Python
if !exists('g:formatdef_autopep8')
    " Autopep8 will not do indentation fixes when a range is specified, so we
    " only pass a range when there is a visual selection that is not the
    " entire file. See #125.
    let g:formatdef_autopep8 = '"autopep8 -".(g:DoesRangeEqualBuffer(a:firstline, a:lastline) ? " --range ".a:firstline." ".a:lastline : "")." ".(&textwidth ? "--max-line-length=".&textwidth : "")'
endif

" There doesn't seem to be a reliable way to detect if are in some kind of visual mode,
" so we use this as a workaround. We compare the length of the file against
" the range arguments. If there is no range given, the range arguments default
" to the entire file, so we return false if the range comprises the entire file.
function! g:DoesRangeEqualBuffer(first, last)
    return line('$') != a:last - a:first + 1
endfunction

" Yapf supports multiple formatter styles: pep8, google, chromium, or facebook
if !exists('g:formatter_yapf_style')
    let g:formatter_yapf_style = 'pep8'
endif
if !exists('g:formatdef_yapf')
    let s:configfile_def   = "'yapf -l '.a:firstline.'-'.a:lastline"
    let s:noconfigfile_def = "'yapf --style=\"{based_on_style:'.g:formatter_yapf_style.',indent_width:'.shiftwidth().(&textwidth ? ',column_limit:'.&textwidth : '').'}\" -l '.a:firstline.'-'.a:lastline"
    let g:formatdef_yapf   = "g:YAPFFormatConfigFileExists() ? (" . s:configfile_def . ") : (" . s:noconfigfile_def . ")"
endif

function! g:YAPFFormatConfigFileExists()
    return len(findfile(".style.yapf", expand("%:p:h").";")) || len(findfile("setup.cfg", expand("%:p:h").";")) || filereadable(exists('$XDG_CONFIG_HOME') ? expand('$XDG_CONFIG_HOME/yapf/style') : expand('~/.config/yapf/style'))
endfunction

if !exists('g:formatdef_black')
    let g:formatdef_black = '"black -q ".(&textwidth ? "-l".&textwidth : "")." -"'
endif

if !exists('g:formatters_python')
    let g:formatters_python = ['autopep8','yapf', 'black']
endif


" C#
if !exists('g:formatdef_astyle_cs')
    if filereadable('.astylerc')
        let g:formatdef_astyle_cs = '"astyle --mode=cs --options=.astylerc"'
    elseif filereadable(expand('~/.astylerc')) || exists('$ARTISTIC_STYLE_OPTIONS')
        let g:formatdef_astyle_cs = '"astyle --mode=cs"'
    else
        let g:formatdef_astyle_cs = '"astyle --mode=cs --style=ansi --indent-namespaces -pcH".(&expandtab ? "s".shiftwidth() : "t")'
    endif
endif

if !exists('g:formatters_cs')
    let g:formatters_cs = ['astyle_cs']
endif

if !exists('g:formatters_bzl')
    let g:formatters_bzl = ['buildifier']
endif


" Generic C, C++, Objective-C
if !exists('g:formatdef_clangformat')
    let s:configfile_def = "'clang-format -lines='.a:firstline.':'.a:lastline.' --assume-filename=\"'.expand('%:p').'\" -style=file'"
    let s:noconfigfile_def = "'clang-format -lines='.a:firstline.':'.a:lastline.' --assume-filename=\"'.expand('%:p').'\" -style=\"{BasedOnStyle: WebKit, AlignTrailingComments: true, '.(&textwidth ? 'ColumnLimit: '.&textwidth.', ' : '').'IndentWidth: '.shiftwidth().', TabWidth: '.&tabstop.', '.(&expandtab ? 'UseTab: Never' : 'UseTab: Always').'}\"'"
    let g:formatdef_clangformat = "g:ClangFormatConfigFileExists() ? (" . s:configfile_def . ") : (" . s:noconfigfile_def . ")"
endif

function! g:ClangFormatConfigFileExists()
    return len(findfile(".clang-format", expand("%:p:h").";")) || len(findfile("_clang-format", expand("%:p:h").";")) || len(findfile("~/.clang-format", expand("%:p:h").";")) || len(findfile("~/_clang-format", expand("%:p:h").";"))
endfunction



" C
if !exists('g:formatdef_astyle_c')
    if filereadable('.astylerc')
        let g:formatdef_astyle_c = '"astyle --mode=c --options=.astylerc"'
    elseif filereadable(expand('~/.astylerc')) || exists('$ARTISTIC_STYLE_OPTIONS')
        let g:formatdef_astyle_c = '"astyle --mode=c"'
    else
        let g:formatdef_astyle_c = '"astyle --mode=c --style=ansi -pcH".(&expandtab ? "s".shiftwidth() : "t")'
    endif
endif

if !exists('g:formatters_c')
    let g:formatters_c = ['clangformat', 'astyle_c']
endif


" C++
if !exists('g:formatdef_astyle_cpp')
    if filereadable('.astylerc')
        let g:formatdef_astyle_cpp = '"astyle --mode=c --options=.astylerc"'
    elseif filereadable(expand('~/.astylerc')) || exists('$ARTISTIC_STYLE_OPTIONS')
        let g:formatdef_astyle_cpp = '"astyle --mode=c"'
    else
        let g:formatdef_astyle_cpp = '"astyle --mode=c --style=ansi -pcH".(&expandtab ? "s".shiftwidth() : "t")'
    endif
endif

if !exists('g:formatters_cpp')
    let g:formatters_cpp = ['clangformat', 'astyle_cpp']
endif


" Objective C
if !exists('g:formatters_objc')
    let g:formatters_objc = ['clangformat']
endif


" D
if !exists('g:formatdef_dfmt')
    if executable('dfmt')
        let s:dfmt_command = 'dfmt'
    else
        let s:dfmt_command = 'dub run -q dfmt --'
    endif

    let s:configfile_def = '"' . s:dfmt_command . '"'
    let s:noconfigfile_def = '"' . s:dfmt_command . ' -t " . (&expandtab ? "space" : "tab") . " --indent_size " . shiftwidth() . (&textwidth ? " --soft_max_line_length " . &textwidth : "")'

    let g:formatdef_dfmt = 'g:EditorconfigFileExists() ? (' . s:configfile_def . ') : (' . s:noconfigfile_def . ')'
    let g:formatters_d = ['dfmt']
endif

function! g:EditorconfigFileExists()
    return len(findfile(".editorconfig", expand("%:p:h").";"))
endfunction


" Protobuf
if !exists('g:formatters_proto')
    let g:formatters_proto = ['clangformat']
endif


" Java
if !exists('g:formatdef_astyle_java')
    if filereadable('.astylerc')
        let g:formatdef_astyle_java = '"astyle --mode=java --options=.astylerc"'
    elseif filereadable(expand('~/.astylerc')) || exists('$ARTISTIC_STYLE_OPTIONS')
        let g:formatdef_astyle_java = '"astyle --mode=java"'
    else
        let g:formatdef_astyle_java = '"astyle --mode=java --style=java -pcH".(&expandtab ? "s".shiftwidth() : "t")'
    endif
endif

if !exists('g:formatters_java')
    let g:formatters_java = ['astyle_java']
endif


" Javascript
if !exists('g:formatdef_jsbeautify_javascript')
    if filereadable('.jsbeautifyrc')
        let g:formatdef_jsbeautify_javascript = '"js-beautify"'
    elseif filereadable(expand('~/.jsbeautifyrc'))
        let g:formatdef_jsbeautify_javascript = '"js-beautify"'
    else
        let g:formatdef_jsbeautify_javascript = '"js-beautify -X -".(&expandtab ? "s ".shiftwidth() : "t").(&textwidth ? " -w ".&textwidth : "")'
    endif
endif

if !exists('g:formatdef_jscs')
    let g:formatdef_jscs = '"jscs -x"'
endif

if !exists('g:formatdef_standard_javascript')
    let g:formatdef_standard_javascript = '"standard --fix --stdin"'
endif


if !exists('g:formatdef_prettier')
    let g:formatdef_prettier = '"prettier --stdin-filepath ".expand("%:p").(&textwidth ? " --print-width ".&textwidth : "")." --tab-width=".shiftwidth()'
endif


" This is an xo formatter (inspired by the above eslint formatter)
" To support ignore and overrides options, we need to use a tmp file
" So we create a tmp file here and then remove it afterwards
if !exists('g:formatdef_xo_javascript')
    function! g:BuildXOLocalCmd()
        return "npx xo --fix --stdin --stdin-filename ".bufname('%')
    endfunction
    let g:formatdef_xo_javascript = "g:BuildXOLocalCmd()"
endif

function! s:NodeJsFindPathToExecFile(exec_name)
    let l:path = fnamemodify(expand('%'), ':p')
    " find formatter & config file
    let l:prog = findfile('node_modules/.bin/'.a:exec_name, l:path.";")
    if empty(l:prog)
        let l:prog = findfile('~/.npm-global/bin/'.a:exec_name)
        if empty(l:prog)
            let l:prog = findfile('/usr/local/bin/'.a:exec_name)
        endif
    else
        let l:prog = getcwd()."/".l:prog
    endif
    return l:prog
endfunction

" Setup ESLint local. Setup is done on formatter execution if ESLint and
" corresponding config is found they are used, otherwiese the formatter fails.
" No windows support at the moment.
if !exists('g:formatdef_eslint_local')
    " returns unique file name near original
    function! g:BuildESLintTmpFile(path, ext)
        let l:i = 0
        let l:result = a:path.'_eslint_tmp_'.l:i.a:ext
        while filereadable(l:result) && l:i < 100000
            let l:i = l:i + 1
            let l:result = a:path.'_eslint_tmp_'.l:i.a:ext
        endwhile
        if filereadable(l:result)
            echoerr "Temporary file could not be created for ".a:path
            echoerr "Tried from ".a:path.'_eslint_tmp_0'.a:ext." to ".a:path.'_eslint_tmp_'.l:i.a:ext
            return ''
        endif
        return l:result
    endfunction

    function! g:BuildESLintLocalCmd()
        let l:path = fnamemodify(expand('%'), ':p')
        let l:ext = ".".expand('%:p:e')
        let verbose = &verbose || g:autoformat_verbosemode == 1
        if has('win32')
            return "(>&2 echo 'ESLint not supported on win32')"
        endif
        " find formatter & config file
        let l:prog = s:NodeJsFindPathToExecFile('eslint')

        "initial
        let l:cfg = findfile('.eslintrc.js', l:path.";")

        if empty(l:cfg)
            let l:cfg_fallbacks = [
                        \'.eslintrc.yaml',
                        \'.eslintrc.yml',
                        \'.eslintrc.json',
                        \'.eslintrc',
                        \]

            for i in l:cfg_fallbacks
                let l:tcfg = findfile(i, l:path.";")
                if !empty(l:tcfg)
                    break
                endif
            endfor

            if !empty(l:tcfg)
                let l:cfg = fnamemodify(l:tcfg, ":p")
            else
                let l:cfg = findfile('~/.eslintrc.js')
                for i in l:cfg_fallbacks
                    if !empty(l:cfg)
                        break
                    endif
                    let l:cfg = findfile("~/".i)
                endfor
            endif
        endif

        if (empty(l:cfg) || empty(l:prog))
            if verbose > 0
                return "(>&2 echo 'No local or global ESLint program and/or config found')"
            endif
            return
        endif

        " This formatter uses a temporary file as ESLint has not option to print
        " the formatted source to stdout without modifieing the file.
        let l:eslint_tmp_file = g:BuildESLintTmpFile(l:path, l:ext)
        let content = getline('1', '$')
        call writefile(content, l:eslint_tmp_file)
        return l:prog." -c ".l:cfg." --fix ".l:eslint_tmp_file." 1> /dev/null; exit_code=$?;
                    \ cat ".l:eslint_tmp_file."; rm -f ".l:eslint_tmp_file."; exit $exit_code"
    endfunction
    let g:formatdef_eslint_local = "g:BuildESLintLocalCmd()"
endif

if !exists('g:formatters_javascript')
    let g:formatters_javascript = [
                \ 'eslint_local',
                \ 'jsbeautify_javascript',
                \ 'jscs',
                \ 'standard_javascript',
                \ 'prettier',
                \ 'xo_javascript',
                \ 'stylelint',
                \ ]
endif

" Vue
if !exists('g:formatters_vue')
    let g:formatters_vue = [
                \ 'eslint_local',
                \ 'stylelint',
                \ ]
endif

" JSON
if !exists('g:formatdef_jsbeautify_json')
    if filereadable('.jsbeautifyrc')
        let g:formatdef_jsbeautify_json = '"js-beautify"'
    elseif filereadable(expand('~/.jsbeautifyrc'))
        let g:formatdef_jsbeautify_json = '"js-beautify"'
    else
        let g:formatdef_jsbeautify_json = '"js-beautify -".(&expandtab ? "s ".shiftwidth() : "t")'
    endif
endif

if !exists('g:formatdef_fixjson')
    let g:formatdef_fixjson =  '"fixjson"'
endif

if !exists('g:formatters_json')
    let g:formatters_json = [
                \ 'jsbeautify_json',
                \ 'fixjson',
                \ 'prettier',
                \ ]
endif


" Julia
if !exists('g:formatdef_juliaformatter')
    function! g:BuildJuliaCmd()
        return 'julia -e "using JuliaFormatter; print(format_text(read(\"' . expand("%:p") . '\", String)))"'
    endfunction
    let g:formatdef_juliaformatter = 'g:BuildJuliaCmd()'
endif

if !exists('g:formatters_julia')
    let g:formatters_julia = ['juliaformatter']
endif


" HTML
if !exists('g:formatdef_htmlbeautify')
    let g:formatdef_htmlbeautify = '"html-beautify - -".(&expandtab ? "s ".shiftwidth() : "t").(&textwidth ? " -w ".&textwidth : "")'
endif

if !exists('g:formatdef_tidy_html')
    let g:formatdef_tidy_html = '"tidy -q --show-errors 0 --show-warnings 0 --force-output --indent auto --indent-spaces ".shiftwidth()." --vertical-space yes --tidy-mark no -wrap ".&textwidth'
endif

if !exists('g:formatters_html')
    let g:formatters_html = ['htmlbeautify', 'tidy_html', 'stylelint']
endif



" XML
if !exists('g:formatdef_tidy_xml')
    let g:formatdef_tidy_xml = '"tidy -q -xml --show-errors 0 --show-warnings 0 --force-output --indent auto --indent-spaces ".shiftwidth()." --vertical-space yes --tidy-mark no -wrap ".&textwidth'
endif

if !exists('g:formatters_xml')
    let g:formatters_xml = ['tidy_xml']
endif

" SVG
if !exists('g:formatters_svg')
    let g:formatters_svg = ['tidy_xml']
endif

" XHTML
if !exists('g:formatdef_tidy_xhtml')
    let g:formatdef_tidy_xhtml = '"tidy -q --show-errors 0 --show-warnings 0 --force-output --indent auto --indent-spaces ".shiftwidth()." --vertical-space yes --tidy-mark no -asxhtml -wrap ".&textwidth'
endif

if !exists('g:formatters_xhtml')
    let g:formatters_xhtml = ['tidy_xhtml']
endif

" Ruby
if !exists('g:formatdef_rbeautify')
    let g:formatdef_rbeautify = '"rbeautify ".(&expandtab ? "-s -c ".shiftwidth() : "-t")'
endif

if !exists('g:formatdef_rubocop')
    " The pipe to sed is required to remove some rubocop output that could not
    " be suppressed.
    let g:formatdef_rubocop = "'rubocop --auto-correct -o /dev/null -s '.bufname('%').' \| sed -n 2,\\$p'"
endif

if !exists('g:formatters_ruby')
    let g:formatters_ruby = ['rbeautify', 'rubocop']
endif


" CSS

" Setup stylelint. Setup is done on formatter execution
" if stylelint is found, otherwise the formatter fails.
" No windows support at the moment.
if !exists('g:formatdef_stylelint')
    function! g:BuildStyleLintCmd()
        let verbose = &verbose || g:autoformat_verbosemode == 1
        if has('win32')
            return "(>&2 echo 'stylelint not supported on win32')"
        endif
        " find formatter
        let l:prog = s:NodeJsFindPathToExecFile('stylelint')

        if (empty(l:prog))
            if verbose > 0
                return "(>&2 echo 'No local or global stylelint program found')"
            endif
            return
        endif

        return l:prog." --fix --stdin --stdin-filename ".bufname('%')
    endfunction
    let g:formatdef_stylelint = "g:BuildStyleLintCmd()"
endif


if !exists('g:formatdef_cssbeautify')
    let g:formatdef_cssbeautify = '"css-beautify -f - -s ".shiftwidth()'
endif

if !exists('g:formatters_css')
    let g:formatters_css = ['cssbeautify', 'prettier', 'stylelint']
endif

" SCSS
if !exists('g:formatdef_sassconvert')
    let g:formatdef_sassconvert = '"sass-convert -F scss -T scss --indent " . (&expandtab ? shiftwidth() : "t")'
endif

if !exists('g:formatters_scss')
    let g:formatters_scss = ['sassconvert', 'prettier', 'stylelint']
endif

" Less
if !exists('g:formatters_less')
    let g:formatters_less = ['prettier', 'stylelint']
endif

" Typescript
if !exists('g:formatdef_tsfmt')
    let g:formatdef_tsfmt = "'tsfmt --stdin '.bufname('%')"
endif

if !exists('g:formatters_typescript')
    let g:formatters_typescript = ['tsfmt', 'prettier']
endif

" Haxe
if !exists('g:formatdef_haxe_formatter')
    let g:formatdef_haxe_formatter = "'haxelib run formatter --stdin --source " . fnamemodify("%", ":p:h") . "'"
endif

if !exists('g:formatters_haxe')
    let g:formatters_haxe = ["haxe_formatter"]
endif

" Golang
" Two definitions are provided for two versions of gofmt.
" See issue #59
if !exists('g:formatdef_gofmt_1')
    let g:formatdef_gofmt_1 = '"gofmt -tabs=".(&expandtab ? "false" : "true")." -tabwidth=".shiftwidth()'
endif

if !exists('g:formatdef_gofmt_2')
    let g:formatdef_gofmt_2 = '"gofmt"'
endif

if !exists('g:formatdef_goimports')
    let g:formatdef_goimports = '"goimports"'
endif

if !exists('g:formatdef_gofumpt')
    let g:formatdef_gofumpt = '"gofumpt"'
endif

if !exists('g:formatters_go')
    let g:formatters_go = ['gofmt_1', 'goimports', 'gofmt_2', 'gofumpt']
endif

" Rust
if !exists('g:formatdef_rustfmt')
    let g:formatdef_rustfmt = '"rustfmt --edition 2018"'
endif

if !exists('g:formatters_rust')
    let g:formatters_rust = ['rustfmt']
endif

" Zig
if !exists('g:formatdef_zigfmt')
    let g:formatdef_zigfmt = '"zig fmt --stdin"'
endif

if !exists('g:formatters_zig')
    let g:formatters_zig = ['zigfmt']
endif

" Dart
if !exists('g:formatdef_dart_format')
    let g:formatdef_dart_format = '"dart format"'
endif

if !exists('g:formatters_dart')
    let g:formatters_dart = ['dart_format']
endif

" Perl
if !exists('g:formatdef_perltidy')
    " use perltidyrc file if readable
    if (has("win32") && (filereadable("perltidy.ini") ||
                \ filereadable($HOMEPATH."/perltidy.ini"))) ||
                \ ((has("unix") ||
                \ has("mac")) && (filereadable(".perltidyrc") ||
                \ filereadable(expand("~/.perltidyrc")) ||
                \ filereadable("/usr/local/etc/perltidyrc") ||
                \ filereadable("/etc/perltidyrc")))
        let g:formatdef_perltidy = '"perltidy -q -st"'
    else
        let g:formatdef_perltidy = '"perltidy --perl-best-practices --format-skipping -q "'
    endif
endif

if !exists('g:formatters_perl')
    let g:formatters_perl = ['perltidy']
endif

" Haskell
if !exists('g:formatdef_stylish_haskell')
    let g:formatdef_stylish_haskell = '"stylish-haskell"'
endif

if !exists('g:formatters_haskell')
    let g:formatters_haskell = ['stylish_haskell']
endif

" Purescript
if !exists('g:formatdef_purty')
    let g:formatdef_purty = '"purty -"'
endif

if !exists('g:formatters_purescript')
    let g:formatters_purescript = ['purty']
endif

" Markdown
if !exists('g:formatdef_remark_markdown')
    let g:formatdef_remark_markdown = '"remark --silent --no-color"'
endif

if !exists('g:formatters_markdown')
    let g:formatters_markdown = ['remark_markdown', 'prettier', 'stylelint']
endif

" Graphql
if !exists('g:formatters_graphql')
    let g:formatters_graphql = ['prettier']
endif

" Fortran
if !exists('g:formatdef_fprettify')
    let g:formatdef_fprettify = '"fprettify --no-report-errors --indent=".shiftwidth()'
endif

if !exists('g:formatters_fortran')
    let g:formatters_fortran = ['fprettify']
endif

" Elixir

if !exists('g:formatters_elixir')
    let s:configfile_def = '"mix format --dot-formatter " . findfile(".formatter.exs", expand("%:p:h").";") . " -"'
    let s:noconfigfile_def = '"mix format -"'

    let g:formatdef_mix_format = 'g:ElixirconfigFileExists() ? (' . s:configfile_def . ') : (' . s:noconfigfile_def . ')'
    let g:formatters_elixir = ['mix_format']
endif


function! g:ElixirconfigFileExists()
  return len(findfile(".formatter.exs", expand("%:p:h").";"))
endfunction

" Shell
if !exists('g:formatdef_shfmt')
    let g:formatdef_shfmt = '"shfmt -i ".(&expandtab ? shiftwidth() : "0")'
endif

if !exists('g:formatters_sh')
    let g:formatters_sh = ['shfmt']
endif

" Fish shell
if !exists('g:formatdef_fish_indent')
    let g:formatdef_fish_indent = '"fish_indent"'
endif

if !exists('g:formatters_fish')
    let g:formatters_fish = ['fish_indent']
endif

" Lua
if !exists('g:formatdef_luafmt')
    let g:formatdef_luafmt = "'luafmt --stdin '.bufname('%')"
endif
if !exists('g:formatdef_stylua')
    let g:formatdef_stylua = "'stylua --search-parent-directories --stdin-filepath ' . expand('%:p') .' -- -'"
endif
if !exists('g:formatters_lua')
    let g:formatters_lua = ['luafmt', 'stylua']
endif

" SQL
if !exists('g:formatdef_sqlformat')
    let g:formatdef_sqlformat = '"sqlformat --reindent --indent_width ".shiftwidth()." --keywords upper --identifiers lower -"'
endif
if !exists('g:formatters_sql')
    let g:formatters_sql = ['sqlformat']
endif

" CMake
if !exists('g:formatdef_cmake_format')
    let g:formatdef_cmake_format = '"cmake-format - --tab-size ".shiftwidth()." ".(&textwidth ? "--line-width=".&textwidth : "")'
endif

if !exists('g:formatters_cmake')
    let g:formatters_cmake = ['cmake_format']
endif

" Latex
if !exists('g:formatdef_latexindent')
    let g:formatdef_latexindent = '"latexindent.pl -"'
endif

if !exists('g:formatters_latex')
    let g:formatters_tex = ['latexindent']
endif

" OCaml
if !exists('g:formatdef_ocp_indent')
    let g:formatdef_ocp_indent = '"ocp-indent"'
endif

if !exists('g:formatdef_ocamlformat')
    if filereadable('.ocamlformat')
        let g:formatdef_ocamlformat = '"ocamlformat --enable-outside-detected-project --name " . expand("%:p") . " -"'
    else
        let g:formatdef_ocamlformat = '"ocamlformat --profile=ocamlformat --enable-outside-detected-project --name " . expand("%:p") . " -"'
    endif
endif

if !exists('g:formatters_ocaml')
    let g:formatters_ocaml = ['ocamlformat', 'ocp_indent']
endif

" Assembly
if !exists('g:formatdef_asm_format')
    let g:formatdef_asm_format = '"asmfmt"'
endif

if !exists('g:formatters_asm')
    let g:formatters_asm = ['asm_format']
endif

" Nix
if !exists('g:formatdef_nix_format')
    let g:formatdef_nix_format = '"nixfmt"'
endif

if !exists('g:formatters_nix')
    let g:formatters_nix = ['nix_format']
endif

" Dhall
if !exists('g:formatdef_dhall_format')
    let g:formatdef_dhall_format = '"dhall --ascii format"'
endif

if !exists('g:formatters_dhall')
    let g:formatters_dhall = ['dhall_format']
endif

" Terraform
if !exists('g:formatdef_terraform_format')
    let g:formatdef_terraform_format = '"terraform fmt -"'
endif

if !exists('g:formatters_terraform')
    let g:formatters_terraform = ['terraform_format']
endif

" Packer
if !exists('g:formatdef_packer_format')
    let g:formatdef_packer_format = '"packer fmt -"'
endif

if !exists('g:formatters_packer')
    let g:formatters_packer = ['packer_format']
endif

" Nginx
if !exists('g:formatdef_nginxfmt')
    let g:formatdef_nginxfmt = '"nginxfmt.py -i ".shiftwidth()." -"'
endif

if !exists('g:formatters_nginx')
    let g:formatters_nginx = ['nginxfmt']
endif
