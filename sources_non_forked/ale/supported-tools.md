# ALE Supported Languages and Tools

This plugin supports the following languages and tools. All available
tools will be run in combination, so they can be complementary.

<!--
Keep the table rows sorted alphabetically by the language name,
and the tools in the tools column sorted alphabetically by the tool
name. That seems to be the fairest way to arrange this table.

Remember to also update doc/ale.txt, which has a similar list with different
formatting.
-->

**Legend**

|      Key      |             Definition           |
| ------------- | -------------------------------- |
| :floppy_disk: | Only checked when saved to disk  |
|   :warning:   | Disabled by default              |

---

* Ada
  * [gcc](https://gcc.gnu.org)
  * [gnatpp](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/gnat_utility_programs.html#the-gnat-pretty-printer-gnatpp) :floppy_disk:
* Ansible
  * [ansible-lint](https://github.com/willthames/ansible-lint)
* API Blueprint
  * [drafter](https://github.com/apiaryio/drafter)
* AsciiDoc
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [proselint](http://proselint.com/)
  * [redpen](http://redpen.cc/)
  * [textlint](https://textlint.github.io/)
  * [vale](https://github.com/ValeLint/vale)
  * [write-good](https://github.com/btford/write-good)
* ASM
  * [gcc](https://gcc.gnu.org)
* Awk
  * [gawk](https://www.gnu.org/software/gawk/)
* Bash
  * [language-server](https://github.com/mads-hartmann/bash-language-server)
  * shell [-n flag](https://www.gnu.org/software/bash/manual/bash.html#index-set)
  * [shellcheck](https://www.shellcheck.net/)
  * [shfmt](https://github.com/mvdan/sh)
* BibTeX
  * [bibclean](http://ftp.math.utah.edu/pub/bibclean/)
* Bourne Shell
  * shell [-n flag](http://linux.die.net/man/1/sh)
  * [shellcheck](https://www.shellcheck.net/)
  * [shfmt](https://github.com/mvdan/sh)
* C
  * [ccls](https://github.com/MaskRay/ccls)
  * [clang](http://clang.llvm.org/)
  * [clangd](https://clang.llvm.org/extra/clangd.html)
  * [clang-format](https://clang.llvm.org/docs/ClangFormat.html)
  * [clangtidy](http://clang.llvm.org/extra/clang-tidy/) :floppy_disk:
  * [cppcheck](http://cppcheck.sourceforge.net)
  * [cpplint](https://github.com/google/styleguide/tree/gh-pages/cpplint)
  * [cquery](https://github.com/cquery-project/cquery)
  * [flawfinder](https://www.dwheeler.com/flawfinder/)
  * [gcc](https://gcc.gnu.org/)
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* C#
  * [csc](http://www.mono-project.com/docs/about-mono/languages/csharp/) :floppy_disk: see:`help ale-cs-csc` for details and configuration
  * [mcs](http://www.mono-project.com/docs/about-mono/languages/csharp/) see:`help ale-cs-mcs` for details
  * [mcsc](http://www.mono-project.com/docs/about-mono/languages/csharp/) :floppy_disk: see:`help ale-cs-mcsc` for details and configuration
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* C++ (filetype cpp)
  * [ccls](https://github.com/MaskRay/ccls)
  * [clang](http://clang.llvm.org/)
  * [clangcheck](http://clang.llvm.org/docs/ClangCheck.html) :floppy_disk:
  * [clangd](https://clang.llvm.org/extra/clangd.html)
  * [clang-format](https://clang.llvm.org/docs/ClangFormat.html)
  * [clangtidy](http://clang.llvm.org/extra/clang-tidy/) :floppy_disk:
  * [clazy](https://github.com/KDE/clazy) :floppy_disk:
  * [cppcheck](http://cppcheck.sourceforge.net)
  * [cpplint](https://github.com/google/styleguide/tree/gh-pages/cpplint) :floppy_disk:
  * [cquery](https://github.com/cquery-project/cquery)
  * [flawfinder](https://www.dwheeler.com/flawfinder/)
  * [gcc](https://gcc.gnu.org/)
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* Chef
  * [cookstyle](https://docs.chef.io/cookstyle.html)
  * [foodcritic](http://www.foodcritic.io/)
* Clojure
  * [clj-kondo](https://github.com/borkdude/clj-kondo)
  * [joker](https://github.com/candid82/joker)
* CloudFormation
  * [cfn-python-lint](https://github.com/awslabs/cfn-python-lint)
* CMake
  * [cmake-format](https://github.com/cheshirekow/cmake_format)
  * [cmakelint](https://github.com/richq/cmake-lint)
* CoffeeScript
  * [coffee](http://coffeescript.org/)
  * [coffeelint](https://www.npmjs.com/package/coffeelint)
* Crystal
  * [ameba](https://github.com/veelenga/ameba) :floppy_disk:
  * [crystal](https://crystal-lang.org/) :floppy_disk:
* CSS
  * [csslint](http://csslint.net/)
  * [fecs](http://fecs.baidu.com/)
  * [prettier](https://github.com/prettier/prettier)
  * [stylelint](https://github.com/stylelint/stylelint)
* Cucumber
  * [cucumber](https://cucumber.io/)
* CUDA
  * [nvcc](http://docs.nvidia.com/cuda/cuda-compiler-driver-nvcc/index.html)
* Cypher
  * [cypher-lint](https://github.com/cleishm/libcypher-parser)
* Cython (pyrex filetype)
  * [cython](http://cython.org/)
* D
  * [dfmt](https://github.com/dlang-community/dfmt)
  * [dls](https://github.com/d-language-server/dls)
  * [dmd](https://dlang.org/dmd-linux.html)
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* Dafny
  * [dafny](https://rise4fun.com/Dafny) :floppy_disk:
* Dart
  * [dartanalyzer](https://github.com/dart-lang/sdk/tree/master/pkg/analyzer_cli) :floppy_disk:
  * [dartfmt](https://github.com/dart-lang/sdk/tree/master/utils/dartfmt)
  * [language_server](https://github.com/natebosch/dart_language_server)
* Dockerfile
  * [dockerfile_lint](https://github.com/projectatomic/dockerfile_lint)
  * [hadolint](https://github.com/hadolint/hadolint)
* Elixir
  * [credo](https://github.com/rrrene/credo)
  * [dialyxir](https://github.com/jeremyjh/dialyxir) :floppy_disk:
  * [dogma](https://github.com/lpil/dogma) :floppy_disk:
  * [elixir-ls](https://github.com/JakeBecker/elixir-ls) :warning:
  * [mix](https://hexdocs.pm/mix/Mix.html) :warning: :floppy_disk:
* Elm
  * [elm-format](https://github.com/avh4/elm-format)
  * [elm-ls](https://github.com/elm-tooling/elm-language-server)
  * [elm-make](https://github.com/elm/compiler)
* Erb
  * [erb](https://apidock.com/ruby/ERB)
  * [erubi](https://github.com/jeremyevans/erubi)
  * [erubis](https://github.com/kwatch/erubis)
  * [ruumba](https://github.com/ericqweinstein/ruumba)
* Erlang
  * [erlc](http://erlang.org/doc/man/erlc.html)
  * [SyntaxErl](https://github.com/ten0s/syntaxerl)
* Fish
  * fish [-n flag](https://linux.die.net/man/1/fish)
* Fortran
  * [gcc](https://gcc.gnu.org/)
  * [language_server](https://github.com/hansec/fortran-language-server)
* Fountain
  * [proselint](http://proselint.com/)
* FusionScript
  * [fusion-lint](https://github.com/RyanSquared/fusionscript)
* Git Commit Messages
  * [gitlint](https://github.com/jorisroovers/gitlint)
* GLSL
  * [glslang](https://github.com/KhronosGroup/glslang)
  * [glslls](https://github.com/svenstaro/glsl-language-server)
* Go
  * [bingo](https://github.com/saibing/bingo) :warning:
  * [go build](https://golang.org/cmd/go/) :warning: :floppy_disk:
  * [gofmt](https://golang.org/cmd/gofmt/)
  * [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports) :warning:
  * [golangci-lint](https://github.com/golangci/golangci-lint) :warning: :floppy_disk:
  * [golangserver](https://github.com/sourcegraph/go-langserver) :warning:
  * [golint](https://godoc.org/github.com/golang/lint)
  * [gometalinter](https://github.com/alecthomas/gometalinter) :warning: :floppy_disk:
  * [go mod](https://golang.org/cmd/go/) :warning: :floppy_disk:
  * [gopls](https://github.com/golang/go/wiki/gopls) :warning:
  * [gosimple](https://github.com/dominikh/go-tools/tree/master/cmd/gosimple) :warning: :floppy_disk:
  * [gotype](https://godoc.org/golang.org/x/tools/cmd/gotype) :warning: :floppy_disk:
  * [go vet](https://golang.org/cmd/vet/) :floppy_disk:
  * [staticcheck](https://github.com/dominikh/go-tools/tree/master/cmd/staticcheck) :warning: :floppy_disk:
* GraphQL
  * [eslint](http://eslint.org/)
  * [gqlint](https://github.com/happylinks/gqlint)
  * [prettier](https://github.com/prettier/prettier)
* Hack
  * [hack](http://hacklang.org/)
  * [hackfmt](https://github.com/facebook/hhvm/tree/master/hphp/hack/hackfmt)
  * [hhast](https://github.com/hhvm/hhast) :warning: (see `:help ale-integration-hack`)
* Haml
  * [haml-lint](https://github.com/brigade/haml-lint)
* Handlebars
  * [ember-template-lint](https://github.com/rwjblue/ember-template-lint)
* Haskell
  * [brittany](https://github.com/lspitzner/brittany)
  * [cabal-ghc](https://www.haskell.org/cabal/)
  * [floskell](https://github.com/ennocramer/floskell)
  * [ghc](https://www.haskell.org/ghc/)
  * [ghc-mod](https://github.com/DanielG/ghc-mod)
  * [hdevtools](https://hackage.haskell.org/package/hdevtools)
  * [hfmt](https://github.com/danstiner/hfmt)
  * [hie](https://github.com/haskell/haskell-ide-engine)
  * [hindent](https://hackage.haskell.org/package/hindent)
  * [hlint](https://hackage.haskell.org/package/hlint)
  * [stack-build](https://haskellstack.org/) :floppy_disk:
  * [stack-ghc](https://haskellstack.org/)
  * [stylish-haskell](https://github.com/jaspervdj/stylish-haskell)
* HCL
  * [terraform-fmt](https://github.com/hashicorp/terraform)
* HTML
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [fecs](http://fecs.baidu.com/)
  * [html-beautify](https://beautifier.io/)
  * [HTMLHint](http://htmlhint.com/)
  * [prettier](https://github.com/prettier/prettier)
  * [proselint](http://proselint.com/)
  * [tidy](http://www.html-tidy.org/)
  * [write-good](https://github.com/btford/write-good)
* Idris
  * [idris](http://www.idris-lang.org/)
* Ink
  * [ink-language-server](https://github.com/ephread/ink-language-server)
* ISPC
  * [ispc](https://ispc.github.io/) :floppy_disk:
* Java
  * [checkstyle](http://checkstyle.sourceforge.net)
  * [eclipselsp](https://github.com/eclipse/eclipse.jdt.ls)
  * [google-java-format](https://github.com/google/google-java-format)
  * [javac](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
  * [javalsp](https://github.com/georgewfraser/vscode-javac)
  * [PMD](https://pmd.github.io/)
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* JavaScript
  * [eslint](http://eslint.org/)
  * [fecs](http://fecs.baidu.com/)
  * [flow](https://flowtype.org/)
  * [jscs](https://jscs-dev.github.io/)
  * [jshint](http://jshint.com/)
  * [prettier](https://github.com/prettier/prettier)
  * [prettier-eslint](https://github.com/prettier/prettier-eslint-cli)
  * [prettier-standard](https://github.com/sheerun/prettier-standard)
  * [standard](http://standardjs.com/)
  * [tsserver](https://github.com/Microsoft/TypeScript/wiki/Standalone-Server-%28tsserver%29)
  * [xo](https://github.com/sindresorhus/xo)
* JSON
  * [fixjson](https://github.com/rhysd/fixjson)
  * [jq](https://stedolan.github.io/jq/)
  * [jsonlint](http://zaa.ch/jsonlint/)
  * [prettier](https://github.com/prettier/prettier)
* Julia
  * [languageserver](https://github.com/JuliaEditorSupport/LanguageServer.jl)
* Kotlin
  * [kotlinc](https://kotlinlang.org) :floppy_disk:
  * [ktlint](https://ktlint.github.io) :floppy_disk:
  * [languageserver](https://github.com/fwcd/KotlinLanguageServer) see `:help ale-integration-kotlin` for configuration instructions
* LaTeX
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [chktex](http://www.nongnu.org/chktex/)
  * [lacheck](https://www.ctan.org/pkg/lacheck)
  * [proselint](http://proselint.com/)
  * [redpen](http://redpen.cc/)
  * [texlab](https://texlab.netlify.com)
  * [textlint](https://textlint.github.io/)
  * [vale](https://github.com/ValeLint/vale)
  * [write-good](https://github.com/btford/write-good)
* Less
  * [lessc](https://www.npmjs.com/package/less)
  * [prettier](https://github.com/prettier/prettier)
  * [stylelint](https://github.com/stylelint/stylelint)
* LLVM
  * [llc](https://llvm.org/docs/CommandGuide/llc.html)
* Lua
  * [luac](https://www.lua.org/manual/5.1/luac.html)
  * [luacheck](https://github.com/mpeterv/luacheck)
* Mail
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [languagetool](https://languagetool.org/) :floppy_disk:
  * [proselint](http://proselint.com/)
  * [vale](https://github.com/ValeLint/vale)
* Make
  * [checkmake](https://github.com/mrtazz/checkmake)
* Markdown
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [languagetool](https://languagetool.org/) :floppy_disk:
  * [markdownlint](https://github.com/DavidAnson/markdownlint) :floppy_disk:
  * [mdl](https://github.com/mivok/markdownlint)
  * [prettier](https://github.com/prettier/prettier)
  * [proselint](http://proselint.com/)
  * [redpen](http://redpen.cc/)
  * [remark-lint](https://github.com/wooorm/remark-lint)
  * [textlint](https://textlint.github.io/)
  * [vale](https://github.com/ValeLint/vale)
  * [write-good](https://github.com/btford/write-good)
* MATLAB
  * [mlint](https://www.mathworks.com/help/matlab/ref/mlint.html)
* Mercury
  * [mmc](http://mercurylang.org) :floppy_disk:
* NASM
  * [nasm](https://www.nasm.us/) :floppy_disk:
* Nim
  * [nim check](https://nim-lang.org/docs/nimc.html) :floppy_disk:
  * [nimlsp](https://github.com/PMunch/nimlsp)
  * nimpretty
* nix
  * [nix-instantiate](http://nixos.org/nix/manual/#sec-nix-instantiate)
  * [nixpkgs-fmt](https://github.com/nix-community/nixpkgs-fmt)
* nroff
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [proselint](http://proselint.com/)
  * [write-good](https://github.com/btford/write-good)
* Objective-C
  * [ccls](https://github.com/MaskRay/ccls)
  * [clang](http://clang.llvm.org/)
  * [clangd](https://clang.llvm.org/extra/clangd.html)
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* Objective-C++
  * [clang](http://clang.llvm.org/)
  * [clangd](https://clang.llvm.org/extra/clangd.html)
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* OCaml
  * [merlin](https://github.com/the-lambda-church/merlin) see `:help ale-ocaml-merlin` for configuration instructions
  * [ocamlformat](https://github.com/ocaml-ppx/ocamlformat)
  * [ocp-indent](https://github.com/OCamlPro/ocp-indent)
  * [ols](https://github.com/freebroccolo/ocaml-language-server)
* Pawn
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* Perl
  * [perl -c](https://perl.org/) :warning:
  * [perl-critic](https://metacpan.org/pod/Perl::Critic)
  * [perltidy](https://metacpan.org/pod/distribution/Perl-Tidy/bin/perltidy)
* Perl6
  * [perl6 -c](https://perl6.org) :warning:
* PHP
  * [langserver](https://github.com/felixfbecker/php-language-server)
  * [phan](https://github.com/phan/phan) see `:help ale-php-phan` to instructions
  * [phpcbf](https://github.com/squizlabs/PHP_CodeSniffer)
  * [phpcs](https://github.com/squizlabs/PHP_CodeSniffer)
  * [php-cs-fixer](http://cs.sensiolabs.org/)
  * [php -l](https://secure.php.net/)
  * [phpmd](https://phpmd.org)
  * [phpstan](https://github.com/phpstan/phpstan)
  * [psalm](https://getpsalm.org) :floppy_disk:
* PO
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [msgfmt](https://www.gnu.org/software/gettext/manual/html_node/msgfmt-Invocation.html)
  * [proselint](http://proselint.com/)
  * [write-good](https://github.com/btford/write-good)
* Pod
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [proselint](http://proselint.com/)
  * [write-good](https://github.com/btford/write-good)
* Pony
  * [ponyc](https://github.com/ponylang/ponyc)
* PowerShell
  * [powershell](https://github.com/PowerShell/PowerShell) :floppy_disk:
  * [psscriptanalyzer](https://github.com/PowerShell/PSScriptAnalyzer) :floppy_disk:
* Prolog
  * [swipl](https://github.com/SWI-Prolog/swipl-devel)
* proto
  * [protoc-gen-lint](https://github.com/ckaznocha/protoc-gen-lint)
* Pug
  * [pug-lint](https://github.com/pugjs/pug-lint)
* Puppet
  * [languageserver](https://github.com/lingua-pupuli/puppet-editor-services)
  * [puppet](https://puppet.com)
  * [puppet-lint](https://puppet-lint.com)
* PureScript
  * [purescript-language-server](https://github.com/nwolverson/purescript-language-server)
  * [purty](https://gitlab.com/joneshf/purty)
* Python
  * [autopep8](https://github.com/hhatto/autopep8)
  * [bandit](https://github.com/PyCQA/bandit) :warning:
  * [black](https://github.com/ambv/black)
  * [flake8](http://flake8.pycqa.org/en/latest/)
  * [isort](https://github.com/timothycrosley/isort)
  * [mypy](http://mypy-lang.org/)
  * [prospector](https://github.com/PyCQA/prospector) :warning:
  * [pycodestyle](https://github.com/PyCQA/pycodestyle) :warning:
  * [pydocstyle](https://www.pydocstyle.org/) :warning:
  * [pyflakes](https://github.com/PyCQA/pyflakes)
  * [pylama](https://github.com/klen/pylama) :floppy_disk:
  * [pylint](https://www.pylint.org/) :floppy_disk:
  * [pyls](https://github.com/palantir/python-language-server) :warning:
  * [pyre](https://github.com/facebook/pyre-check) :warning:
  * [reorder-python-imports](https://github.com/asottile/reorder_python_imports)
  * [vulture](https://github.com/jendrikseipp/vulture) :warning: :floppy_disk:
  * [yapf](https://github.com/google/yapf)
* QML
  * [qmlfmt](https://github.com/jesperhh/qmlfmt)
  * [qmllint](https://github.com/qt/qtdeclarative/tree/5.11/tools/qmllint)
* R
  * [lintr](https://github.com/jimhester/lintr)
  * [styler](https://github.com/r-lib/styler)
* Racket
  * [raco](https://docs.racket-lang.org/raco/)
* ReasonML
  * [merlin](https://github.com/the-lambda-church/merlin) see `:help ale-reasonml-ols` for configuration instructions
  * [ols](https://github.com/freebroccolo/ocaml-language-server)
  * [reason-language-server](https://github.com/jaredly/reason-language-server)
  * [refmt](https://github.com/reasonml/reason-cli)
* reStructuredText
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [proselint](http://proselint.com/)
  * [redpen](http://redpen.cc/)
  * [rstcheck](https://github.com/myint/rstcheck)
  * [textlint](https://textlint.github.io/)
  * [vale](https://github.com/ValeLint/vale)
  * [write-good](https://github.com/btford/write-good)
* Re:VIEW
  * [redpen](http://redpen.cc/)
* RPM spec
  * [rpmlint](https://github.com/rpm-software-management/rpmlint) :warning: (see `:help ale-integration-spec`)
* Ruby
  * [brakeman](http://brakemanscanner.org/) :floppy_disk:
  * [debride](https://github.com/seattlerb/debride) :floppy_disk:
  * [rails_best_practices](https://github.com/flyerhzm/rails_best_practices) :floppy_disk:
  * [reek](https://github.com/troessner/reek)
  * [rubocop](https://github.com/bbatsov/rubocop)
  * [ruby](https://www.ruby-lang.org)
  * [rufo](https://github.com/ruby-formatter/rufo)
  * [solargraph](https://solargraph.org)
  * [sorbet](https://github.com/sorbet/sorbet)
  * [standardrb](https://github.com/testdouble/standard)
* Rust
  * [cargo](https://github.com/rust-lang/cargo) :floppy_disk: (see `:help ale-integration-rust` for configuration instructions)
  * [rls](https://github.com/rust-lang-nursery/rls) :warning:
  * [rustc](https://www.rust-lang.org/) :warning:
  * [rustfmt](https://github.com/rust-lang-nursery/rustfmt)
* Sass
  * [sass-lint](https://www.npmjs.com/package/sass-lint)
  * [stylelint](https://github.com/stylelint/stylelint)
* Scala
  * [fsc](https://www.scala-lang.org/old/sites/default/files/linuxsoft_archives/docu/files/tools/fsc.html)
  * [metals](https://scalameta.org/metals/)
  * [sbtserver](https://www.scala-sbt.org/1.x/docs/sbt-server.html)
  * [scalac](http://scala-lang.org)
  * [scalafmt](https://scalameta.org/scalafmt/)
  * [scalastyle](http://www.scalastyle.org)
* SCSS
  * [prettier](https://github.com/prettier/prettier)
  * [sass-lint](https://www.npmjs.com/package/sass-lint)
  * [scss-lint](https://github.com/brigade/scss-lint)
  * [stylelint](https://github.com/stylelint/stylelint)
* Slim
  * [slim-lint](https://github.com/sds/slim-lint)
* SML
  * [smlnj](http://www.smlnj.org/)
* Solidity
  * [solc](https://solidity.readthedocs.io/)
  * [solhint](https://github.com/protofire/solhint)
  * [solium](https://github.com/duaraghav8/Solium)
* SQL
  * [pgformatter](https://github.com/darold/pgFormatter)
  * [sqlfmt](https://github.com/jackc/sqlfmt)
  * [sqlformat](https://github.com/andialbrecht/sqlparse)
  * [sqlint](https://github.com/purcell/sqlint)
* Stylus
  * [stylelint](https://github.com/stylelint/stylelint)
* SugarSS
  * [stylelint](https://github.com/stylelint/stylelint)
* Swift
  * [sourcekit-lsp](https://github.com/apple/sourcekit-lsp)
  * [swiftformat](https://github.com/nicklockwood/SwiftFormat)
  * [swiftlint](https://github.com/realm/SwiftLint)
* Tcl
  * [nagelfar](http://nagelfar.sourceforge.net) :floppy_disk:
* Terraform
  * [fmt](https://github.com/hashicorp/terraform)
  * [tflint](https://github.com/wata727/tflint)
* Texinfo
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [proselint](http://proselint.com/)
  * [write-good](https://github.com/btford/write-good)
* Text
  * [alex](https://github.com/wooorm/alex) :warning: :floppy_disk:
  * [languagetool](https://languagetool.org/) :floppy_disk:
  * [proselint](http://proselint.com/) :warning:
  * [redpen](http://redpen.cc/) :warning:
  * [textlint](https://textlint.github.io/) :warning:
  * [vale](https://github.com/ValeLint/vale) :warning:
  * [write-good](https://github.com/btford/write-good) :warning:
* Thrift
  * [thrift](http://thrift.apache.org/)
* TypeScript
  * [eslint](http://eslint.org/)
  * [fecs](http://fecs.baidu.com/)
  * [prettier](https://github.com/prettier/prettier)
  * [standard](http://standardjs.com/)
  * [tslint](https://github.com/palantir/tslint)
  * [tsserver](https://github.com/Microsoft/TypeScript/wiki/Standalone-Server-%28tsserver%29)
  * typecheck
* VALA
  * [uncrustify](https://github.com/uncrustify/uncrustify)
* Verilog
  * [iverilog](https://github.com/steveicarus/iverilog)
  * [verilator](http://www.veripool.org/projects/verilator/wiki/Intro)
  * [vlog](https://www.mentor.com/products/fv/questa/)
  * [xvlog](https://www.xilinx.com/products/design-tools/vivado.html)
* VHDL
  * [ghdl](https://github.com/ghdl/ghdl)
  * [vcom](https://www.mentor.com/products/fv/questa/)
  * [xvhdl](https://www.xilinx.com/products/design-tools/vivado.html)
* Vim
  * [vint](https://github.com/Kuniwak/vint)
* Vim help
  * [alex](https://github.com/wooorm/alex) :warning: :floppy_disk:
  * [proselint](http://proselint.com/) :warning:
  * [write-good](https://github.com/btford/write-good) :warning:
* Vue
  * [prettier](https://github.com/prettier/prettier)
  * [vls](https://github.com/vuejs/vetur/tree/master/server)
* XHTML
  * [alex](https://github.com/wooorm/alex) :floppy_disk:
  * [proselint](http://proselint.com/)
  * [write-good](https://github.com/btford/write-good)
* XML
  * [xmllint](http://xmlsoft.org/xmllint.html)
* YAML
  * [prettier](https://github.com/prettier/prettier)
  * [swaglint](https://github.com/byCedric/swaglint)
  * [yamllint](https://yamllint.readthedocs.io/)
* YANG
  * [yang-lsp](https://github.com/theia-ide/yang-lsp)
