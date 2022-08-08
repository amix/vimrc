# vim-autoformat

Format code with one button press (or automatically on save).

This plugin makes use of external formatting programs to achieve the most decent results.
Check the list of formatprograms below to see which languages are supported by default.
Most formatprograms will obey vim settings, such as `textwidth` and `shiftwidth()`.
You can easily customize existing formatprogram definitions or add your own formatprogram.
When no formatprogram exists (or no formatprogram is installed) for a certain filetype,
vim-autoformat falls back by default to indenting, (using vim's auto indent functionality), retabbing and removing trailing whitespace.

## Requirement
vim-autoformat requires vim to have python support (python 2 or python 3). You can check your vim has python support by running `:echo has("python3")` and `:echo has("python2")`.

#### Neovim
Neovim does not come with python support by default, and additional setup is required.

First install [pynvim](https://github.com/neovim/pynvim)
```
python3 -m pip install pynvim
```

And add the following configuration in your `.vimrc`

```
let g:python3_host_prog="/path/to/python/executable/"
```


## How to install

#### Vundle

Put this in your `.vimrc`.

```vim
Plugin 'vim-autoformat/vim-autoformat'
```

Then restart vim and run `:PluginInstall`. Alternatively, you could run `:source $MYVIMRC`
to reload your `.vimrc` without restarting vim.
To update the plugin to the latest version, you can run `:PluginUpdate`.

#### Pathogen

Download the source and extract in your bundle directory.
Updating has to be done manually, as far as I'm aware.

#### Other

It is highly recommended to use a plugin manager such as Vundle, since this makes it easy to update plugins or uninstall them.
It also keeps your .vim directory clean.
Still you can decide to download this repository as a zip file or whatever and extract it to your .vim folder.

## How to use

First you should install an external program that can format code of the programming language you are using.
This can either be one of the programs that are listed below as defaultprograms, or a custom program.
For defaultprograms, vim-autoformat knows for which filetypes it can be used.
For using a custom formatprogram, read the text below *How can I change the behaviour of formatters, or add one myself?*
If the formatprogram you want to use is installed in one of the following ways, vim automatically detects it:

* It suffices to make the formatprogram globally available, which is the case if you install it via your package manager.
* Alternatively you can point vim-autoformat to folders containing formatters, by putting the absolute paths to these folders in `g:formatterpath` in your .vimrc, like:

```vim
let g:formatterpath = ['/some/path/to/a/folder', '/home/superman/formatters']
```

Remember that when no formatprograms exists for a certain filetype,
vim-autoformat falls back by default to indenting, retabbing and removing trailing whitespace.
This will fix at least the most basic things, according to vim's indentfile for that filetype.

When you have installed the formatter you need, you can format the entire buffer with the command `:Autoformat`.
You can provide the command with a file type such as `:Autoformat json`, otherwise the buffer's filetype will be used.

Some formatters allow you to format only a part of the file, for instance `clang-format` and
`autopep8`.
To use this, provide a range to the `:Autoformat` command, for instance by visually selecting a
part of your file, and then executing `:Autoformat`.
For convenience it is recommended that you assign a key for this, like so:

```vim
noremap <F3> :Autoformat<CR>
```

Or to have your code be formatted upon saving your file, you could use something like this:

```vim
au BufWrite * :Autoformat
```

You can also format the current line only (without having to make a visual selection) by executing `:AutoformatLine`.

To disable the fallback to vim's indent file, retabbing and removing trailing whitespace, set the following variables to 0.

```vim
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0
```

To disable or re-enable these option for specific buffers, use the buffer local variants:
`b:autoformat_autoindent`, `b:autoformat_retab` and `b:autoformat_remove_trailing_spaces`.
So to disable autoindent for filetypes that have incompetent indent files, use

```vim
autocmd FileType vim,tex let b:autoformat_autoindent=0
```

You can manually autoindent, retab or remove trailing whitespace with the following respective
commands.

```vim
gg=G
:retab
:RemoveTrailingSpaces
```

For each filetype, vim-autoformat has a list of applicable formatters.
If you have multiple formatters installed that are supported for some filetype, vim-autoformat
tries all formatters in this list of applicable formatters, until one succeeds.
You can set this list manually in your vimrc (see section *How can I change the behaviour of formatters, or add one myself?*,
or change the formatter with the highest priority by the commands `:NextFormatter` and `:PreviousFormatter`.
To print the currently selected formatter use `:CurrentFormatter`.
These latter commands are mostly useful for debugging purposes.
If you have a composite filetype with dots (like `django.python` or `php.wordpress`),
vim-autoformat first tries to detect and use formatters for the exact original filetype, and
then tries the same for all supertypes occurring from left to right in the original filetype
separated by dots (`.`).

### Using multiple formatters for the same file

It is possible to apply multiple formatters for single file, for example, html can use special formatters for js/css etc.
Support can be enabled via the `g:run_all_formatters_<identifier>` option.

In this case, formatters from `g:formatdef_<identifier>` will be applied to the file one by one. Fallback (vim) formatting
isn't used if at least one formatter has finished sucessfully.

Sample config:
```vim
let g:formatters_vue = ['eslint_local', 'stylelint']
let g:run_all_formatters_vue = 1
```

## Default formatprograms

Here is a list of formatprograms that are supported by default, and thus will be detected and used by vim when they are installed properly.

* `buildifier` for __bazel__ build files. (https://github.com/bazelbuild/buildtools/tree/master/buildifier)

* `clang-format` for __C__, __C++__, __Objective-C__, __Protobuf__ (supports formatting ranges).
  Clang-format is a product of LLVM source builds.
  If you `brew install llvm`, clang-format can be found in /usr/local/Cellar/llvm/bin/.
  Vim-autoformat checks whether there exists a `.clang-format` or a `_clang-format` file up in
  the current directory's ancestry. Based on that it either uses that file or tries to match
  vim options as much as possible.
  Details: http://clang.llvm.org/docs/ClangFormat.html.

* `astyle` for __C#__, __C++__, __C__ and __Java__.
  Download it here: http://astyle.sourceforge.net/.
  *Important: version `2.0.5` or higher is required, since only those versions correctly support piping and are stable enough.*

* `dfmt` for __D__.
  It can be built or downloaded from https://github.com/dlang-community/dfmt.
  Arch Linux users can install it from the `community` repository with `pacman -S dfmt`.
  If `dfmt` is not found in `PATH`, Vim-autoformat will try to use `dub run dfmt` command
  to automatically download and run `dfmt`.

* `autopep8` for __Python__ (supports formatting ranges).
  It's probably in your distro's repository, so you can download it as a regular package.
  For Ubuntu type `sudo apt-get install python-autopep8` in a terminal.
  Here is the link to the repository: https://github.com/hhatto/autopep8.
  And here the link to its page on the python website: http://pypi.python.org/pypi/autopep8/0.5.2.

* `yapf` for __Python__ (supports formatting ranges).
  Vim-autoformat checks whether there exists a `.style.yapf` or a `setup.cfg` file up in the current directory's ancestry.
  Based on that it either uses that file or tries to match vim options as much as possible.
  Most users can install with the terminal command `sudo pip install yapf` or `pip install --user yapf`.
  YAPF has one optional configuration variable to control the formatter style.
  For example:

  ```vim
  let g:formatter_yapf_style = 'pep8'
   ```

  `pep8` is the default value, or you can choose: `google`, `facebook`, `chromium`.

  Here is the link to the repository: https://github.com/google/yapf

* `black` for __Python__.
  Most users can install with the terminal command `sudo pip install black` or `pip install --user black`.
  Here is the link to the repository: https://github.com/ambv/black

* `js-beautify` for __Javascript__ and __JSON__.
  It can be installed by running `npm install -g js-beautify`.
  Note that `nodejs` is needed for this to work.
  The python version version is also supported by default, which does not need `nodejs` to run.
  Here is the link to the repository: https://github.com/einars/js-beautify.

* `JSCS` for __Javascript__. https://jscs-dev.github.io/

* `gnatpp` for __Ada__. http://gcc.gnu.org/onlinedocs/gcc-3.4.6/gnat_ugn_unw/The-GNAT-Pretty_002dPrinter-gnatpp.html

* `standard` for __Javascript__.
  It can be installed by running `npm install -g standard` (`nodejs` is required). No more configuration needed.
  More information about the style guide can be found here: http://standardjs.com/.

* `ESlint` for __Javascript__. http://eslint.org/
  It can be installed by running `npm install eslint` for a local project or by running `npm install -g eslint` for global use. The linter is then installed locally at `$YOUR_PROJECT/node_modules/.bin/eslint` or globally at `~/.npm-global/bin/eslint`.
  When running the formatter, vim will walk up from the current file to search for such local installation and a ESLint configuration file (either `.eslintrc.js` or `eslintrc.json`). When the local version is missing it will fallback to the global version in your home directory. When both requirements are found eslint is executed with the `--fix` argument.
  Note that the formatter's name is still `eslint_local` for legacy reasons even though it already supports global `eslint`.
  Currently only working on \*nix like OS (Linux, MacOS etc.) as it requires the OS to provide sh like shell syntax.

* `xo` for __Javascript__.
  It can be installed by running `npm install -g xo` (`nodejs` is required).
  Here is the link to the repository: https://github.com/sindresorhus/xo.

* `JuliaFormatter.jl` for __Julia__.
  It can be installed by running `julia -e 'import Pkg; Pkg.add("JuliaFormatter")'`. You will need to install Julia and have the `julia` binary in your `PATH`.
  See https://github.com/domluna/JuliaFormatter.jl for more information on how to configure `JuliaFormatter.jl`.
  Note that since `vim-autoformat` works by running a subprocess, a new instance of Julia is instantiated every time it is invoked.
  And since Julia needs to precompile the code to run `format_text`, this may block the vim instance while the subprocess is running.
  Once Julia finishes executing, control will be handled back to the user and the formatted text will replaces the current buffer.
  You can consider precompiling `JuliaFormatter.jl` to make this process faster (See [`PackageCompiler.jl`](https://github.com/JuliaLang/PackageCompiler.jl) for more information on that),
  or consider using [a dedicated `JuliaFormatter.vim` plugin](https://github.com/kdheepak/JuliaFormatter.vim) that works asynchronously.

* `html-beautify` for __HTML__.
  It is shipped with `js-beautify`, which can be installed by running `npm install -g js-beautify`.
  Note that `nodejs` is needed for this to work.
  Here is the link to the repository: https://github.com/einars/js-beautify.

* `css-beautify` for __CSS__.
  It is shipped with `js-beautify`, which can be installed by running `npm install -g js-beautify`.
  Note that `nodejs` is needed for this to work.
  Here is the link to the repository: https://github.com/einars/js-beautify.

* `stylelint` for __CSS__. https://stylelint.io/
  It can be installed by running `npm install stylelint stylelint-config-standard` for a local project or by running `npm install -g stylelint stylelint-config-standard` for global use. The linter is then installed locally at `$YOUR_PROJECT/node_modules/.bin/stylelint` or globally at `~/.npm-global/bin/stylelint`.
  When running the formatter, vim will walk up from the current file to search for such local installation. When the local version is missing it will fallback to the global version in your home directory. When both requirements are found styelint is executed with the `--fix` argument.
  Currently only working on \*nix like OS (Linux, MacOS etc.) as it requires the OS to provide sh like shell syntax.

* `typescript-formatter` for __Typescript__.
  `typescript-formatter` is a thin wrapper around the TypeScript compiler services.
  It can be installed by running `npm install -g typescript-formatter`.
  Note that `nodejs` is needed for this to work.
  Here is the link to the repository: https://github.com/vvakame/typescript-formatter.

* `haxe-formatter` for __Haxe__.
  `haxe-formatter` is a thin wrapper around the haxelib formatter library.
  It can be installed by running `haxelib install formatter`.
  Here is the link to the repository: https://github.com/HaxeCheckstyle/haxe-formatter

* `sass-convert` for __SCSS__.
  It is shipped with `sass`, a CSS preprocessor written in Ruby, which can be installed by running `gem install sass`.
  Here is the link to the SASS homepage: http://sass-lang.com/.

* `tidy` for __HTML__, __XHTML__ and __XML__.
  It's probably in your distro's repository, so you can download it as a regular package.
  For Ubuntu type `sudo apt-get install tidy` in a terminal.

* `rbeautify` for __Ruby__.
  It is shipped with `ruby-beautify`, which can be installed by running `gem install ruby-beautify`.
  Note that compatible `ruby-beautify-0.94.0` or higher version.
  Here is the link to the repository: https://github.com/erniebrodeur/ruby-beautify.
  This beautifier developed and tested with ruby `2.0+`, so you can have weird results with earlier ruby versions.

* `rubocop` for __Ruby__.
  It can be installed by running `gem install rubocop`.
  Here is the link to the repository: https://github.com/bbatsov/rubocop

* `gofmt` for __Golang__.
  The default golang formatting program is shipped with the golang distribution. Make sure `gofmt` is in your PATH (if golang is installed properly, it should be).
  Here is the link to the installation: https://golang.org/doc/install
  An alternative formatter is [gofumpt](https://github.com/mvdan/gofumpt), which enforces a stricter format than `gofmt`. To enable `gofumpt` support, you should install it by running `go install mvdan.cc/gofumpt@latest`, and then change the default golang formatter by configuring `let g:formatters_go = ['gofumpt']`.

* `rustfmt` for __Rust__.
  It can be installed using `cargo`, the Rust package manager. Up-to-date installation instructions are on the project page: https://github.com/rust-lang/rustfmt#quick-start.

* `dartfmt` for __Dart__.
  Part of the Dart SDK (make sure it is on your PATH). See https://www.dartlang.org/tools/dartfmt/ for more info.

* `perltidy` for __Perl__.
  It can be installed from CPAN `cpanm Perl::Tidy` . See https://metacpan.org/pod/Perl::Tidy and http://perltidy.sourceforge.net/ for more info.

* `stylish-haskell` for __Haskell__
  It can be installed using [`cabal`](https://www.haskell.org/cabal/) build tool. Installation instructions are available at https://github.com/jaspervdj/stylish-haskell#installation

* `purty` for __Purescript__
  It can be installed using `npm install purty`. Further instructions available at https://gitlab.com/joneshf/purty

* `remark` for __Markdown__.
  A Javascript based markdown processor that can be installed with `npm install -g remark-cli`. More info is available at https://github.com/wooorm/remark.

* `fprettify` for modern __Fortran__.
  Download from [official repository](https://github.com/pseewald/fprettify). Install with `./setup.py install` or `./setup.py install --user`.

* `mix format` for __Elixir__.
  `mix format` is included with Elixir 1.6+.

* `fixjson` for JSON.
  It is a JSON file fixer/formatter for humans using (relaxed) JSON5. It fixes various failures while humans writing JSON and formats JSON codes.
  It can be installed with `npm install -g fixjson`. More info is available at https://github.com/rhysd/fixjson.

* `shfmt` for __Shell__.
  A shell formatter written in Go supporting POSIX Shell, Bash and mksh that can be installed with `go get -u mvdan.cc/sh/cmd/shfmt`. See https://github.com/mvdan/sh for more info.

* `fish_indent` for __Fish-shell__.
  Fish shell builtin fish_indent formatter for fish shell script.

* `luafmt` for __Lua__.
  Install `luafmt` with `npm`. See https://github.com/trixnz/lua-fmt for more info.
  
* `stylua` for __Lua__.
  Install `stylua` with `cargo`. See https://github.com/JohnnyMorganz/StyLua for more info.

* `sqlformat` for __SQL__.
  Install `sqlparse` with `pip`.

* `cmake-format` for __CMake__.
  Install `cmake_format` with `pip`. See https://github.com/cheshirekow/cmake_format for more info.

* `latexindent.pl` for __LaTeX__.
  Installation instructions at https://github.com/cmhughes/latexindent.pl. On mac you can install it with `brew install latexindent`, then you have to add `let g:formatdef_latexindent = '"latexindent -"'` to `.vimrc`.

* `ocamlformat` for __OCaml__.
  OCamlFormat can be installed with opam: `opam install ocamlformat`.
  Details: https://github.com/ocaml-ppx/ocamlformat.
  We also provide `ocp-indent` as reserve formatter.

* `asmfmt` for __Assembly__.
  An assembly formatter. Can be installed with `go get -u github.com/klauspost/asmfmt/cmd/asmfmt`. See https://github.com/klauspost/asmfmt for more info.

* `nixfmt` for __Nix__.
  It can be installed from nixpkgs with `nix-env -iA nixpkgs.nixfmt`. See https://github.com/serokell/nixfmt for more.

* `dhall format` for __Dhall__.
  The standard formatter, bundled with the interpreter. See https://github.com/dhall-lang/dhall-lang for more info.

* `terraform fmt` for __Terraform__.
  The standard formatter included with Terraform. See https://www.terraform.io/docs/cli/commands/fmt.html for more info.

* `packer fmt` for __Packer__.
  The standard formatter included with Packer. See https://www.packer.io/docs/commands/fmt for more info.

* `nginxfmt.py` for __NGINX__.
  See https://github.com/slomkowski/nginx-config-formatter for more info.

* `zigformat` for __Zig__.
  It is an unofficial binary. You can find the installation instructions from the repo: [zigformat](https://github.com/Himujjal/zigformat)

## Help, the formatter doesn't work as expected!

If you're struggling with getting a formatter to work, it may help to set vim-autoformat in
verbose-mode. Vim-autoformat will then output errors on formatters that failed.
The value of g:autoformat_verbosemode could set as 0, 1 or 2. which means:
0: no message output. 1: only error message output. 2: all message output.

```vim
let g:autoformat_verbosemode=1
" OR:
let verbose=1
```

To read all messages in a vim session type `:messages`.  Since one cannot always easily copy
the contents of messages (e.g. for posting it in an issue), vim-autoformats command `:PutMessages` may
help. It puts the messages in the current buffer, allowing you to do whatever you want.

#### Reporting bugs

Please report bugs by creating an issue in this repository.
When there are problems with getting a certain formatter to work, provide the output of verbose
mode in the issue.

## How can I change the behaviour of formatters, or add one myself?

If you need a formatter that is not among the defaults, or if you are not satisfied with the default formatting behaviour that is provided by vim-autoformat, you can define it yourself.
*The formatprogram must read the unformatted code from the standard input, and write the formatted code to the standard output.*

#### Basic definitions

The formatprograms that available for a certain `<filetype>` are defined in `g:formatters_<filetype>`.
This is a list containing string identifiers, which point to corresponding formatter definitions.
The formatter definitions themselves are defined in `g:formatdef_<identifier>` as a string
expression.
Defining any of these variable manually in your .vimrc, will override the default value, if existing.
For example, a complete definition in your .vimrc for C# files could look like this:

```vim
let g:formatdef_my_custom_cs = '"astyle --mode=cs --style=ansi -pcHs4"'
let g:formatters_cs = ['my_custom_cs']
```

In this example, `my_custom_cs` is the identifier for our formatter definition.
The first line defines how to call the external formatter, while the second line tells
vim-autoformat that this is the only formatter that we want to use for C# files.
*Please note the double quotes in `g:formatdef_my_custom_cs`*.
This allows you to define the arguments dynamically:

```vim
let g:formatdef_my_custom_cs = '"astyle --mode=cs --style=ansi -pcHs".&shiftwidth'
let g:formatters_cs = ['my_custom_cs']
```

Please notice that `g:formatdef_my_custom_cs` contains an expression that can be evaluated, as required.
As you see, this allows us to dynamically define some parameters.
In this example, the indent width that astyle will use, depends on the buffer local value of `&shiftwidth`, instead of being fixed at 4.
So if you're editing a csharp file and change the `shiftwidth` (even at runtime), the `g:formatdef_my_custom_cs` will change correspondingly.

For the default formatprogram definitions, the options `expandtab`, `shiftwidth` and `textwidth` are taken into account whenever possible.
This means that the formatting style will match your current vim settings as much as possible.
You can have look look at the exact default definitions for more examples.
They are defined in `vim-autoformat/plugin/defaults.vim`.
As a small side note, in the actual defaults the function `shiftwidth()` is used instead of the
property. This is because it falls back to the value of `tabstop` if `shiftwidth` is 0.

If you have a composite filetype with dots (like `django.python` or `php.wordpress`),
vim-autoformat internally replaces the dots with underscores so you can specify formatters
through `g:formatters_django_python` and so on.

To override these options for a local buffer, use the buffer local variants:
`b:formatters_<filetype>` and `b:formatdef_<identifier>`. This can be useful, for example, when
working with different projects with conflicting formatting rules, with each project having settings
in its own vimrc or exrc file:

```vim
let b:formatdef_custom_c='"astyle --mode=c --suffix=none --options=/home/user/special_project/astylerc"'
let b:formatters_c = ['custom_c']
```

#### Ranged definitions

If your format program supports formatting specific ranges, you can provide a format
definition which allows to make use of this.
The first and last line of the current range can be retrieved by the variables `a:firstline` and
`a:lastline`. They default to the first and last line of your file, if no range was explicitly
specified.
So, a ranged definition could look like this.

```vim
let g:formatdef_autopep8 = "'autopep8 - --range '.a:firstline.' '.a:lastline"
let g:formatters_python = ['autopep8']
```

This would allow the user to select a part of the file and execute `:Autoformat`, which
would then only format the selected part.

## Contributing

This project is community driven. I don't actively do development on vim-autoformat myself,
as it's current state fulfills my personal needs.
However, I will review pull requests and keep an eye on the general sanity of the code.

If you have any improvements on this plugin or on this readme, if you have some
formatter definition that can be added to the defaults, or if you experience problems, please
open a pull request or an issue in this repository.

## Major Change Log

### October 2018
* We also take the returncode of the formatter process into account, not just the presence of output on stderr.

### March 2016
* We don't use the option formatprg internally anymore, to always have the possible of using the default `gq` command.
* More fallback options have been added.

### June 2015

* *Backward incompatible patch!*
* Multiple formatters per filetype are now supported.
* Configuration variable names changed.
* Using `gq` as alias for `:Autoformat` is no longer supported.
* `:Autoformat` now supports ranges.
* Composite filetypes are fully supported.

### December 20 2013

* `html-beautify` is now the new default for HTML since it seems to be better maintained, and seems to handle inline javascript neatly.
* The `formatters/` folder is no longer supported anymore, because it is unnecessary.
* `js-beautify` can no longer be installed as a bundle, since it only makes this plugin unnecessarily complex.

### March 27 2013

* The default behaviour of gq is enabled again by removing the fallback on auto-indenting.
  Instead, the fallback is only used when running the command `:Autoformat`.

### March 16 2013

* The options `expandtab`, `shiftwidth`, `tabstop` and `softtabstop` are not overwritten anymore.
* This obsoletes `g:autoformat_no_default_shiftwidth`
* `g:formatprg_args_expr_<filetype>` is introduced.

### March 13 2013

* It is now possible to prevent vim-autoformat from overwriting your settings for  `tabstop`, `softtabstop`, `shiftwidth` and `expandtab` in your .vimrc.

### March 10 2013

* When no formatter is installed or defined, vim will now auto-indent the file instead. This uses the indentfile for that specific filetype.

### March 9 2013

* Customization of formatprograms can be done easily now, as explained in the readme.
* I set the default tabwidth to 4 for all formatprograms as well as for vim itself.
* phpCB has been removed from the defaults, due to code-breaking behaviour.
