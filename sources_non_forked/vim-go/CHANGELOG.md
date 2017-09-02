## unplanned

IMPROVEMENTS:

* `:GoAddTags` and `:GoRemoveTags` now continue to process if there are malformed individual struct tags [gh-1401]
* `:GoAddTags` and `:GoRemoveTags` now shows a quickfix window if there are malformed struct tags [gh-1401]


BUG FIXES:

* Include comments in import block when folding is enabled [gh-1387]
* Fix opening definitions in tabs [gh-1400]
* Fix accidently closing quickfix window from other commands if :GoFmt or autosave format was called [gh-1407]

## 1.14 - (August 6, 2017)

FEATURES:

* We now have folding support based on Go syntax. Check out the
  [demo](https://twitter.com/fatih/status/893843722093330433) to see it in
  action. To enable it you have to set the following vim setting: `set
  foldmethod=syntax`. Currently it folds at block (`{ }`), var and const level.
  These can be individually disabled/enabled if wished. For more info please
  read the documentation for the `g:go_fold_enable` setting. [gh-1339] 
* `:GoFiles` accepts now an argument to change the type of files it can show.
  By default it shows`.go source files` but now it can be changed to show
  various kind of files. The full list can be seen via `go list --help` under
  the `// Source Files` section [gh-1372] i.e:

```
:GoFiles CgoFiles        // shows .go sources files that import "C"
:GoFiles TestGoFiles     // shows _test.go files in package
:GoFiles IgnoredGoFiles  // shows .go sources ignored due to build constraints
etc..
```

IMPROVEMENTS

* Files created with `_test.go` extension have a new template with a ready to
  go test function. The template can be changed with the
  `g:go_template_test_file` setting. [gh-1318]
* Improve performance for highly used operations by caching `go env` calls [gh-1320]
* `:GoCoverage` can accept arguments now. i.e: `:GoCoverage -run TestFoo` [gh-1326]
* `:GoDecls` and `:GoDeclsDir` shows a warning if [ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim) is not installed
* `:GoBuild` now compiles the package with the `-i` flag added. This means that subsequent calls are much more faster due caching of packages [gh-1330]
* `:GoCoverage` echos now the progress if `g:go_echo_command_info` is enabled [gh-1333]
* Add `g:go_doc_max_height` setting to control the maximum height of the window created by `:GoDoc` and `K` mapping [gh-1335]
* The `af` text object is able to include the assignment variable for anonymous functions. Can be disabled with `g:go_textobj_include_variable = 0` [gh-1345]
* Add `g:go_list_autoclose` setting to prevent closting the quickfix/location list after zero items [gh-1361]
* Cursor is now adjusted and locked to the correct line when `goimports` is used for autosave [gh-1367]
* Complement the path of command for different situations of Cygwin environment [gh-1394]
* Show message when using :GoDef and opening a new buffer [gh-1385]


BUG FIXES:

* Fix obtaining package's import path for the current directory. This fixes some issues we had if the user was using multiple GOPATH's [gh-1321]
* Fix documentation for vim-go & syntastic integration for errcheck using [gh-1323]
* Fix showing an output if a test has finished when `:GoTest` is called [gh-1327]
* Fix warning when goimports doesn't support srcdir [gh-1344]
* Fix broken code folding with go_highlight_types [gh-1338]
* Fix blocking the ui when swapfile is enabled and `:GoFmt` is called (either manually or via autosave) [gh-1362]
* Fix getting bin paths for binaries if GOPATH was not set and Go version =>1.7 was used [gh-1363]
* Fix picking up the correct list type for showing `:GoFmt` errors [gh-1365]
* Fix auto detecting of GOPATH for import paths with string 'src' (i.e: `GOPATH/src/github.com/foo/src/bar`) [gh-1366]
* Fix showing an empty window if `gogetdoc` was not found [gh-1379]
* Fix commands not being executed if paths would include spaces (binary name, GOPATH, file itself, etc..)  [gh-1374]
* Fix showing correct message when editing a new file [gh-1371]
* Fix filepaths in the quickfix list for :GoVet [gh-1381]
* Run :GoLint against the package of the open file [gh-1382]

BACKWARDS INCOMPATIBILITIES:

* `:GoFmt` now uses `quickfix` to show formatting errors instead of
  `locationlist`. To change back to `locationlist` you can change it with the
  setting `let g:go_list_type = "locationlist"` [gh-1365]
* `:GoLint` now runs against the package of the open file instead of the
  current working directory. This is so all commands behave the same relative
  to the current open buffer. For more info check the [comment
  here](https://github.com/fatih/vim-go/issues/1375#issuecomment-317535953)
  [gh-1382]



## 1.13 - (June 6, 2017)

FEATURES:

* New `:GoKeyify` command that turns unkeyed struct literals into keyed struct literals. [gh-1258]. i.e:

```
Example{"foo", "bar", "qux"}
```

will be converted to:

```
Example{
  foo: "foo",
  bar: "bar",
  qux: "qux",
}
```

Checkout the demo here: https://twitter.com/fatih/status/860410299714764802


* New `g:go_addtags_transform` setting to change the transform rule (snakecase, camelcase, etc..) for `:GoAddTags` command [gh-1275]
* New snippet shortcut assigned to `ife` that expands to `if err := foo(); err != nil { ... }` [gh-1268]

IMPROVEMENTS

* :GoMetaLinter can now exclude linters with the new `g:go_metalinter_excludes` option [gh-1253]
* Override `<C-LeftMouse>` mapping so `:GoDef` is used by default (as we do the same for `CTRL-]`, `gd`, etc. [gh-1264]
* add support for `go_list_type` setting in `:GoFmt` and `:GoImports` commands [gh-1304]
* add support for `go_list_type` setting in `:GoMetaLinter` commands [gh-1309]
* `go_fmt_options` can be now a dictionary to allow us to specifcy the
  options for multiple binaries [gh-1308]. i.e:

```
  let g:go_fmt_options = {
    \ 'gofmt': '-s',
    \ 'goimports': '-local mycompany.com',
    \ }
```
* If win-vim(x64) with Cygwin is used, `cygpath` is used for constructing the paths [gh-1092]

BUG FIXES:

* job: fix race between channel close and job exit [gh-1247]
* internal: fix system calls when using tcsh [gh-1276]
* path: return the unmodified GOPATH if autodetect is disabled [gh-1280]
* fix jumping to quickfix window when autom gometalinter on save was enabled [gh-1293]
* fix highlighting for `interface` and `structs` words when `go_highlight_types` is enabled [gh-1301]
* fix cwd for running `:GoRun` when used with neovim [gh-1296]
* `:GoFmt` handles files that are symlinked into GOPATH better (note that this behaviour is discouraged, but we're trying our best to handle all edge case :)) [gh-1310]
* `:GoTest` is able to parse error messages that include a colon `:` [gh-1316]
* `:GoTestCompile` under the hood doesn't produces a test binary anymore. Sometimes a race condition would happen which would not delete the test binary. [gh-1317]
* `:GoDef` jumps now to definition for build tags defined with `:GoBuildTags` (only guru) [gh-1319]
 
BACKWARDS INCOMPATIBILITIES:

* `:GoLint` works on the whole directory instead of the current file. To use it for the current file give it as an argument, i.e `:GoLint foo.go` [gh-1295]
* `go_snippet_case_type` is removed in favor of the new `go_addtags_transform` setting [gh-1299]
* `go_imports_bin` is removed to avoid confusion as it would lead to race
  conditions when set to `gofmt` along with the usage of `go_fmt_command`
  [gh-1212] [gh-1308]
* commands such as `:GoTest` has been refactored for easy maintainability. If
  you use any custom script that was using the function `go#cmd#Test`, it
  should be renamed to `go#test#Test`

## 1.12 - (March 29, 2017)

FEATURES:

* New `:GoAddTags` and `:GoRemoveTags` command based on the tool
  [gomodifytags](https://github.com/fatih/gomodifytags). This fixes many old
  bugs that were due prior regexp based implementation. For the usage please
  read the docs and checkout the demo at:
  https://github.com/fatih/vim-go/pull/1204 [gh-1204]
* Add new `errl` snippet that expands to [gh-1185]:

```
if err != nil {
	log.Fatal(err)
}
```
* New `:GoBuildTags` command to change build tags for tools such as `guru`,
  `gorename`, etc ... There is also a new setting called `g:go_build_tags`
  [gh-1232]

IMPROVEMENTS:

* vim-go works now even if GOPATH is not set (starting with Go 1.8) [gh-1248]
* Lowercase `<Leader>` in mappings examples for consistent documentation across the README [gh-1192]
* All of files should be written in utf-8 if the file will be passed to external command. [gh-1184]
* `:GoAddTags` is now able to add options to existing tags with the syntax
  `:GoAddTags key,option`, i.e: `:GoAddTags json,omitempty` [gh-985]
* Document 'noshowmode' requirement for echo_go_info [gh-1197]
* Improve godoc view for vertical splits [gh-1195]
* Set GOPATH for both possible go guru execution paths (sync and async) [gh-1193]
* Improve docs for :GoDef usage [gh-1242]
* Highlight trimming syntax for Go templates [gh-1235]

BUG FIXES:

* Honor `g:go_echo_command_info` when dispatching builds in neovim [gh-1176]
* Fix `:GoBuild` error in neovim due to invalid jobcontrol handler function
  signatures (`s:on_stdout`, `s:on_stderr`)[gh-1176]
* Update statusline before and after `go#jobcontrol#Spawn` command is executed [gh-1176]
* Correctly report the value of the 'g:go_guru_tags' variable [gh-1177]
* Ensure no trailing `:` exist in GOPATH detection if initial GOPATH is not set [gh-1194]
* Fix `:GoAddTags` to allow modifying existing comments [gh-984]
* Fix `:GoAddTags` to work with nested structs [gh-990]
* Fix `:GoAddTags` adding tags twice for existing tags [gh-1064]
* Fix `:GoAddTags` not working for fields of types `interface{}` [gh-1091]
* Fix `:GoAddTags` not working for fields with one line comments [gh-1181]
* Fix `:GoAddTags` not working if any field comment would contain `{}` [gh-1189]
* Respect go_fmt_options when running goimports [gh-1211]
* Set the filename in the location-list when there is an error with :GoFmt [gh-1199]
* Fix `:GoInstall` to accept additional arguments if async mode was enabled [gh-1246]

BACKWARDS INCOMPATIBILITIES:

* The command `:GoGuruTags` is removed in favour of the new command
  `:GoBuildTags`. This command will be used now not just for `guru`, also for
  all new commands such as `gorename` [gh-1232]
* The setting `g:go_guru_tags` is removed in favour of the new setting
  `g:go_build_tags` [gh-1232]


## 1.11 - (January 9, 2017)

FEATURES:

* Travis test integration has been added. Now any file that is added as
  `<name>_test.vim` will be automatically tested in for every Pull Request
  (just like how we add tests to Go with `_test.go`). Going forward this will
  tremendously increase the stability and decrease the maintenance burden of
  vim-go. [gh-1157]
* Add new `g:go_updatetime` setting to change the default updatetime (which was hardcoded previously) [gh-1055]
* Add new `g:go_template_use_pkg` setting to enable to use cwd as package name instead of basic template file [gh-1124]

IMPROVEMENTS:

* Add `statusline` support for `:GoMetaLinter` [gh-1120]
* Quickfix and Location lists contain now a descriptive title (requires at least Vim `7.4.2200`)[gh-1004]
* Check `go env GOPATH` as well for `:GoInstallBinaries` as Go has now a default path for GOPATH ("~/go")starting with 1.8 [gh-1152]
* `:GoDocBrowser` now also works on import paths [gh-1174]

BUG FIXES:

* Always use full path to detect packages to be shown in statusline [gh-1121]
* Use `echom` to persist errors in case of multiple echos [gh-1122]
* Fix a race condition where a quickfix window was not closed if a job has succeeded [gh-1123]
* Do not expand coverage arguments for non job execution of `:GoCoverage` [gh-1127]
* `:GoCoverage` doesn't mess up custom syntax anymore [gh-1128]
* Disable autoformat for `asm` files as they might be non Go ASM format [gh-1141]
* Fix indentation broken when using a action with a minus sign like `{{-` [gh-1143]
* Fix breaking Neovim change of passing less arguments to callbacks [gh-1145]
* Fix `guru` commands if custom build tags were set [gh-1136]
* Fix referencing a non defined variable for async commands when bang (!) was used
* Fix `:GoDef` failing for a modified buffer if `hidden` was not set [gh-1132]
* Fix `:GoDefStack` to allow popping from jump list when buffer is modified [gh-1133]
* Improve internal defining of functions and referencing them for async operations [gh-1155]
* Fix `:GoMetaLinter` failing if `go_metalinter_command` is set. [gh-1160]
* Fix `:GoMetaLinter`'s `go_metalinter_deadline` setting for async mode [gh-1146]

BACKWARDS INCOMPATIBILITIES:

* The following syntax options are now disabled by default. If you're using them be sure to set them in your .vimrc [gh-1167]

```viml
g:go_highlight_array_whitespace_error
g:go_highlight_chan_whitespace_error
g:go_highlight_extra_types
g:go_highlight_space_tab_error
g:go_highlight_trailing_whitespace_error
```



## 1.10 (November 24, 2016)

FEATURES:

* **Vim 8.0 support!** This is the initial version to add Vim 8.0 based support to
  all basic commands (check out below for more information). With time we'll
  going to extend it to other commands. All the features are only enabled if
  you have at least Vim 8.0.0087. Backwards compatible with Vim 7.4.x.
  If you see any problems, please open an issue.

* We have now a [logo for vim-go](https://github.com/fatih/vim-go/blob/master/assets/vim-go.png)! Thanks to @egonelbre for his work on this.
* `:GoBuild`, `:GoTest`, `:GoTestCompile`, `:GoInstall` commands are now fully
  async. Async means it doesn't block your UI anymore. If the command finished
  it echoes the status. For a better experience use the statusline information
  (more info below)

* `:GoCoverage` and `:GoCoverageBrowser` commands are fully async.
* `:GoDef` is fully async if `guru` is used as command.
* `:GoRename` is fully async .

* `:GoMetaLinter` is fully asnyc. Also works with the current autosave linting
  feature. As a reminder, to enable auto linting on save either call
  `:GoMetaLinterAutoSaveToggle` (temporary) or add `let
  g:go_metalinter_autosave = 1` (persistent) to your virmc).

* All `guru` commands run asynchronously if Vim 8.0 is being used. Current
  Commands:
	* GoImplements
	* GoWhicherrs
	* GoCallees
	* GoDescribe
	* GoCallers
	* GoCallstack
	* GoFreevars
	* GoChannelPeers
	* GoReferrers

* `:GoSameIds` also runs asynchronously. This makes it useful especially for
  auto sameids mode. In this mode it constantly evaluates the identifier under the
  cursor whenever it's in hold position and then calls :GoSameIds. As a
  reminder, to enable auto info either call `:GoSameIdsAutoToggle`(temporary)
  or add `let g:go_auto_sameids = 1` (persistent) to your vimrc.

* `:GoInfo` is now non blocking and works in async mode if `guru` is used in
  `g:go_info_mode`. This makes it useful especially for autoinfo mode. In this
  mode it constantly evaluates the identifier under the cursor whenever it's in
  hold position and then calls :GoInfo. As a reminder, to enable auto info
  either call `:GoAutoTypeInfoToggle`(temporary) or add `let
  g:go_auto_type_info = 1` (persistent) to your vimrc. To use `guru` instead of
  `gocode` add following to your vimrc: `let g:go_info_mode = 'guru'`

  The `guru` is more accurate and reliabed due the usage of `guru` describe. It
  doesn't rely on `pkg/` folder like `gocode` does. However it's slower than
  `gocode` as there is no caching mechanism in `guru` yet.

* **New**: Statusline function: `go#statusline#Show()` which can be plugged into
  the statusline bar. Works only with vim 8.0. It shows all asynchronously
  called functions status real time.  Checkout it in action:
  https://twitter.com/fatih/status/800473735467847680. To enable it add the
  following to your `vimrc`. If you use lightline, airline, .. check out their
  respective documentation on how to add a custom function:

```viml
" go command status (requires vim-go)
set statusline+=%#goStatuslineColor#
set statusline+=%{go#statusline#Show()}
set statusline+=%*
```

IMPROVEMENTS:

* **:GoDocBrowser** is now capable to to understand the identifier under the cursor (just like :GoDoc)
* Function calls are now highlighted as well when `g:go_highlight_functions` is enabled [gh-1048]
* Add completion support for un-imported packages. This allows to complete even
  if the package is not imported. By default it's disabled, enable by adding
  `let g:go_gocode_unimported_packages = 1` [gh-1084]
* Tools that embeds GOROOT into their binaries do not work when people update
  their Go version and the GOROOT contains the vesion as part of their path
  (i.e: `/usr/local/Cellar/go/1.7.2/libexec`, [more
  info](https://blog.filippo.io/stale-goroot-and-gorebuild/)) . This is now
  fixed by introducing automatic GOROOT set/unset before each tool invoke.
  [gh-954]
* Added new setting `g:go_echo_go_info` to enable/disable printing identifier
  information when completion is done [gh-1101]
* Added new `go_echo_command_info` setting is added, which is enabled by
  default.  It's just a switch for disabling messages of commands, such as
  `:GoBuild`, `:GoTest`, etc.. Useful to *disable* if `go#statusline#Show()` is
  being used in Statusline, to prevent to see duplicates notifications.
* goSameId highlighting is now linked to `Search`, which is much more clear as
  it changes according to the users colorscheme
* Add plug mapping `(go-lint)` for :GoLint [gh-1089]


BUG FIXES:

* Change back nil and iota highlighting color to the old type [gh-1049]
* Fix passing arguments to `:GoBuild` while using NeoVim [gh-1062]
* Do not open a split if `:GoDef` is used on a modified file [gh-1083]
* Highlight nested structs correctly [gh-1075]
* Highlight builtin functions correctly if `g:go_highlight_functions` is enabled [gh-1070]
* Fix `:GoSameIds` highlighting if a new buffer is opened in the same window [gh-1067]
* Internal: add `abort` to all vim function to return in case of errors [gh-1100]
* Fix `:GoCoverage` to be executed if working dir is not inside the test dir [gh-1033]

BACKWARDS INCOMPATIBILITIES:

* remove vim-dispatch and vimproc.vim support. vim 8.0 has now the necessary
  API to invoke async jobs and timers. Going forward we should use those. Also
  this will remove the burden to maintain compatibility with those plugins.

* `go#jobcontrol#Statusline()` is removed in favor of the new, global and
  extensible `go#statusline#Show()`

## 1.9 (September 13, 2016)

IMPROVEMENTS:

* **guru** uses now the `-modified` flag, which allows us use guru on modified
  buffers as well. This affects all commands where `guru` is used. Such as
  `:GoDef`, `:GoReferrers`, etc.. [gh-944]
* **:GoDoc** uses now the `-modified` flag under the hood (for `gogetdoc), which allows us to get documentation for the identifier under the cursor ina modified buffer. [gh-1014]
* Cleanup and improve documentation [gh-987]
* Add new `g:go_gocode_socket_type` setting to change the underlying socket type passed to `gocode`. Useful to fallback to `tcp` on cases such as Bash on Windows [gh-1000]
* `:GoSameIds` is now automatically re-evaluated in cases of buffer reloads (such as `:GoRename`) [gh-998]
* Improve docs about `go_auto_sameids` [gh-1017]
* Improve error message by printing the full path if an incompatible `goimports` is being used [gh-1006]
* `iota` and `nil` are now highlighted correctly and are not treated as booleans [gh-1030]

BUG FIXES:

* Fix system calls on Windows [gh-988]
* Fix :GoSameIds and :GoCoverage for light background and after changing color schemes [gh-983]
* Fix TagBar and `GoCallers` for Windows user [gh-999]
* Set updatetime for for `auto_sameids` feature as well [gh-1016]
* Update docs about missing `go_highlight_generate_tags` setting [gh-1023]
* Fix updating the jumplist if `:GoDef` is used [gh-1029]
* Fix highlighting literal percent sign (`%%`) in strings [gh-1011]
* Fix highlighting of nested fields [gh-1007]
* Fix checking for `exepath` feature for the upcoming vim 8.0 release [gh-1046]

BACKWARDS INCOMPATIBILITIES:

* Rename `GoMetalinterAutoSaveToggle` to `GoMetaLinterAutoSaveToggle` to make it compatible with the existing `:GoMetaLinter` command [gh-1020]

## 1.8 (July 31, 2016)

FEATURES:
* New **`:GoAddTags`** command that adds field tags for the fields of a struct automatically based on the field names. Checkout the demo to see it in action: https://twitter.com/fatih/status/759822857773907968 [gh-971]
* The snippet expansion `json` is now much more smarter. It pre populates the placeholder according to the first word and it also applies `snake_case` or `camelCase` conversion. Together with `:GoAddTags` it gives `vim-go` users flexible ways of populating a field tag. Checkout the demo to see it in action: https://twitter.com/fatih/status/754477622042689536 [gh-927]
* New **`:GoSameIds`** command. When called highlights all same identifiers in the current file. Can be also enabled to highlight identifiers automatically (with `:GoSameIdsAutoToggle` or `g:go_auto_sameids`). Checkout the demo to see it in action: https://twitter.com/fatih/status/753673709278339072. [gh-936]
* New **`:GoWhicherrs`** command. It shows all possible values of the selected error variable. [gh-948]
* Add new `errp` snippet to expand an `if err != nil { panic() }` clause [gh-926]
* If you open a new buffer with a Go filename it get automatically populated based on the directory. If there are no Go files a simple main package is created, otherwise the file will include the package declaration line based on the package in the current directory. Checkout the demo to see it in action: https://twitter.com/fatih/status/748333086643994624. This is enabled by default. Can be disabled with `let g:go_template_autocreate = 0`. You can use your own template with `let g:go_template_file = "foo.go"` and putting the file under the `templates/` folder. [gh-918]
* Added new toggle commands to enable/disable feature that run for your
  automatic. For example if you have `let g:go_auto_type_info = 1` enabled, you
  can now easily enable/disable it on the fly. Support added with the following
  commands: `:GoAutoTypeInfoToggle`, `:GoFmtAutoSaveToggle`,
  `:GoAsmFmtAutoSaveToggle`, `:GoMetalinterAutoSaveToggle`,
  `:GoTemplateAutoCreateToggle` [gh-945]


IMPROVEMENTS:
* `:GoDoc` accepts arguments now which are passed directly to `godoc`. So usages like `:GoDoc flag` works again (it was changed in previous versions [gh-894]
* `:GoDef` works now for modified files as well [gh-910]
* Internal: pass filename to the `--srcdir` flag to enable upcoming `goimports` features [gh-957]
* Internal: fix indentations on all files to **2-spaces/no tabs**. This is now the default vim-go style across all VimL files [gh-915]
* Internal: autocmd settings can be now dynamically enabled/disabled [gh-939]
* Internal: automatically detect `GOPATH`  for :GoInstall [gh-980]
* Internal: shell executions uses now by default `sh` and then resets it back to the user preference. [gh-967]
* Syntax: improved syntax highglighting performance for methods, fields, structs and interface type declarations [gh-917]
* Syntax: moved `:GoCoverage` highlight definition into go's syntax file for more customizability [gh-962]


BUG FIXES:

* Escape `#` characters when opening URL's, as it's handled as alternative file in vim [gh-895]
* Fix typos in `doc/vim-go.txt` about usages of syntax highglightings [gh-897]
* Fix `:GoCoverage` not running for Neovim [gh-899]
* Fix `:GoFmt` not picking up `-srcdir` if the command was set to use `goimports` [gh-904]
* Fix `:GoTestCompile` to not leave behind artifacts if the cwd and the test files's directory do not match [gh-909]
* Fix `:GoDocBrowser` to not fail if godoc doesn't exist [gh-920]
* Fix `:GoFmt` to not change the permissions of saved file. Now original file permissions are restored [gh-922]

BACKWARDS INCOMPATIBILITIES:

* `g:go_highlight_structs` and `g:go_highlight_interface` are removed in favor of `g:go_highlight_types` [gh-917]


## 1.7.1 (June 7, 2016)

BUG FIXES:
* Fixed typo in `syntax/go.vim` file from `go:go_highlight_fields` to `g:go_highlight_fields`

## 1.7 (June 7, 2016)

FEATURES:

* New **`:GoImpl`** command that generates method stubs for implementing an interface. Checkout the [demo](https://twitter.com/fatih/status/729991365581545472) to see how it works. [gh-846]
* `godef` support is added back as an optional setting.  By default `:GoDef` still uses `guru`, but can be changed to `godef` by adding the option: `let g:go_def_mode = 'godef'` [gh-888]
* New `<C-w><C-]>` and `<C-w>]>` shortcuts to split current window and jumpt to the identifier under cursor. [gh-838]
* New syntax setting" `g:go_highlight_fields` that highlights struct field references [gh-854]

IMPROVEMENTS:

* Invoking `:GoRename` now reloads all files to reflect new changes automatically [gh-855]
* Calling `:GoTestCompile` does not create any temporary binary file anymore [gh-879]
* Enable passing the `-tags` flag to `:GoDef`. Now you can pass build tags to `:GoDef` via `:GoGuruTags` or `g:go_guru_tags`
* Internal refactoring to use custom `system()` function that wraps both the standard `system()` call and `vimproc`. Now all system calls will take advantage and will use `vimproc` if installed. [gh-801]
* Completion enables now `gocode`'s `autobuild` and `propose-builtins` flags automatically. With these settings packages will be automatically build to get the freshest completion candidates and builtin keywords will be showed as well. By defaults these settings are enabled. Settings can be disabled/enabled via `g:go_gocode_autobuild` and `g:go_gocode_propose_builtins`. [gh-815]
* Added new `http.HandlerFunc` snippets with `hf` and `hhf` shortcuts [gh-816]
* Added new `Example` and `Benchmark` snippets with `example` and `benchmark` shortcuts [gh-836]
* Search tool binaries first in `GOBIN` and then in `PATH` as most of vim-go users installs it to `GOBIN` mostly [gh-823]
* Improve `guru` based commands by providing automatically detected GOPATHS, such as `gb`, `godep` to be used if possible [gh-861]
* Add `<Plug>(go-imports)` mapping to make it assignable to other keys [gh-878]
* Increase compatibility with tcsh [gh-869]
* Improve `:GoInstallBinaries` for GOPATH's which don't have packages that work well with `go get -u`. We have a new `g:go_get_update` setting to disable it. By default it's enabled. [gh-883]



BUG FIXES:
* Fix `(go-freevars)` plug mapping to work as in visual mode instead of noncompatible normal mode [gh-832]
* Commands based on guru now shows a more meaningful error message instead of just showing the exit status (-1)
* Fix `:GoCoverage` accidentally enabling syntax highlighting for users who don't use syntax (i.e syntax off) [gh-827]
* Fix `:GoCoverage` colors to work for xterm as well [gh-863]
* Fix commenting out block of texts for Go templates (filetype gothtmltmpl) [gh-813]
* Fix `:GoImplements` failing because of an empty scope definition. Now we default to current package to make it usable.
* Fix `:GoPlay` posting to non HTTPS url. [gh-847]
* Fix escaping the filenames for lint and motion commands [gh-862]
* Fix escaping the filename to `:GoDef` completely for tcsh [gh-868]
* Fix showing SUCCESS for `go test` related commands if no test files are available [gh-859]



## 1.6 (April 25, 2016)

FEATURES:

* New `CHANGELOG.md` file (which you're reading now). This will make it easier
  for me to track changes and release versions
* **`:GoCoverage`** is now highlighting the current source file for
  covered/uncovered lines. If called again it runs the tests and updates the
  annotation. Use `:GoCoverageClear` to clear the coverage annotation.
  This is a pretty good addition to vim-go and I suggest to check out the gif
  that shows it in action: https://twitter.com/fatih/status/716722650383564800
  [gh-786]
* **`:GoCoverageToggle`** just like `:GoCoverage` but acts as a toggle. If run
  again it clears the annotation.
* **`:GoCoverageBrowser`** opens a new annotated HTML page. This is the old
  `:GoCoverage` behavior [gh-786]
* **`:GoDoc`** uses now [gogetdoc](https://github.com/zmb3/gogetdoc) to
  lookup and display the comment documentation for the identifier under the
  cursor. This is more superior as it support looking up dot imports, named
  imports and imports where package name and file name are different [gh-782]
* **`guru support`**: `oracle` is replaced by the new tool `guru`. `oracle.vim`
  is therefore renamed to `guru.vim`. I've also refactored the code to make it
  much more easier to maintain and add additional features in future (such as
  upcoming JSON decoding). vim-go is now fully compatible with `guru`. Please
  be sure you have installed `guru`. You can easily do it with
  `:GoInstallBinaries`.
* **`:GoDef`** uses now `guru definition` under the hood instead of `godef`.
  This fixes the following issues: 1. dot imports 2. vendor imports 3. folder
  != package name imports. The tool `godef` is also deprecated and not used
  anymore.
* **`:GoDef`** does have now history of the call stack. This means you can
  easily jump back to your last entry. This can be done with the new command
  `:GoDefPop` or the mapping `CTRL-t`. To see the stack and jump between entries
  you can use the new command `:GoDefStack`, which shows the list of all stack
  entries. To reset the stack list anytime you can call `:GoDefStackClear`
  [gh-776]

IMPROVEMENTS:

* **`:GoCoverage`** is executed asynchronously when used within Neovim [gh-686]
* **`:GoTestFunc`** supports now testable examples [gh-794]
* **`:GoDef`** can jump to existing buffers instead of opening a new window
  (split, vsplit or tab). By default it's disabled to not break the old
  behavior, can be enabled with `let g:go_def_reuse_buffer = 1`

BUG FIXES:

* Fix not showing documentation for dot, named and package/file name being different imports [gh-332]
* Term mode: fix closing location list if result is successful after a failed attempt [gh-768]
* Syntax: fix gotexttmpl identifier highlighting [gh-778]
* Doc: fix wrong wording for `go-run` mapping. It's for the whole main package,
  not for the current file

BACKWARDS INCOMPATIBILITIES:

* `:GoDef` doesn't accept any identifier as an argument. This is not suported
  via `guru definition` and also was not widely used either. Also with this, we
  significantly simplified the existing def.vim code
* `:GoOracleScope`  and `:GoOracleTags` are deprecated in favor of
  `:GoGuruScope` and `:GoGuruTags`. Also `g:go_oracle_scope` is renamed to
  `g:go_guru_scope`
* `g:go_guru_scope` accepts a variable in type of `list` instead of `string`.
  i.g: `let g:go_guru_scope = ["github.com/fatih/structs", "golang.org/x/tools/..."]`


## 1.5 (Mar 16, 2016)

FEATURES:
* Introducing code name "motion". A new whole way of moving
  around and navigating (gh-765). Checkout the following new changes:
  * A vim-go specific tool, called [motion](https://github.com/fatih/motion) is being developed which
    provides us the underlying foundation for the following and upcoming
    new features.
  * `]]` and `[[` motions can be used to jump between functions
  * `if` and `af` are improved and implement from scratch. It has now
    support for literal functions, comments of functions, better cursor
    position support and more stable.
  * New `:GoDecls` and `:GoDeclsDir` commands that are available if
    `ctrlp.vim` is installed. Once called one can easily jump to any generic declaration available.
  * I wrote two blog posts about these new features in more detail. I recommend you to read it: [Treating Go types as objects in Vim](https://medium.com/@farslan/treating-go-types-as-objects-in-vim-ed6b3fad9287#.mbwaisevp) and [Navigation between functions and types in vim-go](https://medium.com/@farslan/navigation-between-functions-and-types-in-vim-go-f9dd7de8ca37#.2sdf8tbbe)
* A new `:GoAlternate` command that toggles to the test
  file of the current file. It also has new appropriate mappings to open the
  alternate file in split or tabs. (gh-704)
* Now commands can choose whether they want to open a
  `quickfix` or a `location list` via the setting `g:go_list_type`. Also all
  the commands have now some sensible settings, some will open a qf window,
  some will open a location list (gh-700)

IMPROVEMENTS:

* Add support for goimport's new `-srcdir`. Goimports now succesfully suports `vendor/` folders with this release. (gh-735)
* Add `g:go_gorename_prefill` setting which disabled pre filling the argument for `:GoRename` (gh-711)
* Improve `:GoRun` to complete to filenames (gh-742)
* Highlight `//go:generate` comment directives (gh-757)
* Indent code in Go HTML templates (gh-709)
* Improve negative numbers of all types, octals, imaginary numbers with exponents (gh-752)
* Improved internal usage of retrieving offsets (gh-762)
* Improve by substitute all backslashes to slashes for filename (gh-703)
* Improve internal Go package path function (gh-702)
* Improved typo and grammar errors in docs (gh-714)
* Improved internal `:GoInfo` automatic call (gh-759)

BUG FIXES:

* Fix oracle scope not working if trailing slash exists in scope (gh-751) 
* Fix `:GoErrCheck` checking abspath (gh-671)
* Fix `:GoInstall` correctly parsing errors (gh-692)
* Fix `:GoInstall` correctly parsing errors (gh-692)
* Fix `:GoTestFunc` for neovim (gh-695)
* Fix `:GoRun` accepting arguments for neovim (gh-730)
* Fix `go run` mappings not working (gh-542)
* Fix autodetect gopath picking up non existing GB vendor folder
* Fix gofmt errors showing per buffer instead of per script (gh-721)
* Fix some of the neosnippet snippets

## 1.4 (Jan 18, 2016)

FEATURES:

* You waited for it for a long time. And here you have it: **Neovim support!**
  This is a huge feature. It's fully compatible with Vim and kicks only in if
  vim-go is being used within Neovim. Checkout the full list of changes
  (gh-607):
  * An async launcher and base foundation was implemented for the `go` command.
	This will be used in the future for all upcoming subcommands of the `go`
	tool.
  * `:GoBuild` is now called asynchronously (it doesn't block the UI anymore). 
  * A new `go#jobcontrol#Statusline()` can be used to plug into the statusline.
	This will show the status of the job running asynchronously. The statusline
	is improved to show the status per package instead of file. Assume you have
	three files open, all belonging to the same package, if the package build
	(`:GoBuild`) is successful, all statusline's will be empty (means SUCCESS),
	if it fails all files statusline's will show `FAILED`. 
  * `:GoRun` opens a new vertical terminal emulator inside Neovim and runs the
	command there. The terminal mode can be changed with `g:go_term_mode`,
	which is by default `vsplit`. Current options are `vsplit, split or tab`.
	We also have three new mappings to open `:GoRun` command in different
	terminal split modes: `<Plug>(go-run-vertical)`,  `<Plug>(go-run-split)`
	and  `<Plug>(go-run-tab)`
  * `:GoTest`, `:GoTestFunc` and `:GoTestCompile` opens and runs in a new
	terminal. The view mode (split,vertical, tab) is defined with
	`g:go_term_mode`.  The `g:go_term_enabled` setting can be use to change the
	behavior of `:GoTestXXX` commands .If set to `1`, it opens the test
	commands inside a terminal, if not it runs them in background just like
	`:GoBuild` and displays the result in the statusline.
  * We have two settings for terminal sizes: `g:go_term_height` and
	`g:go_term_width`. By default a vertical or horizontal view is equally
	splitted by vim automatically. However with these settings we can for
	example have a terminal with a smaller height when we split it
	horizontally.
  * If a command inside the term fails (such as `go run`, `go test` ...) we
	parse now the errors and list them inside a location list.
* Instead of quickfix window, vim-go now uses the `location list` feature of
  Vim. These are associated with each window independently of each other. This
  enables us to have multiple, independent location lists per window (example
  usages: `:GoBuild` with errors that needs to be fixed, `:GoLint` with
  warnings that we want to check, `:GoReferrers` with a list of referred
  identifiers) (gh-626)
* a new **`:AsmFmt`** command which is integrated to work with [asmfmt](https://github.com/klauspost/asmfmt) (gh-673)
* the full identifier information of a completed identifier is echoed in
  statusline. This is very useful to see a function signatures arguments.
  (gh-685)

IMPROVEMENTS:

* Improve `:GoFmt` by checking if the binary is indeed installed on the system (gh-617)
* Improve `:GoMetaLinter` by adding the option to run the metalinter on save
  and adding the option to limit the output to the currently active buffer. Set
  `let g:go_metalinter_autosave = 1` to enable autosave and use `let
  g:go_metalinter_autosave_enabled = ['vet', 'golint']` to change your options.
  (gh-631)
* Improved `:GoDef`. If `vimproc` is installed `godef` will make use of it (gh-670)
* Improve completion of godoce when vimproc is used (gh-620)
* Improve internal error matching prodecure to not match false positives (gh-618)
* A new option to highlight interface variables with `go_highlight_interfaces` (gh-681)

BUG FIXES

* Fix `:GoFmt` changing the fileformat of the current buffer (gh-615)
* Fix `:GoRename` to output the original error if parsing fails (gh-675)
* Fix `:GoTest` to output the original error if parsing fails (gh-676)
* Fixed `fmt.Fprintln` not to highlight as builtin (gh-628)
* Fixed wrong highlighting of channels of channels (gh-678)

## 1.3 (Nov 22, 2015)

FEATURES:

* A new `:GoOracleTags` command was added to pass build tags to Oracle's `-tags` flag. (gh-573)

IMPROVEMENTS:

* Change `:GoTest` command to timeout after 10 seconds. Vim UI is blocking and
  tests with large running times makes Vim blocking for a long time. This is
  also customizable with the new option `g:go_test_timeout`. (gh-578)
* Improve `:GoRename` to collect and populate quickfix window with errors.
  (gh-577)
* Improve `:GoRun` by dropping bad filenames from quickfix window. This allows
  us to have only valid entries which can be jumped to (gh-547)
* Improve `:GoMetaLinter` quickfix output by using absolute paths. This enables
  us to jump to errors for all cases. (gh-565)
* Improve `:GoMetaLinter` command by adding a new option
  `g:go_metalinter_deadline` which cancels the linters after 5 seconds
  (previous default).  (gh-576)
* Improve `:GoMetaLinter` by jumping to the first encountered error from the quickfix window.
* Automatically resize quickfix window based on the number of errors (gh-602)
* Improve build constraints to show invalid cases (such as `// +buildfoo`, not
  having an empty line between the package statement, etc..). Also add missing
  `GOARCH` values sucha s `arm64`. There are many other useful improvements,
  for more detail please have a look at
  ([gh-589](https://github.com/fatih/vim-go/pull/589))
* Add support for all values of `GOARCH` (gh-601)
* Add note about Syntastic usage as this problem comes up a lot (gh-580)
* Add note about `:GoUpdateBinaries` (gh-606)

BUG FIXES:

* Fixed `:GoErrCheck` showing the correct output when executed inside the source folder (gh-564)
* Fixed `:GoBuild` by not using `/dev/null` anymore for build output (not
  supported by `go`). We pass a temporary file now. (gh-567)
* Fixed `:GoFmt` passing `g:go_fmt_options` options to `goimports`. This option
  is only valid with `gofmt`. (gh-590)
* Fix vim-go for `cygwin` users. (gh-575)
* Fixed identifier in template files to be highlighted correctly (gh-559)
* Fixed character region in template files to be highlighted correctly (gh-603)
* Fixed variables in template files to be highlighted correctly (gh-611)
* Do not treat builtins as keywords. Now `make` will not highlighted but
  `make()` will be highlighted (gh-605)

## 1.2 (Oct 2, 2015)

FEATURES:

* A new `:GoMetaLinter` command which invokes [gometalinter](https://github.com/alecthomas/gometalinter). Please check the PR [#553](https://github.com/fatih/vim-go/pull/553) for more detail on customizing and usage of `:GoMetaLinter`.

IMPROVEMENTS:

* Improve `:GoImport` to trim spaces when including import paths of form `"fmt "`
* Avoid setting `filetype` twice. Previously it was doing it twice, which was expensive
* Improve handling of GOPATH's with trailing `/` characters, such as `/home/user/go/`
* Add a new `g:go_highlight_string_spellcheck` feature, which is enabled by feature. Now if spell is enabled, go strings are also checked.
* Specify our limited but functional [gb](http://getgb.io/) support

BUG FIXES:
* Fixed `:GoRun` to display errors when `g:go_dispatch_enabled` was enabled
* Fixed `:GoDrop` displaying "Not enough arguments" (regression)
* Fixed `:GoErrCheck` not showing `PASS` message if the command was successful
* Fixed `:GoErrCheck` not executing in the directory of the currently edited file
* Close quickfix window after a successful second round of `:GoInstall`
* Fix passing files rather than packages to certain oracle commands.
* Escape files passed to oracle command. This could lead to some serious things.
* Clear `g:go_oracle_scope` when the scope was reseted. Previously it was set to empty string, which was causing false positives.
* Correct various misspellings.

## 1.1 (Jul 25, 2015)

With this release the version will now increase in `minor` levels. So the next
release will be `1.2`, the other one `1.3`, etc.. This provides us more
flexibility (like releasing patch versions if needed).

FEATURES:
* A new `:GoGenerate` command is now available which can be used to invoke `go generate` within vim
* Vim-go didn't had any license, now we use BSD 3-Clause License (the same as Go). This is needed for Linux distributions to package vim-go and is also something that people asked for.

IMPROVEMENTS:
* Improve commands `GoRun, GoTest{,Func,Compile}, GoCoverage,
  GoGenerate, GoErrcheck, GoLint, and GoVet` to handle multiple arguments.
  Previously this feature was limited to only certain commands. What this means
  is, for example `:GoVet . -all` will invoke `go tool vet . -all`
  automatically instead of plan `go vet`. This is one of the big changes in
  this release, so give it a try :)
* Improved `:GoFmt` command, which now uses the `-w` flag to
  write to the source code directly, instead of outputting it to stdout. This
  makes `:GoFmt` much more faster than the current implementation. This is one
  of the big changes in this release, so feedback is welcome!
* Improve `:GoImport` to have a `!` feature. Now when when called
  with a `!` appended it will go get it. i.e: `:GoImport!
  github.com/fatih/color`. Useful if `:GoImport` fails and you want to download
  it.
* Automatic GOPATH detections can now detect `gb` vendored folders. Some commands should now work without any problem when invoked on a `gb` project.
* All command arguments are now properly escaped for shell invocation. 
* Added the `-f` flag to :GoInstallBinaries command to support `git url.<base>.insteadOf` configuration
* Improve width and precision highlighting, such as `%s %5s %-5s %5.5f %.5f`
* Show an error if a region is not selected when `:GoFreeVars` is called

BUG FIXES:
* Fix`:GoDef` for files containing spaces. We know escape the files before passing to `:GoDef`
* Fix `:GoFmt` not picking up the correct GOPATH when the fmt command was set to `goimports`
* Fix and simplify README.md, add Wiki reference
* Fixed tagbar integration to show correct imports.


## 1.0.5 (May 26, 2015)

FEATURES:
* A new `:GoOracleScope` is added to change the oracle scope on-the-fly. It
  accepts import paths as arguments. If no arguments are passed it prints the
  current custom oracle scope. `:GoOracleScope` also supports completion of
  import paths, so it's very fast and handy to use. `:GoOracleScope ""` clears
  the current custom scope.
* A new `:GoPath` command that displays the current `GOPATH`. A path can be
  passed to change the `GOPATH` (i.e `:GoPath ~/foo/src`). `:GoPath ""` clears
  and resets the `GOPATH` to the initial value.
* A new "autodetect GOPATH" feature is added. This automatically detects if the
  project is using `godep` or is under a `src` root directory which is not in
  `GOPATH` and changes/modifies the `GOPATH` so all commands work based on this
  GOPATH. What this means is, commands such as `:GoDef`, `:GoBuild`, etc.. will
  include the Godeps folder. For example any go-to-definition via `:GoDef` will
  jump to the source code inside Godeps. This is enabled by default, but can
  disabled with `let g:go_autodetect_gopath = 0`. This new feature is also the
  foundation for other tools such as `gb` or `wgo`.

IMPROVEMENTS:
* Improve `:GoFmt` (gofmt and goimports) speed. Now it's 2x faster than the previous implementation.
* Add Dispatch support for `:GoBuild` and `:GoRun`. For more info about
  dispatch see https://github.com/tpope/vim-dispatch . By default it's
  disabled, to enable it add `let g:go_dispatch_enabled = 1` to your vimrc.
* Add support for the bang `!` attribute for all `go` tool commands. What this
  does it, if `:GoBuild` is called it will jump to the error. But `:GoBuild!`
  will not jump to any error. This has the same behavior as the internal
  `:make` command in vim. We had this feature already for `:GoBuild` and
  `:GoRun`. But not for `:GoInstall`, `:GoTest`, etc.. Now all commands are
  unified.
* Add autojump to error for `:GoInstall`.
* Add autowrite feature for `:GoInstall`, `:GoTestXXX` functions and `:GoVet`
* Support `git url.<base>.insteadOf` and custom import paths of binaries. This
  improves the commands `:GoInstallBinaries` and `:GoUpdateBinaries`.
* Add support for highlighting go templates with `*.tmpl` extensions. Based on
  the work from @cespare from https://github.com/cespare/vim-go-templates

BUG FIXES:
* Fix clearing the status bar when `:GoErrCheck` is called
* Fix godocNotFound to not match 'os' pkg contents. This improves the command
  `:GoDoc`
* Fix parsing and jumping to error locations when used Vim from a different
  directory than the current buffer's directory
* Fix completion showing duplicates paths for completion results, such as
  github.com/fatih/color and github.com/fatih/color/.

## 1.0.4 (Apr 28, 2015)

FEATURES:

* A new `:GoTestFunc` command (with appropriate
  mappings) is added. Run tests function which surrounds the current cursor
  location. Useful to test single tests.
* Highlight all Go operators. Previously not all
  operators were highlighted. As previously, to highlight options, enable it
  with by setting `g:go_highlight_operators` to 1 in your vimrc.

IMPROVEMENTS:

* Improved certain `:GoDoc` usages to show a better error message
* Improved `:GoRename` to have a default value for rename input. Avoids retyping similar words.
* Synced with latest Oracle version. `callgraph` is removed.
* Removed our custom referrers mode. New version of oracle now displays the matching lines.

BUG FIXES:

* Fixed the internal `executeInDir` function which was failing when ignorelist was not set properly.
* Fixed trailing slash for package completion with `:GoImport`
* Fixed paths in error list for Windows users.
* Fixed not showing "import cycle not allowed" error message when called `:GoBuild` or `:GoRun`
* Fixed users using vimproc requiring arguments to functions to be escaped.
* Fixed depth for test snippets
* Fixed neosnippet support loading snippet files the second time if necessary.

## 1.0.3 (Mar 7, 2015)

FEATURES:
* A new `:GoTestCompile` command (with appropriate mappings) is added. Useful to compile a test binary or show/fix compile errors in quickfix window

IMPROVEMENTS:
* `referrer` mode is improved to show referring lines in the quickfix window
* A new `errt` snippet is added, which expands to `if err != nil { t.Fatal(err) }`
* A new `errh` snippet is added, useful to be used in a `http.Handler`
* UltiSnips snippets are improved to take advance of Vim's `Visual` mode. For example selecting a block and typing `if` will create an if scope around the block. 
* Cleanup README.md

BUG FIXES:
* Fix trimming brackets if completion was invoked on a previous completion
* Fix Oracle scope settings. Added docs about usage.
* Fixed previously broken `var` and `vars` snippets
* Fix duplicate docs
* Fix fallback binary path for Windows users. The fallback mechanism is used to discover the necessary Go tools, such as `godef`, `gocode`, etc...

## 1.0.2 (Feb 17, 2015)

FEATURES:

* New snippets are added, mostly for testing ( [changes](https://github.com/fatih/vim-go/pull/321/files))

IMPROVEMENTS:

* Enable all Oracle commands. Docs, mappings and commands are also added. It uses Quickfix list instead of a custom UI.
* Clarify installation process in Readme, add instructions for vim-plug, NeoBundle and manual.

BUG FIXES:

* Fix shiftwidth parsing, it was broken in the previous release for old Vim versions
* Fix experimantal mode


## 1.0.1 (Feb 9, 2015)

FEATURES:

* New feature to highlight build constraints (disabled by default)

IMPROVEMENTS:

* Updated godef import path
* Updated Readme for possible problems with `csh`
* Documentation for text objects are updated, typo fixes are merged
* If vimproc is installed, Windows users will use it for autocompletion
* Improve UltiSnips snippets to pick Visual selection (demo: http://quick.as/0dvigz5)
* Packages with extensions, like "gopkg.in/yaml.v2" can be now displayed
* Packages with different import paths, like "github.com/bitly/go-simplejson" can be now displayed

BUG FIXES:

* Fatal errors are now parsed successfully and populated to quickfix list
* Shiftwidth is changed to use shiftwidth() function. Fixes usage with plugins like vim-sleuth and possible mis usage (like setting shiftwidth to zero)
* Added a new [Donation](https://github.com/fatih/vim-go#donations) section to Readme, for those who ask for it.
* Fix parsing of errcheck error syntax
* Fix consistency between Neosnippet and UltiSnips snippets


## 1.0 (Dec 24, 2014)

We don't tag any changes or releases, so let's start with `1.0`. Our Windows
support is now in a good shape, tons of bugs are fixed, many new features and
improvements is being added and it's getting better with each day (thanks to
the community contributions).

## 0.0 (Mar 24, 2014)

Initial commit: https://github.com/fatih/vim-go/commit/78c5caa82c111c50e9c219f222d65b07694f8f5a
