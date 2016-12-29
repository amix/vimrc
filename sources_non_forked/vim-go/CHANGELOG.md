## 1.11 - Unplanned

FEATURES:

* Add new `g:go_updatetime` setting to change the default updatetime (which was hardcoded previously) [gh-1055]
* Add new `g:go_template_use_pkg` setting to enable to use cwd as package name instead of basic template file [gh-1124]

IMPROVEMENTS:

* Add `statusline` support for `:GoMetaLinter` [gh-1120]
* Quickfix and Location lists contain now a descriptive title (requires at least Vim `7.4.2200`)[gh-1004]

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
* Add new `g:go_gocode_socket_type` setting to change the underlying socket type passed to `gocode`. Usefull to fallback to `tcp` on cases such as Bash on Windows [gh-1000]
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
* Fix `:GoCoverage` accidently enabling syntax highlighting for users who don't use syntax (i.e syntax off) [gh-827]
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


## Previous releases

Previous changelogs can be found here: https://github.com/fatih/vim-go/releases

