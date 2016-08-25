## Unplanned

IMPROVEMENTS:

* **guru** uses now the `-modified` flag, which allows us use guru on modified
  buffers as well. This affects all commands where `guru` is used. Such as
  `:GoDef`, `:GoReferrers`, etc.. [gh-944]
* Cleanup and improve documentation [gh-987]
* Add new `g:go_gocode_socket_type` setting to change the underlying socket type passed to `gocode`. Usefull to fallback to `tcp` on cases such as Bash on Windows [gh-1000]
* `:GoSameIds` is now automatically re-evaluated in cases of buffer reloads (such as `:GoRename`) [gh-998]

BUG FIXES:

* Fix system calls on Windows [gh-988]
* Fix :GoSameIds and :GoCoverage for light background and after changing color schemes [gh-983]
* Fix TagBar and `GoCallers` for Windows user [gh-999]
* Set updatetime for for `auto_sameids` feature as well [gh-1016]


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

