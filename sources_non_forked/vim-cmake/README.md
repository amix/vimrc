# Vim-CMake

Vim-CMake is a plugin for building CMake projects inside of Vim/Neovim, with a
nice visual feedback.

![lint](https://img.shields.io/circleci/build/github/cdelledonne/vim-cmake?label=lint&logo=circleci)
![language](https://img.shields.io/github/languages/top/cdelledonne/vim-cmake)
![version](https://img.shields.io/github/v/tag/cdelledonne/vim-cmake?label=version&sort=semver)
![license](https://img.shields.io/github/license/cdelledonne/vim-cmake)

![screencast][screencast]

**Features**

* Visual experience, shows CMake output in a console-like window
* Slick management of build configurations
* Autocompletion for build targets and build configurations
* Quickfix list population after each build
* Airline/statusline status information, including current build configuration
* Plug-and-play, but configurable
* Written in Vimscript

**Requirements**

* Vim with `+terminal`, or Neovim >= 0.5
* Under Windows, only Neovim is supported at the moment

<!--=========================================================================-->

## Installation

Use a package manager like [vim-plug][vim-plug]:

```vim
Plug 'cdelledonne/vim-cmake'
```

or Vim's native package manager:

```sh
mkdir -p ~/.vim/pack/plug/start
cd ~/.vim/pack/plug/start
git clone https://github.com/cdelledonne/vim-cmake.git
```

<!--=========================================================================-->

## Usage

Run `:CMakeGenerate` from the top-level CMake source directory to generate a
build system for the project.  Then, run `:CMakeBuild` to build the project.
The built files will end up in the binary directory ([out-of-source
build][oos]).  To switch between build configurations, run `:CMakeSwitch
<config>`.

With Vim-CMake, you can easily manage build configurations (Debug, Release,
etc.), build specific targets and control build options, and fix errors using
Vim's quickfix feature.  For a detailed explanation of commands, mappings and
functionalities run `:help cmake`.  A quick overview follows.

### Commands and `<Plug>` mappings

| Command                   | `<Plug>` mapping  | Description                           |
|:--------------------------|:------------------|:--------------------------------------|
| `:CMakeGenerate[!]`       | `(CMakeGenerate)` | Generate build system                 |
| `:CMakeClean`             | `(CMakeClean)`    | Remove build system and build files   |
| `:CMakeBuild[!] [target]` | `(CMakeBuild)`    | Build a project                       |
| `:CMakeInstall`           | `(CMakeInstall)`  | Install build output                  |
| `:CMakeSwitch <config>`   | `(CMakeSwitch)`   | Switch to another build configuration |
| `:CMakeOpen`              | `(CMakeOpen)`     | Open CMake console window             |
| `:CMakeClose`             | `(CMakeClose)`    | Close CMake console window            |
| `:CMakeStop`              | `(CMakeStop)`     | Stop running command                  |

### Additional `<Plug>` mappings

| `<Plug>` mapping     | Behaves as                                            |
|:---------------------|:------------------------------------------------------|
| `(CMakeBuildTarget)` | `(CMakeBuild)`, but leaves cursor in the command line |

### Key mappings in the CMake console window

| Key mapping | Description                |
|:------------|:---------------------------|
| `cg`        | Run `:CMakeGenerate`       |
| `cb`        | Run `:CMakeBuild`          |
| `ci`        | Run `:CMakeInstall`        |
| `cq`        | Close CMake console window |
| `<C-C>`     | Stop running command       |

### Events

Vim-CMake provides a set of custom events to trigger further actions.
Run `:help cmake` for an extensive documentation of all configuration options and examples

| Event                           | Description                               |
|:--------------------------------|:------------------------------------------|
| `User CMakeBuildSucceeded`      | Triggered after a successful `:CMakeBuild`|
| `User CMakeBuildFailed`         | Triggered after a failed `:CMakeBuild`    |

### Quickfix list

After each build (e.g. run with `:CMakeBuild`), Vim-CMake populates a quickfix
list to speedup the edit-compile-run cycle, similarly to when running `:make` in
Vim/Neovim.  Upon an unsuccessful build, just use the standard quickfix commands
to open the list of errors (e.g. `:copen`) and jump between errors (e.g.
`:cfirst`, `:cnext`).

<!--=========================================================================-->

## Configuration

Vim-CMake has sensible defaults. Again, run `:help cmake` for an extensive
documentation of all the configuration options.  A list of default values
follows.

| Options                         | Default            |
|:--------------------------------|:-------------------|
| `g:cmake_command`               | `'cmake'`          |
| `g:cmake_default_config`        | `'Debug'`          |
| `g:cmake_build_dir_location`    | `'.'`              |
| `g:cmake_generate_options`      | `[]`               |
| `g:cmake_build_options`         | `[]`               |
| `g:cmake_native_build_options`  | `[]`               |
| `g:cmake_console_size`          | `15`               |
| `g:cmake_console_position`      | `'botright'`       |
| `g:cmake_console_echo_cmd`      | `1`                |
| `g:cmake_jump`                  | `0`                |
| `g:cmake_jump_on_completion`    | `0`                |
| `g:cmake_jump_on_error`         | `1`                |
| `g:cmake_link_compile_commands` | `0`                |
| `g:cmake_root_markers`          | `['.git', '.svn']` |
| `g:cmake_log_file`              | `''`               |

<!--=========================================================================-->

## Contributing

Feedback and feature requests are appreciated.  Bug reports and pull requests
are very welcome.  Check the [Contributing Guidelines][contributing] for how to
write a feature request, post an issue or submit a pull request.

<!--=========================================================================-->

## Related projects

* [vhdirk/vim-cmake][vim-cmake]
* [ilyachur/cmake4vim][cmake4vim]
* [jalcine/cmake.vim][cmake.vim]
* [sigidagi/vim-cmake-project][vim-cmake-project]
* [LucHermitte/vim-build-tools-wrapper][LucHermitte/vim-build-tools-wrapper]
* [kassio/neoterm][neoterm]

<!--=========================================================================-->

## License

Vim-CMake is licensed under the [MIT license][license].  Copyright (c)
2020&ndash;2022 Carlo Delle Donne.

<!--=========================================================================-->

[screencast]: https://user-images.githubusercontent.com/24732205/88468329-18aad100-cee2-11ea-94f4-f2ac59a2e6b9.gif
[vim-cmake]: https://github.com/vhdirk/vim-cmake
[cmake4vim]: https://github.com/ilyachur/cmake4vim
[cmake.vim]: https://github.com/jalcine/cmake.vim
[vim-cmake-project]: https://github.com/sigidagi/vim-cmake-project
[LucHermitte/vim-build-tools-wrapper]: https://github.com/LucHermitte/vim-build-tools-wrapper
[neoterm]: https://github.com/kassio/neoterm
[vim-plug]: https://github.com/junegunn/vim-plug
[oos]: https://cprieto.com/posts/2016/10/cmake-out-of-source-build.html
[contributing]: ./CONTRIBUTING.md
[license]: ./LICENSE
