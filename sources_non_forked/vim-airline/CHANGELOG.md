# Change Log

This is the Changelog for the vim-airline project.

## [Unreleased]
- Changes
  - Themes have been moved into an extra repository [vim-airline-themes](https://github.com/vim-airline/vim-airline-themes)
  - Many new themes
  - Airline Moved to new [repository](https://github.com/vim-airline/vim-airline)
- New features
  - Integration with [taboo](https://github.com/gcmt/taboo.vim), [vim-ctrlspace](https://github.com/szw/vim-ctrlspace),
    [quickfixsigns](https://github.com/tomtom/quickfixsigns_vim), [YouCompleteMe](https://github.com/Valloric/YouCompleteMe)
  - Support for Neovim
  - Added wordcount extension
  - Adding Crypt and Byte Order Mark Indicator

## [0.7] - 2014-12-10
- New features
    - accents support; allowing multiple colors/styles in the same section
    - extensions: eclim
    - themes: understated, monochrome, murmur, sol, lucius
- Improvements
    -  solarized theme; support for 8 color terminals
    -  tabline resizes dynamically based on number of open buffers
    -  miscellaneous bug fixes

## [0.6] - 2013-10-08

- New features
    - accents support; allowing multiple colors/styles in the same section
    - extensions: eclim
    - themes: understated, monochrome, murmur, sol, lucius
- Improvements
    - solarized theme; support for 8 color terminals
    - tabline resizes dynamically based on number of open buffers
    - miscellaneous bug fixes

## [0.5] - 2013-09-13

- New features
    - smart tabline extension which displays all buffers when only one tab is visible
    - automatic section truncation when the window resizes
    - support for a declarative style of configuration, allowing parts to contain metadata such as minimum window width or conditional visibility
    - themes: zenburn, serene
- Other
    - a sizable chunk of vim-airline is now running through a unit testing suite, automated via Travis CI

## [0.4] - 2013-08-26

 - New features
    - integration with csv.vim and vim-virtualenv
    - hunks extension for vim-gitgutter and vim-signify
    - automatic theme switching with matching colorschemes
    - commands: AirlineToggle
    - themes: base16 (all variants)
 - Improvements
    - integration with undotree, tagbar, and unite
 - Other
    - refactored core and exposed statusline builder and pipeline
    - all extension related g:airline_variables have been deprecated in favor of g:airline#extensions# variables
    - extensions found in the runtimepath outside of the default distribution will be automatically loaded

## [0.3] - 2013-08-12

-  New features
    -  first-class integration with tagbar
    -  white space detection for trailing spaces and mixed indentation
    -  introduced warning section for syntastic and white space detection
    -  improved ctrlp integration: colors are automatically selected based on the current airline theme
    -  new themes: molokai, bubblegum, jellybeans, tomorrow
-  Bug fixes
    -  improved handling of eventignore used by other plugins
-  Other
    - code cleaned up for clearer separation between core functionality and extensions
    - introduced color extraction from highlight groups, allowing themes to be generated off of the active colorscheme (e.g. jellybeans and tomorrow)
    - License changed to MIT

## [0.2] - 2013-07-28

-  New features
      - iminsert detection
      - integration with vimshell, vimfiler, commandt, lawrencium
      - enhanced bufferline theming
      - support for ctrlp theming
      - support for custom window excludes
- New themes
      - luna and wombat
- Bug fixes
      - refresh branch name after switching with a shell command

## [0.1] - 2013-07-17

- Initial release
  - integration with other plugins: netrw, unite, nerdtree, undotree, gundo, tagbar, minibufexplr, ctrlp
  - support for themes: 8 included

[Unreleased]: https://github.com/vim-airline/vim-airline/compare/v0.7...HEAD
[0.7]: https://github.com/vim-airline/vim-airline/compare/v0.6...v0.7
[0.6]: https://github.com/vim-airline/vim-airline/compare/v0.5...v0.6
[0.5]: https://github.com/vim-airline/vim-airline/compare/v0.4...v0.5
[0.4]: https://github.com/vim-airline/vim-airline/compare/v0.3...v0.4
[0.3]: https://github.com/vim-airline/vim-airline/compare/v0.2...v0.3
[0.2]: https://github.com/vim-airline/vim-airline/compare/v0.1...v0.2
[0.1]: https://github.com/vim-airline/vim-airline/releases/tag/v0.1
