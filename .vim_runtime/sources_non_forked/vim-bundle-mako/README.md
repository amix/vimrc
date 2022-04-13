This project is a colleciton of vim scripts that relate  to the Mako templating
engine for python. Most of thse are not at all written by me, just packaged
here from the vim-script site. The purpose is to make them easy to use with
pathogen.vim.

Useful configuration variables:

* `g:mako_detect_lang_from_ext`: when set to 1 (the default), the ftdetect
  script will attempt to figure out the "outer" filetype of the file by
  stripping the ".mako" extension (eg: index.html.mako will be treated as HTML,
  while script.cpp.mako will be treated as C++). Set to 0 to prevent this
  detection.
* `g:mako_default_outer_lang`: if ftdetect cannot detect the "outer" filetype of
  the file, this sets the default filetype used. If not set, defaults to "html".

About mako: http://www.makotemplates.org/

Externally sourced scripts:

* [indent/mako.vim](http://www.vim.org/scripts/script.php?script_id=2663) 0.4 by Scott Torborg
* [syntax/mako.vim](http://www.vim.org/scripts/script.php?script_id=1858) 0.6.1 by Armin Ronacher

