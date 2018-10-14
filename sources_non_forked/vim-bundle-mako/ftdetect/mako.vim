if !exists("g:mako_detect_lang_from_ext")
  let g:mako_detect_lang_from_ext = 1
endif
if g:mako_detect_lang_from_ext
  au BufNewFile *.*.mako   execute "do BufNewFile filetypedetect " . expand("<afile>:r") | let b:mako_outer_lang = &filetype
  " it's important to get this before any of the normal BufRead autocmds execute
  " for this file, otherwise a mako tag at the start of the file can cause the
  " filetype to be set to mason
  au BufReadPre *.*.mako   execute "do BufRead filetypedetect " . expand("<afile>:r") | let b:mako_outer_lang = &filetype
endif
au BufRead,BufNewFile *.mako     set filetype=mako
