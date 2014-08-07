" Vim plug-in
" Author: Peter Odding <peter@peterodding.com>
" Last Change: August 19, 2013
" URL: http://peterodding.com/code/vim/notes/

" Support for automatic update using the GLVS plug-in.
" GetLatestVimScripts: 3375 1 :AutoInstall: notes.zip

" Don't source the plug-in when it's already been loaded or &compatible is set.
if &cp || exists('g:loaded_notes')
  finish
endif

" Make sure vim-misc is installed.
try
  " The point of this code is to do something completely innocent while making
  " sure the vim-misc plug-in is installed. We specifically don't use Vim's
  " exists() function because it doesn't load auto-load scripts that haven't
  " already been loaded yet (last tested on Vim 7.3).
  call type(g:xolox#misc#version)
catch
  echomsg "Warning: The vim-notes plug-in requires the vim-misc plug-in which seems not to be installed! For more information please review the installation instructions in the readme (also available on the homepage and on GitHub). The vim-notes plug-in will now be disabled."
  let g:loaded_notes = 1
  finish
endtry

" Initialize the configuration defaults.
call xolox#notes#init()

" User commands to create, delete and search notes.
command! -bar -bang -nargs=? -complete=customlist,xolox#notes#cmd_complete Note call xolox#notes#edit(<q-bang>, <q-args>)
command! -bar -bang -nargs=? -complete=customlist,xolox#notes#cmd_complete DeleteNote call xolox#notes#delete(<q-bang>, <q-args>)
command! -bang -nargs=? -complete=customlist,xolox#notes#keyword_complete SearchNotes call xolox#notes#search(<q-bang>, <q-args>)
command! -bar -bang RelatedNotes call xolox#notes#related(<q-bang>)
command! -bar -bang -nargs=? RecentNotes call xolox#notes#recent#show(<q-bang>, <q-args>)
command! -bar -bang MostRecentNote call xolox#notes#recent#edit(<q-bang>)
command! -bar -count=1 ShowTaggedNotes call xolox#notes#tags#show_tags(<count>)
command! -bar IndexTaggedNotes call xolox#notes#tags#create_index()
command! -bar NoteToMarkdown call xolox#notes#markdown#view()
command! -bar NoteToHtml call xolox#notes#html#view()

" TODO Generalize this so we have one command + modifiers (like :tab)?
command! -bar -bang -range NoteFromSelectedText call xolox#notes#from_selection(<q-bang>, 'edit')
command! -bar -bang -range SplitNoteFromSelectedText call xolox#notes#from_selection(<q-bang>, 'vsplit')
command! -bar -bang -range TabNoteFromSelectedText call xolox#notes#from_selection(<q-bang>, 'tabnew')

" Automatic commands to enable the :edit note:… shortcut and load the notes file type.

augroup PluginNotes
  autocmd!
  au SwapExists * call xolox#notes#swaphack()
  au BufUnload * call xolox#notes#unload_from_cache()
  au BufReadPost,BufWritePost * call xolox#notes#refresh_syntax()
  au InsertEnter,InsertLeave * call xolox#notes#refresh_syntax()
  au CursorHold,CursorHoldI * call xolox#notes#refresh_syntax()
  " NB: "nested" is used here so that SwapExists automatic commands apply
  " to notes (which is IMHO better than always showing the E325 prompt).
  au BufReadCmd note:* nested call xolox#notes#shortcut()
  " Automatic commands to read/write notes (used for automatic renaming).
  exe 'au BufReadCmd' xolox#notes#autocmd_pattern(g:notes_shadowdir, 0) 'call xolox#notes#edit_shadow()'
  for s:directory in xolox#notes#find_directories(0)
    exe 'au BufWriteCmd' xolox#notes#autocmd_pattern(s:directory, 1) 'call xolox#notes#save()'
  endfor
  unlet s:directory
augroup END

augroup filetypedetect
  let s:template = 'au BufNewFile,BufRead %s if &bt == "" | setl ft=notes | end'
  for s:directory in xolox#notes#find_directories(0)
    execute printf(s:template, xolox#notes#autocmd_pattern(s:directory, 1))
  endfor
  unlet s:directory
  execute printf(s:template, xolox#notes#autocmd_pattern(g:notes_shadowdir, 0))
augroup END

" Make sure the plug-in is only loaded once.
let g:loaded_notes = 1

" vim: ts=2 sw=2 et
