" Vim script
" Author: Peter Odding
" Last Change: July 6, 2014
" URL: http://peterodding.com/code/vim/session/

" Support for automatic update using the GLVS plug-in.
" GetLatestVimScripts: 3150 1 :AutoInstall: session.zip

" Don't load the plug-in when &compatible is set or it was already loaded.
if &cp || exists('g:loaded_session')
  finish
endif

" Make sure vim-misc is installed. {{{1

try
  " The point of this code is to do something completely innocent while making
  " sure the vim-misc plug-in is installed. We specifically don't use Vim's
  " exists() function because it doesn't load auto-load scripts that haven't
  " already been loaded yet (last tested on Vim 7.3).
  call type(g:xolox#misc#version)
catch
  echomsg "Warning: The vim-session plug-in requires the vim-misc plug-in which seems not to be installed! For more information please review the installation instructions in the readme (also available on the homepage and on GitHub). The vim-session plug-in will now be disabled."
  let g:loaded_session = 1
  finish
endtry

" Configuration defaults. {{{1

" The name of the default session (without directory or filename extension).
if !exists('g:session_default_name')
  let g:session_default_name = 'default'
endif

" If you set this to 1 (true), every Vim instance without an explicit session
" loaded will overwrite the default session (the last Vim instance wins).
if !exists('g:session_default_overwrite')
  let g:session_default_overwrite = 0
endif

" The file extension of session scripts.
if !exists('g:session_extension')
  let g:session_extension = '.vim'
endif

" When you start Vim without opening any files the plug-in will prompt you
" whether you want to load the default session. Other supported values for
" this option are 'yes' (to load the default session without prompting) and
" 'no' (don't prompt and don't load the default session).
if !exists('g:session_autoload')
  let g:session_autoload = 'prompt'
endif

" When you quit Vim the plug-in will prompt you whether you want to save your
" current session. Other supported values for this option are 'yes' (to save
" the session without prompting) and 'no' (don't prompt and don't save the
" session).
if !exists('g:session_autosave')
  let g:session_autosave = 'prompt'
endif

" Periodically save the active session automatically? Set this to the
" auto-save interval in minutes. The value zero disables the feature
" (this is the default).
if !exists('g:session_autosave_periodic')
  let g:session_autosave_periodic = 0
endif

" Define the verbosity of messages.
if !exists('g:session_verbose_messages')
  let g:session_verbose_messages = 1
endif

" The session plug-in can automatically open sessions in three ways: based on
" Vim's server name, by remembering the last used session or by opening the
" default session. Enable this option to use the second approach.
if !exists('g:session_default_to_last')
  let g:session_default_to_last = 0
endif

" List with global variables and &options to persist between sessions.
if !exists('g:session_persist_globals')
  let g:session_persist_globals = []
endif

" On UNIX the :RestartVim command will pass the following environment
" variables on to the new instance of Vim.
if !exists('g:session_restart_environment')
  let g:session_restart_environment = ['TERM', 'VIM', 'VIMRUNTIME']
endif

" The default directory where session scripts are stored.
if !exists('g:session_directory')
  if xolox#misc#os#is_win()
    let g:session_directory = '~\vimfiles\sessions'
  else
    let g:session_directory = '~/.vim/sessions'
  endif
endif

" Define session command aliases of the form "Session" + Action in addition
" to the real command names which are of the form Action + "Session"?
if !exists('g:session_command_aliases')
  let g:session_command_aliases = 0
endif

" Allow to turn off the menu.
if !exists('g:session_menu')
  let g:session_menu = 1
endif

" Toggle the persistence of color schemes and the 'background' option.
if !exists('g:session_persist_colors')
  let g:session_persist_colors = 1
endif

" Enable user defined session name completion suggestions for :SaveSession.
if !exists('g:session_name_suggestion_function')
  let g:session_name_suggestion_function = 'xolox#session#suggestions#vcs_feature_branch'
endif

" Make sure the sessions directory exists and is writable. {{{1

let s:directory = fnamemodify(g:session_directory, ':p')
if !isdirectory(s:directory)
  call mkdir(s:directory, 'p')
endif
if filewritable(s:directory) != 2
  let s:msg = "session.vim %s: The sessions directory %s isn't writable!"
  call xolox#misc#msg#warn(s:msg, g:xolox#session#version, string(s:directory))
  unlet s:msg
  finish
endif
unlet s:directory

" Menu items to make the plug-in more accessible. {{{1

if g:session_menu
  amenu 400.10 &Sessions.&Open\ session\.\.\.<Tab>:OpenSession :OpenSession<CR>
  amenu 400.20 &Sessions.&Save\ session\.\.\.<Tab>:SaveSession :SaveSession<CR>
  amenu 400.30 &Sessions.&Close\ session\.\.\.<Tab>:CloseSession :CloseSession<CR>
  amenu 400.40 &Sessions.&Delete\ session\.\.\.<Tab>:DeleteSession :DeleteSession<CR>
  amenu 400.50 &Sessions.&View\ session\.\.\.<Tab>:ViewSession :ViewSession<CR>
  amenu 400.60 &Sessions.-Sep1- :
  amenu 400.70 &Sessions.Open\ tab\ session\.\.\.<Tab>:OpenTabSession :OpenTabSession<CR>
  amenu 400.80 &Sessions.&Append\ tab\ session\.\.\.<Tab>:AppendTabSession :AppendTabSession<CR>
  amenu 400.90 &Sessions.Save\ tab\ session\.\.\.<Tab>:SaveTabSession :SaveTabSession<CR>
  amenu 400.100 &Sessions.Close\ tab\ session\.\.\.<Tab>:CloseTabSession :CloseTabSession<CR>
  amenu 400.110 &Sessions.-Sep2- :
  amenu 400.120 &Sessions.&Restart\ Vim\.\.\.<Tab>:RestartVim :RestartVim<CR>
endif

" Automatic commands for automatic session management. {{{1

augroup PluginSession
  autocmd!
  au VimEnter * nested call xolox#session#auto_load()
  au VimLeavePre * call xolox#session#auto_save()
  au VimLeavePre * call xolox#session#auto_unlock()
augroup END

call xolox#misc#cursorhold#register({'function': 'xolox#session#auto_save_periodic', 'interval': 60})

" Plug-in commands (user defined commands). {{{1

" Define commands that enable users to manage multiple named, heavy-weight
" sessions (used to persist/restore a complete Vim editing session including
" one or more tab pages).
command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names OpenSession call xolox#session#open_cmd(<q-args>, <q-bang>, 'OpenSession')
command! -bar -nargs=? -complete=customlist,xolox#session#complete_names ViewSession call xolox#session#view_cmd(<q-args>)
command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names_with_suggestions SaveSession call xolox#session#save_cmd(<q-args>, <q-bang>, 'SaveSession')
command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names DeleteSession call xolox#session#delete_cmd(<q-args>, <q-bang>)
command! -bar -bang CloseSession call xolox#session#close_cmd(<q-bang>, 0, 1, 'CloseSession')

" Define commands that enable users to manage multiple named, light-weight
" sessions (used to persist/restore the window layout of a single tab page).
command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names OpenTabSession call xolox#session#open_tab_cmd(<q-args>, <q-bang>, 'OpenTabSession')
command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names SaveTabSession call xolox#session#save_tab_cmd(<q-args>, <q-bang>, 'SaveTabSession')
command! -bar -bang -range=-1 -nargs=? -complete=customlist,xolox#session#complete_names AppendTabSession call xolox#session#append_tab_cmd(<q-args>, <q-bang>, <count>, 'AppendTabSession')
command! -bar -bang CloseTabSession call xolox#session#close_tab_cmd(<q-bang>, 'CloseTabSession')

" Define a command to restart Vim editing sessions.
command! -bang -nargs=* -complete=command RestartVim call xolox#session#restart_cmd(<q-bang>, <q-args>)

" Plug-in command aliases. {{{2

if g:session_command_aliases
  " Define command aliases of the form "Session" + Action in addition to
  " the real command names which are of the form Action + "Session" (above).
  command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names SessionOpen call xolox#session#open_cmd(<q-args>, <q-bang>, 'SessionOpen')
  command! -bar -nargs=? -complete=customlist,xolox#session#complete_names SessionView call xolox#session#view_cmd(<q-args>)
  command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names SessionSave call xolox#session#save_cmd(<q-args>, <q-bang>, 'SessionSave')
  command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names SessionDelete call xolox#session#delete_cmd(<q-args>, <q-bang>)
  command! -bar -bang SessionClose call xolox#session#close_cmd(<q-bang>, 0, 1, 'SessionClose')
  command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names SessionTabOpen call xolox#session#open_tab_cmd(<q-args>, <q-bang>, 'SessionTabOpen')
  command! -bar -bang -nargs=? -complete=customlist,xolox#session#complete_names SessionTabSave call xolox#session#save_tab_cmd(<q-args>, <q-bang>, 'SessionTabSave')
  command! -bar -bang -range=-1 -nargs=? -complete=customlist,xolox#session#complete_names SessionTabAppend call xolox#session#append_tab_cmd(<q-args>, <q-bang>, <count>, 'SessionTabAppend')
  command! -bar -bang SessionTabClose call xolox#session#close_tab_cmd(<q-bang>, 'SessionTabClose')
endif

" Don't reload the plug-in once it has loaded successfully. {{{1

let g:loaded_session = 1

" vim: ts=2 sw=2 et
