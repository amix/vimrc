" word_complete.vim:	(global plugin) automatically offer word completion
" Last Change:		Wed 6 Dec 2006 12:51:42 PM EST
" Author:		Benji Fisher <benji@member.AMS.org>
" Version:		1.1, for Vim 7.0+
" URL:		http://vim.sourceforge.net/scripts/script.php?script_id=73

" DESCRIPTION:
"  Each time you type an alphabetic character, the script attempts 
"  to complete the current word.  The suggested completion is selected 
"  in Select mode, so you can just type another character to keep going. 
"  Other options:  <Tab> to accept, <BS> to get rid of the completion, 
"  <Esc> to leave Insert mode without accepting the completion, <C-N> 
"  or <C-P> to cycle through choices, <C-X> to enter <C-X> mode. 

" LIMITATIONS: 
"  The script works by :imap'ping each alphabetic character, and uses
"  Insert-mode completion (:help ins-completion).  It is far from perfect.  
"  Since characters you type are not entered in a single round of Insert mode,
"  abbreviations will usually not work.  I have at least one report that
"  mswin.vim interferes a little.  Since Select mode uses the same mappings as
"  Visual mode, the special keys mentioned above may conflict with what you
"  are used to in Visual mode.

" INSTALLATION:
"  :source it from your vimrc file or drop it in your plugin directory. 
"  To activate, choose "Word Completion" from the Tools menu, or type 
"  :call DoWordComplete() 
"  To make it stop, choose "Tools/Stop Completion", or type 
"  :call EndWordComplete()
"  If you want to activate the script by default, add the line
"  	autocmd BufEnter * call DoWordComplete()
"  to your vimrc file.

" USER CONFIGURATION:
"  Use this section to change some of the defaults.  Before upgrading to a new
"  version of this file, you should copy the following section to a file
"  word_complete.vimrc in your plugin directory, normally the same directory
"  as this file.  If your system administrator has installed this file, you
"  should install word_complete.vimrc in your after/plugin/ directory so that
"  your choices override the system defaults.  See
"    :help ftplugin-overrule
"    :help 'runtimepath'
"  for details.
" ==================== file word_complete.vimrc ====================
" User Configuration file for word_complete.vim .
" To use this, uncomment and change the defaults. 

" Do not complete words shorter than this length:
" let g:WC_min_len = 1
" Use this key to accept the offered completion:
" let g:WC_accept_key = "<Tab>"
" ==================== end: word_complete.vimrc ====================

" source the user configuration file(s):
runtime! plugin/<sfile>:t:r.vimrc
" Use the values from the configuration file(s) or the defaults:
let s:min_len = exists("g:WC_min_len") ? g:WC_min_len : 1
let s:accept_key = exists("g:WC_accept_key") ? g:WC_accept_key : "<Tab>"

" Use Vim defaults while :source'ing this file.
let save_cpo = &cpo
set cpo&vim

if has("menu")
  amenu &Tools.&Word\ Completion :call DoWordComplete()<CR>
  amenu &Tools.&Stop\ Completion :call EndWordComplete()<CR>
endif

" Return the :lmap value if there is one, otherwise echo the input.
fun! s:Langmap(char)
  let val = maparg(a:char, "l")
  return (val != "") ? val : a:char
endfun

" The :startinsert command does not have an option for acting like "a"
" instead of "i" so this implements it.
fun! s:StartAppend()
  if strlen(getline(".")) > col(".")
    normal l
    startinsert
  else
    startinsert!
  endif
endfun

fun! WordComplete()
  let length=strlen(expand("<cword>"))
  " Do not try to complete 1- nor 2-character words.
  if length < s:min_len
    call s:StartAppend()
    return
  endif 
  " Save and reset the 'ignorecase' option.
  let save_ic = &ignorecase
  set noignorecase
  " Use language maps (keymaps) if appropriate.
  if &iminsert == 1
    let char = getline(".")[col(".")-1]
    let lchar = maparg(char, "l")
    if lchar != ""
      execute "normal! r" . lchar
    endif
  endif
  " If at EOL or before a space or punctuation character, do completion.
  if strlen(getline(".")) == col(".")
	\ || getline(".")[col(".")] =~ '[[:punct:][:space:]]'
    execute "normal a\<C-P>\<Esc>"
  endif
  " If a match was found, highlight the completed part in Select mode.
  if strlen(expand("<cword>")) > length
    execute "normal viwo" . length . "l\<C-G>"
  else	" ... just return to Insert mode.
    if version > 505
      call s:StartAppend()
    else
      execute "normal a*\<Esc>gh"
    endif "version > 505
  endif
  " Restore the 'ignorecase' option.
  let &ignorecase = save_ic
endfun

" Make an :imap for each alphabetic character, and define a few :smap's.
fun! DoWordComplete()
  execute "snoremap <buffer>" s:accept_key "<Esc>`>a"
  snoremap <buffer> <Esc> d
  if has("mac")
    snoremap <buffer>  <Del>a
  else
    snoremap <buffer> <BS> <Del>a
  endif "has("mac")
  if version > 505
    snoremap <buffer> <C-N> <Del>a<C-N>
    snoremap <buffer> <C-P> <Del>a<C-P><C-P>
    snoremap <buffer> <C-X> <Del>a<C-P><C-X>
  endif "version > 505
  " Thanks to Bohdan Vlasyuk for suggesting a loop here:
  let letter = "a"
  while letter <=# "z"
    execute "inoremap <buffer>" letter letter . "<Esc>:call WordComplete()<CR>"
    let letter = nr2char(char2nr(letter) + 1)
  endwhile
endfun

" Remove all the mappings created by DoWordComplete().
" Lazy:  I do not save and restore existing mappings.
fun! EndWordComplete()
  execute "vunmap <buffer>" s:accept_key
  vunmap <buffer> <Esc>
  if has("mac")
    vunmap <buffer> 
  else
    vunmap <buffer> <BS>
  endif "has("mac")
  if version > 505
    vunmap <buffer> <C-N>
    vunmap <buffer> <C-P>
    vunmap <buffer> <C-X>
  endif "version > 505
  " Thanks to Bohdan Vlasyuk for suggesting a loop here:
  let letter = char2nr("a")
  while letter <= char2nr("z")
    execute "iunmap <buffer>" nr2char(letter)
    let letter = letter + 1
  endwhile
endfun

let &cpo = save_cpo

" vim:sts=2:sw=2:ff=unix:
