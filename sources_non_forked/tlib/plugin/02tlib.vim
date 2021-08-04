" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Created:     2007-04-10.
" @Last Change: 2019-04-09.
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    836
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" GetLatestVimScripts: 1863 1 tlib.vim
" tlib.vim -- Some utility functions

if &cp || exists("g:loaded_tlib")
    finish
endif
if v:version < 700 "{{{2
    echoerr "tlib requires Vim >= 7"
    finish
endif
let g:loaded_tlib = 127

let s:save_cpo = &cpo
set cpo&vim


" :display: :TLet VAR = VALUE
" Set a variable only if it doesn't already exist.
" EXAMPLES: >
"   TLet foo = 1
"   TLet foo = 2
"   echo foo
"   => 1
command! -nargs=+ TLet if !exists(matchstr(<q-args>, '^[^=[:space:]]\+')) | exec 'let '. <q-args> | endif


" Open a scratch buffer (a buffer without a file).
"   TScratch  ... use split window
"   TScratch! ... use the whole frame
" This command takes an (inner) dictionary as optional argument.
" EXAMPLES: >
"   TScratch 'scratch': '__FOO__'
"   => Open a scratch buffer named __FOO__
command! -bar -nargs=* -bang TScratch call tlib#scratch#UseScratch({'scratch_split': empty('<bang>'), <args>})


" :display: :TVarArg VAR1, [VAR2, DEFAULT2] ...
" A convenience wrapper for |tlib#arg#Let|.
" EXAMPLES: >
"   function! Foo(...)
"       TVarArg ['a', 1], 'b'
"       echo 'a='. a
"       echo 'b='. b
"   endf
command! -nargs=+ TVarArg exec tlib#arg#Let([<args>])


" :display: :TBrowseOutput COMMAND
" Ever wondered how to efficiently browse the output of a command 
" without redirecting it to a file? This command takes a command as 
" argument and presents the output via |tlib#input#List()| so that you 
" can easily search for a keyword (e.g. the name of a variable or 
" function) and the like.
"
" If you press enter, the selected line will be copied to the command 
" line. Press ESC to cancel browsing.
"
" EXAMPLES: >
"   TBrowseOutput 20verb TeaseTheCulprit
command! -nargs=1 -complete=command TBrowseOutput call tlib#cmd#BrowseOutput(<q-args>)


" :display: :TBrowseScriptnames
" List all sourced script names (the output of ':scriptnames').
"
" When you press enter, the selected script will be opened in the current
" window. Press ESC to cancel.
"
" EXAMPLES: >
"   TBrowseScriptnames 
command! -nargs=0 TBrowseScriptnames call tlib#cmd#TBrowseScriptnames()


" :display: :Texecqfl CMD
" Run CMD and display the quickfix list.
command! -nargs=1 Texecqfl <args> | call tlib#qfl#QflList(getqflist())


" :display: :Texecloc CMD
" Run CMD and display the quickfix list.
command! -nargs=1 Texecloc <args> | call tlib#qfl#QflList(getloclist(0))


" :display: :Tlibtrace GUARD, VAR1, VAR2...
" Do nothing unless |tlib#trace#Enable()| was called.
" 
" When |:Tlibtraceset| or |tlib#trace#Enable()| were called:
"
" If GUARD is a number that evaluates to true or if it is a string that 
" matches a |regexp|, which was added using Tlibtrace! (with '!'), 
" display the values of VAR1, VAR2 ...
command! -nargs=+ -bang Tlibtrace :


" :Tlibtraceset[!] [--file=FILE] +RX1 -RX2...
" If |tlib#trace#Enable()| was called: With the optional <bang>, users 
" can add and remove GUARDs (actually a |regexp|) that should be traced.
"
" If no `+` or `-` is prepended, assume `+`.
"
" With the optional bang '!', reset any options.
command! -nargs=+ -bang Tlibtraceset call tlib#trace#Set(tlib#arg#GetOpts([<f-args>], {'short': 0}), !empty("<bang>"))


" :display: :Tlibtrace ASSERTION
command! -nargs=+ -bang Tlibassert :

" :display: :Tlibtype val, 'type', ...
command! -nargs=+ Tlibtype :


" Browse the current |quickfix| list.
command! -bar Tbrowseqfl call tlib#qfl#Browse()

" Browse the current |location-list|.
command! -bar Tbrowseloc call tlib#loclist#Browse()


let &cpo = s:save_cpo
unlet s:save_cpo
