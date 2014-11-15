" Vim file type plug-in
" Author: Peter Odding <peter@peterodding.com>
" Last Change: May 16, 2013
" URL: http://peterodding.com/code/vim/notes/

if exists('b:did_ftplugin')
  finish
else
  let b:did_ftplugin = 1
endif

" Copy indent from previous line. {{{1
setlocal autoindent
let b:undo_ftplugin = 'set autoindent<'

" Set &tabstop and &shiftwidth options for bulleted lists. {{{1
setlocal tabstop=3 shiftwidth=3 expandtab
let b:undo_ftplugin .= ' | set tabstop< shiftwidth< expandtab<'

" Automatic formatting for bulleted lists. {{{1
let &l:comments = xolox#notes#get_comments_option()
setlocal formatoptions=tcron
let b:undo_ftplugin .= ' | set comments< formatoptions<'

" Automatic text folding based on headings. {{{1
setlocal foldmethod=expr
setlocal foldexpr=xolox#notes#foldexpr()
setlocal foldtext=xolox#notes#foldtext()
let b:undo_ftplugin .= ' | set foldmethod< foldexpr< foldtext<'

" Enable concealing of notes syntax markers? {{{1
if has('conceal')
  setlocal conceallevel=3
  let b:undo_ftplugin .= ' | set conceallevel<'
endif

" Change <cfile> to jump to notes by name. {{{1
setlocal includeexpr=xolox#notes#include_expr(v:fname)
let b:undo_ftplugin .= ' | set includeexpr<'

" Enable completion of note titles using C-x C-u. {{{1
setlocal completefunc=xolox#notes#user_complete
let b:undo_ftplugin .= ' | set completefunc<'

" Enable completion of tag names using C-x C-o. {{{1
setlocal omnifunc=xolox#notes#omni_complete
let b:undo_ftplugin .= ' | set omnifunc<'

" Automatic completion of tag names after typing "@". {{{1

inoremap <buffer> <silent> @ @<C-x><C-o>
let b:undo_ftplugin .= ' | execute "iunmap <buffer> @"'

" Automatic completion of tag names should not interrupt the flow of typing,
" for this we have to change the (unfortunately) global option &completeopt.
set completeopt+=longest

" Change double-dash to em-dash as it is typed. {{{1
if g:notes_smart_quotes && xolox#notes#unicode_enabled()
  inoremap <buffer> -- —
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> --"'
endif

" Change plain quotes to curly quotes as they're typed. {{{1
if g:notes_smart_quotes
  inoremap <buffer> <expr> ' xolox#notes#insert_quote(1)
  inoremap <buffer> <expr> " xolox#notes#insert_quote(2)
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> ''"'
  let b:undo_ftplugin .= ' | execute ''iunmap <buffer> "'''
endif

" Change ASCII style arrows to Unicode arrows. {{{1
if g:notes_smart_quotes && xolox#notes#unicode_enabled()
  inoremap <buffer> -> →
  inoremap <buffer> <- ←
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> ->"'
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> <-"'
endif

" Convert ASCII list bullets to Unicode bullets. {{{1
if g:notes_smart_quotes
  inoremap <buffer> <expr> * xolox#notes#insert_bullet('*')
  inoremap <buffer> <expr> - xolox#notes#insert_bullet('-')
  inoremap <buffer> <expr> + xolox#notes#insert_bullet('+')
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> *"'
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> -"'
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> +"'
endif

" Format three asterisks as a horizontal ruler. {{{1
inoremap <buffer> *** <C-o>:call xolox#notes#insert_ruler()<CR>
let b:undo_ftplugin .= ' | execute "iunmap <buffer> ***"'

" Indent list items using <Tab> and <Shift-Tab>? {{{1
if g:notes_tab_indents
  inoremap <buffer> <silent> <Tab> <C-o>:call xolox#notes#indent_list(1, line('.'), line('.'))<CR>
  snoremap <buffer> <silent> <Tab> <C-o>:<C-u>call xolox#notes#indent_list(1, line("'<"), line("'>"))<CR><C-o>gv
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> <Tab>"'
  let b:undo_ftplugin .= ' | execute "sunmap <buffer> <Tab>"'
  inoremap <buffer> <silent> <S-Tab> <C-o>:call xolox#notes#indent_list(-1, line('.'), line('.'))<CR>
  snoremap <buffer> <silent> <S-Tab> <C-o>:<C-u>call xolox#notes#indent_list(-1, line("'<"), line("'>"))<CR><C-o>gv
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> <S-Tab>"'
  let b:undo_ftplugin .= ' | execute "sunmap <buffer> <S-Tab>"'
endif

" Indent list items using <Alt-Left> and <Alt-Right>? {{{1
if g:notes_alt_indents
  inoremap <buffer> <silent> <A-Right> <C-o>:call xolox#notes#indent_list(1, line('.'), line('.'))<CR>
  snoremap <buffer> <silent> <A-Right> <C-o>:<C-u>call xolox#notes#indent_list(1, line("'<"), line("'>"))<CR><C-o>gv
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> <A-Right>"'
  let b:undo_ftplugin .= ' | execute "sunmap <buffer> <A-Right>"'
  inoremap <buffer> <silent> <A-Left> <C-o>:call xolox#notes#indent_list(-1, line('.'), line('.'))<CR>
  snoremap <buffer> <silent> <A-Left> <C-o>:<C-u>call xolox#notes#indent_list(-1, line("'<"), line("'>"))<CR><C-o>gv
  let b:undo_ftplugin .= ' | execute "iunmap <buffer> <A-Left>"'
  let b:undo_ftplugin .= ' | execute "sunmap <buffer> <A-Left>"'
endif

" Automatically remove empty list items on Enter. {{{1
inoremap <buffer> <silent> <expr> <CR> xolox#notes#cleanup_list()
let b:undo_ftplugin .= ' | execute "iunmap <buffer> <CR>"'

" Shortcuts to create new notes from the selected text. {{{1

vnoremap <buffer> <silent> <Leader>en :NoteFromSelectedText<CR>
let b:undo_ftplugin .= ' | execute "vunmap <buffer> <Leader>en"'

vnoremap <buffer> <silent> <Leader>sn :SplitNoteFromSelectedText<CR>
let b:undo_ftplugin .= ' | execute "vunmap <buffer> <Leader>sn"'

vnoremap <buffer> <silent> <Leader>tn :TabNoteFromSelectedText<CR>
let b:undo_ftplugin .= ' | execute "vunmap <buffer> <Leader>tn"'

" }}}1

" This is currently the only place where a command is guaranteed to be
" executed when the user edits a note. Maybe I shouldn't abuse this (it
" doesn't feel right ;-) but for now it will do.
call xolox#notes#recent#track()
call xolox#notes#check_sync_title()

" vim: ts=2 sw=2 et
