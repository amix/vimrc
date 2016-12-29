" =============================================================================
" File:          plugin/ctrlp.vim
" Description:   Fuzzy file, buffer, mru, tag, etc finder.
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================
" GetLatestVimScripts: 3736 1 :AutoInstall: ctrlp.zip

if ( exists('g:loaded_ctrlp') && g:loaded_ctrlp ) || v:version < 700 || &cp
	fini
en
let g:loaded_ctrlp = 1

let [g:ctrlp_lines, g:ctrlp_allfiles, g:ctrlp_alltags, g:ctrlp_alldirs,
	\ g:ctrlp_allmixes, g:ctrlp_buftags, g:ctrlp_ext_vars, g:ctrlp_builtins]
	\ = [[], [], [], [], {}, {}, [], 2]

if !exists('g:ctrlp_map') | let g:ctrlp_map = '<c-p>' | en
if !exists('g:ctrlp_cmd') | let g:ctrlp_cmd = 'CtrlP' | en

com! -n=? -com=dir CtrlP         cal ctrlp#init(0, { 'dir': <q-args> })
com! -n=? -com=dir CtrlPMRUFiles cal ctrlp#init('mru', { 'dir': <q-args> })

com! -bar CtrlPBuffer   cal ctrlp#init('buf')
com! -n=? CtrlPLastMode cal ctrlp#init(-1, { 'args': <q-args> })

com! -bar CtrlPClearCache     cal ctrlp#clr()
com! -bar CtrlPClearAllCaches cal ctrlp#clra()

com! -bar ClearCtrlPCache     cal ctrlp#clr()
com! -bar ClearAllCtrlPCaches cal ctrlp#clra()

com! -bar CtrlPCurWD   cal ctrlp#init('fil', { 'mode': '' })
com! -bar CtrlPCurFile cal ctrlp#init('fil', { 'mode': 'c' })
com! -bar CtrlPRoot    cal ctrlp#init('fil', { 'mode': 'r' })

exe 'nn <silent> <plug>(ctrlp) :<c-u>'.g:ctrlp_cmd.'<cr>'

if g:ctrlp_map != '' && !hasmapto('<plug>(ctrlp)')
	exe 'map' g:ctrlp_map '<plug>(ctrlp)'
en

cal ctrlp#mrufiles#init()

com! -bar CtrlPTag      cal ctrlp#init(ctrlp#tag#id())
com! -bar CtrlPQuickfix cal ctrlp#init(ctrlp#quickfix#id())

com! -n=? -com=dir CtrlPDir
	\ cal ctrlp#init(ctrlp#dir#id(), { 'dir': <q-args> })

com! -n=? -com=buffer CtrlPBufTag
	\ cal ctrlp#init(ctrlp#buffertag#cmd(0, <q-args>))

com! -bar CtrlPBufTagAll cal ctrlp#init(ctrlp#buffertag#cmd(1))
com! -bar CtrlPRTS       cal ctrlp#init(ctrlp#rtscript#id())
com! -bar CtrlPUndo      cal ctrlp#init(ctrlp#undo#id())

com! -n=? -com=buffer CtrlPLine
	\ cal ctrlp#init(ctrlp#line#cmd('buf', <q-args>))

com! -n=? -com=buffer CtrlPChange
	\ cal ctrlp#init(ctrlp#changes#cmd('fil', <q-args>))

com! -bar CtrlPChangeAll   cal ctrlp#init(ctrlp#changes#cmd(1))
com! -bar CtrlPMixed       cal ctrlp#init(ctrlp#mixed#id())
com! -bar CtrlPBookmarkDir cal ctrlp#init(ctrlp#bookmarkdir#id())

com! -n=? -com=dir -bang CtrlPBookmarkDirAdd
	\ cal ctrlp#call('ctrlp#bookmarkdir#add', '<bang>', <q-args>)

" vim:ts=2:sw=2:sts=2
