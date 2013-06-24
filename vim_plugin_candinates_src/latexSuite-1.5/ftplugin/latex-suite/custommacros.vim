"=============================================================================
" 	     File: custommacros.vim
"      Author: Mikolaj Machowski
" 	      CVS: $Id: custommacros.vim,v 1.14.4.1 2003/11/07 06:37:12 srinathava Exp $
" 
"  Description: functions for processing custom macros in the
"               latex-suite/macros directory
"=============================================================================

let s:path = expand('<sfile>:p:h')

" Tex_SetCustomMacrosMenu: sets up the menu for Macros {{{
function! Tex_SetCustomMacrosMenu()
	let flist = glob(s:path."/macros/*")
	exe 'amenu '.g:Tex_MacrosMenuLocation.'&New :call Tex_NewMacro()<CR>'
	exe 'amenu '.g:Tex_MacrosMenuLocation.'&Redraw :call Tex_RedrawMacro()<CR>'

	let i = 1
	while 1
		let fname = Tex_Strntok(flist, "\n", i)
		if fname == ''
			break
		endif
		let fnameshort = fnamemodify(fname, ':p:t:r')
		exe "amenu ".g:Tex_MacrosMenuLocation."&Delete.&".i.":<tab>".fnameshort." :call Tex_DeleteMacro('".fnameshort."')<CR>"
		exe "amenu ".g:Tex_MacrosMenuLocation."&Edit.&".i.":<tab>".fnameshort."   :call Tex_EditMacro('".fnameshort."')<CR>"
		exe "imenu ".g:Tex_MacrosMenuLocation."&".i.":<tab>".fnameshort." <C-r>=Tex_ReadMacro('".fnameshort."')<CR>"
		exe "nmenu ".g:Tex_MacrosMenuLocation."&".i.":<tab>".fnameshort." i<C-r>=Tex_ReadMacro('".fnameshort."')<CR>"
		let i = i + 1
	endwhile
endfunction 

if g:Tex_Menus
	call Tex_SetCustomMacrosMenu()
endif

" }}}
" Tex_NewMacro: opens new file in macros directory {{{
function! Tex_NewMacro()
	exe "cd ".s:path."/macros"
	new
	set filetype=tex
endfunction

" }}}
" Tex_RedrawMacro: refreshes macro menu {{{
function! Tex_RedrawMacro()
	aunmenu TeX-Suite.Macros
	call Tex_SetCustomMacrosMenu()
endfunction

" }}}
" Tex_ChooseMacro: choose a macro file {{{
" Description: 
function! Tex_ChooseMacro(ask)
	let pwd = getcwd()
	exe 'cd '.s:path.'/macros'
	let filename = Tex_ChooseFromPrompt(
				\ a:ask."\n" . 
				\ Tex_CreatePrompt(glob('*'), 2, "\n") .
				\ "\nEnter number or filename :",
				\ glob('*'), "\n")
	exe 'cd '.pwd

	return filename
endfunction " }}}
" Tex_DeleteMacro: deletes macro file {{{
function! Tex_DeleteMacro(...)
	if a:0 > 0
		let filename = a:1
	else
		let pwd = getcwd()
		exe 'cd '.s:path.'/macros'
		let filename = Tex_ChooseMacro('Choose a macro file for deletion :')
		exe 'cd '.pwd
	endif

	let ch = confirm('Really delete '.filename.' ?', 
		\"Yes\nNo", 2)
	if ch == 1
		call delete(s:path.'/macros/'.filename)
	endif
	call Tex_RedrawMacro()
endfunction

" }}}
" Tex_EditMacro: edits macro file {{{
function! Tex_EditMacro(...)
	if a:0 > 0
		let filename = a:1
	else
		let pwd = getcwd()
		exe 'cd '.s:path.'/macros'
		let filename = Tex_ChooseMacro('Choose a macro file for insertion:')
		exe 'cd '.pwd
	endif

	exe "split ".s:path."/macros/".filename
	exe "lcd ".s:path."/macros/"
	set filetype=tex
endfunction

" }}}
" Tex_ReadMacro: reads in a macro from a macro file.  {{{
"            allowing for placement via placeholders.
function! Tex_ReadMacro(...)

	if a:0 > 0
		let filename = a:1
	else
		let pwd = getcwd()
		exe 'cd '.s:path.'/macros'
		let filename = Tex_ChooseMacro('Choose a macro file for insertion:')
		exe 'cd '.pwd

		if filename == ''
			return ''
		endif
	endif

	let fname = s:path.'/macros/'.filename

	let markerString = '<---- Latex Suite End Macro ---->'
	let _a = @a
	let position = line('.').' | normal! '.virtcol('.').'|'
	silent! call append(line('.'), markerString)
	silent! exec "read ".fname
	silent! exec "normal! V/^".markerString."$/-1\<CR>\"ax"
	" This is kind of tricky: At this stage, we are one line after the one we
	" started from with the marker text on it. We need to
	" 1. remove the marker and the line.
	" 2. get focus to the previous line.
	" 3. not remove anything from the previous line.
	silent! exec "normal! $v0k$\"_x"

	call Tex_CleanSearchHistory()

	let @a = substitute(@a, '['."\n\r\t ".']*$', '', '')
	let textWithMovement = IMAP_PutTextWithMovement(@a)
	let @a = _a

	return textWithMovement

endfunction

" }}}
" commands for macros {{{
com! -nargs=? TMacro          :let s:retVal = Tex_ReadMacro(<f-args>) <bar> exec "normal! i\<C-r>=s:retVal<CR>\<right>" <bar> startinsert
com! -nargs=0 TMacroNew       :call Tex_NewMacro()
com! -nargs=? TMacroEdit      :call Tex_EditMacro(<f-args>)
com! -nargs=? TMacroDelete    :call Tex_DeleteMacro(<f-args>)

" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
