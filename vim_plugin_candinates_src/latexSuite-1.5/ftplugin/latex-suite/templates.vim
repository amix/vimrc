"=============================================================================
" 	     File: templates.vim
"      Author: Gergely Kontra
"              (minor modifications by Srinath Avadhanula)
" 	  Version: 1.0 
"     Created: Tue Apr 23 05:00 PM 2002 PST
" 
"  Description: functions for handling templates in latex-suite/templates
"               directory.
"=============================================================================

let s:path = expand("<sfile>:p:h")

" SetTemplateMenu: sets up the menu for templates {{{
function! <SID>SetTemplateMenu()
	let flist = glob(s:path."/templates/*")
	let i = 1
	while 1
		let fname = Tex_Strntok(flist, "\n", i)
		if fname == ''
			break
		endif
		let fnameshort = fnamemodify(fname, ':p:t:r')
		if fnameshort == ''
			let i = i + 1
			continue
		endif
		exe "amenu ".g:Tex_TemplatesMenuLocation."&".i.":<Tab>".fnameshort." ".
			\":call <SID>ReadTemplate('".fnameshort."')<CR>".
			\":call <SID>ProcessTemplate()<CR>:0<CR>".
			\"i<C-r>=IMAP_Jumpfunc('', 1)<CR>"
		let i = i + 1
	endwhile
endfunction 

if g:Tex_Menus
	call <SID>SetTemplateMenu()
endif

" }}}
" ReadTemplate: reads in the template file from the template directory. {{{
function! <SID>ReadTemplate(...)
	if a:0 > 0
		let filename = a:1.'.*'
	else
		let pwd = getcwd()
		exe 'cd '.s:path.'/templates'
		let filename = 
					\ Tex_ChooseFromPrompt("Choose a template file:\n" . 
					\ Tex_CreatePrompt(glob('*'), 2, "\n") . 
					\ "\nEnter number or name of file :", 
					\ glob('*'), "\n")
		exe 'cd '.pwd
	endif

	let fname = glob(s:path."/templates/".filename)
	silent! exe "0read ".fname
	" The first line of the file contains the specifications of what the
	" placeholder characters and the other special characters are.
	let pattern = '\v(\S+)\t(\S+)\t(\S+)\t(\S+)'

	let s:phsTemp = substitute(getline(1), pattern, '\1', '')
	let s:pheTemp = substitute(getline(1), pattern, '\2', '')
	let s:exeTemp = substitute(getline(1), pattern, '\3', '')
	let s:comTemp = substitute(getline(1), pattern, '\4', '')

	call Tex_Debug('phs = '.s:phsTemp.', phe = '.s:pheTemp.', exe = '.s:exeTemp.', com = '.s:comTemp)

	" delete the first line into ze blackhole.
	0 d _

	call Tex_pack_updateall(1)
endfunction

" }}}
" ProcessTemplate: processes the special characters in template file. {{{
"                  This implementation follows from Gergely Kontra's
"                  mu-template.vim
"                  http://vim.sourceforge.net/scripts/script.php?script_id=222
function! <SID>ProcessTemplate()
	if exists('s:phsTemp') && s:phsTemp != ''

		exec 'silent! %s/^'.s:comTemp.'\(\_.\{-}\)'.s:comTemp.'$/\=<SID>Compute(submatch(1))/ge'
		exec 'silent! %s/'.s:exeTemp.'\(.\{-}\)'.s:exeTemp.'/\=<SID>Exec(submatch(1))/ge'
		exec 'silent! g/'.s:comTemp.s:comTemp.'/d'
		
		let phsUser = IMAP_GetPlaceHolderStart()
		let pheUser = IMAP_GetPlaceHolderEnd()

		exec 'silent! %s/'.s:phsTemp.'\(.\{-}\)'.s:pheTemp.'/'.phsUser.'\1'.pheUser.'/ge'

		" A function only puts one item into the search history...
		call Tex_CleanSearchHistory()
	endif
endfunction

function! <SID>Exec(what)
	exec 'return '.a:what
endfunction

" Back-Door to trojans !!!
function! <SID>Compute(what)
	exe a:what
	if exists('s:comTemp')
		return s:comTemp.s:comTemp
	else
		return ''
	endif
endfunction

" }}}

com! -nargs=? TTemplate :call <SID>ReadTemplate(<f-args>)
	\| :call <SID>ProcessTemplate()
	\| :0
	\| :exec "normal! i\<C-r>=IMAP_Jumpfunc('', 1)\<CR>"
	\| :startinsert

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
