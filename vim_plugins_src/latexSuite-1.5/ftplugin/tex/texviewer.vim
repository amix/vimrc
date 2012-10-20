" ============================================================================
" 	     File: texviewer.vim
"      Author: Mikolaj Machowski
"     Created: Sun Jan 26 06:00 PM 2003
" Description: make a viewer for various purposes: \cite{, \ref{
"     License: Vim Charityware License
"              Part of vim-latexSuite: http://vim-latex.sourceforge.net
"         CVS: $Id: texviewer.vim,v 1.32 2003/09/02 07:15:30 srinathava Exp $
" ============================================================================

if exists("g:Tex_Completion")
	call Tex_SetTexViewerMaps()
	finish
endif
let g:Tex_Completion = 1

" Tex_SetTexViewerMaps: sets maps for this ftplugin {{{
function! Tex_SetTexViewerMaps()
	inoremap <silent> <Plug>Tex_Completion <Esc>:call Tex_Complete("default","text")<CR>
	if !hasmapto('<Plug>Tex_Completion', 'i')
		if has('gui_running')
			imap <buffer> <silent> <F9> <Plug>Tex_Completion
		else
			imap <buffer> <F9> <Plug>Tex_Completion
		endif
	endif
endfunction

" call this function the first time
call Tex_SetTexViewerMaps()
" }}}
command -nargs=1 TLook    call Tex_Complete(<q-args>, 'tex')
command -nargs=1 TLookAll call Tex_Complete(<q-args>, 'all')
command -nargs=1 TLookBib call Tex_Complete(<q-args>, 'bib')

" ==============================================================================
" Main completion function
" ==============================================================================
" Tex_Complete: main function {{{
" Description:
function! Tex_Complete(what, where)

	" Get info about current window and position of cursor in file
	let s:winnum = winnr()

	" Change to the directory of the file being edited before running all the
	" :grep commands. We will change back to the original directory after we
	" finish with the grep.
	let s:origdir = getcwd()
	cd %:p:h

	let s:pos = line('.').' | normal! '.virtcol('.').'|'

	unlet! s:type
	unlet! s:typeoption

	if a:where == "text"
		" What to do after <F9> depending on context
		let s:curfile = expand("%:p")
		let s:curline = strpart(getline('.'), col('.') - 40, 40)
		let s:prefix = matchstr(s:curline, '.*{\zs.\{-}$')
		" a command is of the type
		" \psfig[option=value]{figure=}
		" Thus
		" 	s:curline = '\psfig[option=value]{figure='
		" (with possibly some junk before \includegraphics)
		" from which we need to extract
		" 	s:type = 'psfig'
		" 	s:typeoption = '[option=value]'
		let pattern = '.*\\\(\w\{-}\)\(\[.\{-}\]\)\?{\(\S\+\)\?$'
		if s:curline =~ pattern
			let s:type = substitute(s:curline, pattern, '\1', 'e')
			let s:typeoption = substitute(s:curline, pattern, '\2', 'e')
			call Tex_Debug('s:type = '.s:type.', typeoption = '.s:typeoption, 'view')
		endif

		if exists("s:type") && s:type =~ 'ref'
			call Tex_Debug("silent! grep! '".Tex_EscapeForGrep('\label{'.s:prefix)."' *.tex", 'view')
			exec "silent! grep! '".Tex_EscapeForGrep('\label{'.s:prefix)."' *.tex"
			redraw!
			call <SID>Tex_SetupCWindow()

		elseif exists("s:type") && s:type =~ 'cite'
			" grep! nothing % 
			" does _not_ clear the search history contrary to what the
			" help-docs say. This was expected. So use something improbable.
			" TODO: Is there a way to clear the search-history w/o making a
			"       useless, inefficient search?
			let s:prefix = matchstr(s:prefix, '\([^,]\+,\)\+\zs\([^,]\+\)\ze$')
			silent! grep! ____HIGHLY_IMPROBABLE___ %
			if g:Tex_RememberCiteSearch && exists('s:citeSearchHistory')
				call <SID>Tex_SetupCWindow(s:citeSearchHistory)
			else
				call Tex_Debug('calling Tex_GrepForBibItems', 'bib')
				call Tex_GrepForBibItems(s:prefix)
				redraw!
				call <SID>Tex_SetupCWindow()
			endif
			if g:Tex_RememberCiteSearch && &ft == 'qf'
				let _a = @a
				silent! normal! ggVG"ay
				let s:citeSearchHistory = @a
				let @a = _a
			endif

		elseif exists("s:type") && (s:type =~ 'includegraphics' || s:type == 'psfig') 
			call Tex_SetupFileCompletion(
				\ '', 
				\ '^\.\\|\.tex$\\|\.bib$\\|\.bbl$\\|\.zip$\\|\.gz$', 
				\ 'noext')
			
		elseif exists("s:type") && s:type == 'bibliography'
			call Tex_SetupFileCompletion(
				\ '\.b..$',
				\ '',
				\ 'noext')

		elseif exists("s:type") && s:type =~ 'include\(only\)\='
			call Tex_SetupFileCompletion(
				\ '\.t..$', 
				\ '',
				\ 'noext')

		elseif exists("s:type") && s:type == 'input'
			call Tex_SetupFileCompletion(
				\ '', 
				\ '',
				\ 'ext')

		elseif exists('s:type') && exists("g:Tex_completion_".s:type)
			call <SID>Tex_CompleteRefCiteCustom('plugin_'.s:type)

		else
			let s:word = matchstr(s:curline, '\zs\k\{-}$')
			if s:word == ''
				if col('.') == strlen(getline('.'))
					startinsert!
					return
				else
					normal! l
					startinsert
					return
				endif
			endif
			call Tex_Debug("silent! grep! '\\<".s:word."' *.tex", 'view')
			exe "silent! grep! '\\<".s:word."' *.tex"

			call <SID>Tex_SetupCWindow()
		endif
		
	elseif a:where == 'tex'
		" Process :TLook command
		exe "silent! grep! '".a:what."' *.tex"
		call <SID>Tex_SetupCWindow()

	elseif a:where == 'bib'
		" Process :TLookBib command
		exe "silent! grep! '".a:what."' *.bib"
		exe "silent! grepadd! '".a:what."' *.bbl"
		call <SID>Tex_SetupCWindow()

	elseif a:where == 'all'
		" Process :TLookAll command
		exe "silent! grep! '".a:what."' *"
		call <SID>Tex_SetupCWindow()
	endif

endfunction " }}}

" Tex_CompleteWord: inserts a word at the chosen location {{{
" Description: 
function! Tex_CompleteWord(completeword)
	exe s:pos

	" Complete word, check if add closing }
	exe 'normal! a'.a:completeword."\<Esc>"

	if getline('.')[col('.')-1] !~ '{' && getline('.')[col('.')] !~ '}'
		exe "normal! a}\<Esc>"
	endif
	
	" Return to Insert mode
	if col('.') == strlen(getline('.'))
		startinsert!
	else
		normal! l
		startinsert
	endif
endfunction " }}}

" ==============================================================================
" File name completion helper functons
" ============================================================================== 
" Tex_SetupFileCompletion:  {{{
" Description: 
function! Tex_SetupFileCompletion(accept, reject, ext)
	call FB_SetVar('FB_AllowRegexp', a:accept)
	call FB_SetVar('FB_RejectRegexp', a:reject)
	call FB_SetVar('FB_CallBackFunction', 'Tex_CompleteFileName')
	call FB_SetVar('FB_CallBackFunctionArgs', '"'.a:ext.'"')

	call FB_OpenFileBrowser('.')
endfunction " }}}
" Tex_CompleteFileName:  {{{
" Description: 
function! Tex_CompleteFileName(filename, ext)
	call Tex_Debug('getting filename = '.a:filename, 'view')

	let completeword = Tex_RelPath(a:filename, expand('%:p'))
	if a:ext == 'noext'
		let completeword = fnamemodify(completeword, ':r')
	endif

	call Tex_CompleteWord(completeword)
endfunction " }}}
" Tex_Common: common part of strings {{{
function! s:Tex_Common(path1, path2)
	" Assume the caller handles 'ignorecase'
	if a:path1 == a:path2
		return a:path1
	endif
	let n = 0
	while a:path1[n] == a:path2[n]
		let n = n+1
	endwhile
	return strpart(a:path1, 0, n)
endfunction " }}}
" Tex_NormalizePath:  {{{
" Description: 
function! Tex_NormalizePath(path)
	let retpath = a:path
	if has("win32") || has("win16") || has("dos32") || has("dos16")
		let retpath = substitute(retpath, '\\', '/', 'ge')
	endif
	if isdirectory(retpath) && retpath !~ '/$'
		let retpath = retpath.'/'
	endif
	return retpath
endfunction " }}}
" Tex_RelPath: ultimate file name {{{
function! Tex_RelPath(explfilename,texfilename)
	let path1 = Tex_NormalizePath(a:explfilename)
	let path2 = Tex_NormalizePath(a:texfilename)

	let n = matchend(<SID>Tex_Common(path1, path2), '.*/')
	let path1 = strpart(path1, n)
	let path2 = strpart(path2, n)
	if path2 !~ '/'
		let subrelpath = ''
	else
		let subrelpath = substitute(path2, '[^/]\{-}/', '../', 'ge')
		let subrelpath = substitute(subrelpath, '[^/]*$', '', 'ge')
	endif
	let relpath = subrelpath.path1
	return escape(Tex_NormalizePath(relpath), ' ')
endfunction " }}}

" ==============================================================================
" Ref/Cite completion helper functions
" ==============================================================================
" Tex_SetupCWindow: set maps and local settings for cwindow {{{
" Description: Set local maps jkJKq<cr> for cwindow. Also size and basic
" settings
"
function! s:Tex_SetupCWindow(...)
	call Tex_Debug('+Tex_SetupCWindow', 'view')
	cclose
	exe 'copen '. g:Tex_ViewerCwindowHeight
	" If called with an argument, it means we want to re-use some search
	" history from last time. Therefore, just paste it here and proceed.
	if a:0 == 1
		set modifiable
		% d _
		silent! 0put!=a:1
		$ d _
	endif
	setlocal nonumber
	setlocal nowrap

	let s:scrollOffVal = &scrolloff
	call <SID>Tex_SyncPreviewWindow()

	" If everything went well, then we should be situated in the quickfix
	" window. If there were problems, (no matches etc), then we will not be.
	" Therefore return.
	if &ft != 'qf'
		call Tex_Debug('not in quickfix window, quitting', 'view')
		return
	endif

    nnoremap <buffer> <silent> j j:call <SID>Tex_SyncPreviewWindow()<CR>
    nnoremap <buffer> <silent> k k:call <SID>Tex_SyncPreviewWindow()<CR>
    nnoremap <buffer> <silent> <up> <up>:call <SID>Tex_SyncPreviewWindow()<CR>
    nnoremap <buffer> <silent> <down> <down>:call <SID>Tex_SyncPreviewWindow()<CR>

	" Change behaviour of <cr> only for 'ref' and 'cite' context. 
	if exists("s:type") && s:type =~ 'ref\|cite'
		exec 'nnoremap <buffer> <silent> <cr> '
			\ .':set scrolloff='.s:scrollOffVal.'<CR>'
			\ .':cd '.s:origdir.'<CR>'
			\ .':silent! call <SID>Tex_CompleteRefCiteCustom("'.s:type.'")<CR>'

	else
		" In other contexts jump to place described in cwindow and close small
		" windows
		exec 'nnoremap <buffer> <silent> <cr> '
			\ .':set scrolloff='.s:scrollOffVal.'<CR>'
			\ .':cd '.s:origdir.'<CR>'
			\ .':call <SID>Tex_GoToLocation()<cr>'

	endif

	" Scroll the preview window while in the quickfix window
	nnoremap <buffer> <silent> J :wincmd j<cr><c-e>:wincmd k<cr>
	nnoremap <buffer> <silent> K :wincmd j<cr><c-y>:wincmd k<cr>

	" Exit the quickfix window without doing anything.
	exe 'nnoremap <buffer> <silent> q '
		\ .':set scrolloff='.s:scrollOffVal.'<CR>'
		\ .':cd '.s:origdir.'<CR>'
		\ .':call Tex_CloseSmallWindows()<CR>'

endfunction " }}}
" Tex_CompleteRefCiteCustom: complete/insert name for current item {{{
" Description: handle completion of items depending on current context
"
function! s:Tex_CompleteRefCiteCustom(type)

	if a:type =~ 'cite'
		if getline('.') =~ '\\bibitem{'
			let bibkey = matchstr(getline('.'), '\\bibitem{\zs.\{-}\ze}')
		else
			let bibkey = matchstr(getline('.'), '{\zs.\{-}\ze,')
		endif
		let completeword = strpart(bibkey, strlen(s:prefix))

	elseif a:type =~ 'ref'
		let label = matchstr(getline('.'), '\\label{\zs.\{-}\ze}')
		let completeword = strpart(label, strlen(s:prefix))

	elseif a:type =~ '^plugin_'
		let type = substitute(a:type, '^plugin_', '', '')
		let completeword = <SID>Tex_DoCompletion(type)
		
	endif

	call Tex_CloseSmallWindows()
	call Tex_CompleteWord(completeword)
endfunction " }}}
" Tex_SyncPreviewWindow: synchronize quickfix and preview window {{{
" Description: Usually quickfix engine takes care about most of these things
" but we discard it for better control of events.
"
function! s:Tex_SyncPreviewWindow()
	 call Tex_Debug('+Tex_SyncPreviewWindow', 'view')

	let viewfile = matchstr(getline('.'), '^\f*\ze|\d')
	let viewline = matchstr(getline('.'), '|\zs\d\+\ze|')

	" Hilight current line in cwindow
	" Normally hightlighting is done with quickfix engine but we use something
	" different and have to do it separately
	syntax clear
	runtime syntax/qf.vim
	exe 'syn match vTodo /\%'. line('.') .'l.*/'
	hi link vTodo Todo

	" Close preview window and open it again in new place
    pclose
	exe 'silent! bot pedit +'.viewline.' '.viewfile

	" Vanilla 6.1 has bug. This additional setting of cwindow height prevents
	" resizing of this window
	exe g:Tex_ViewerCwindowHeight.' wincmd _'
	
	" Handle situation if there is no item beginning with s:prefix.
	" Unfortunately, because we know it late we have to close everything and
	" return as in complete process 
	if v:errmsg =~ 'E32\>'
		exe s:winnum.' wincmd w'
		pclose!
		cclose
		if exists("s:prefix")
			echomsg 'No bibkey, label or word beginning with "'.s:prefix.'"'
		endif
		if col('.') == strlen(getline('.'))
			startinsert!
		else
			normal! l
			startinsert
		endif
		let v:errmsg = ''
		call Tex_Debug('Tex_SyncPreviewWindow: got error E32, no matches found, quitting', 'view')
		return 0
	endif

	" Move to preview window. Really is it under cwindow?
	wincmd j

	" Settings of preview window
	exe g:Tex_ViewerPreviewHeight.' wincmd _'
	setlocal foldlevel=10

	if exists('s:type') && s:type =~ 'cite'
		" In cite context place bibkey at the top of preview window.
		setlocal scrolloff=0
		normal! zt
	else
		" In other contexts in the middle. Highlight this line?
		setlocal scrolloff=100
		normal! z.
	endif

	" Return to cwindow
	wincmd p

endfunction " }}}
" Tex_CloseSmallWindows: {{{
" Description:
"
function! Tex_CloseSmallWindows()
	exe s:winnum.' wincmd w'
	pclose!
	cclose
	exe s:pos
endfunction " }}}

" Tex_GoToLocation: Go to chosen location {{{
" Description: Get number of current line and go to this number
"
function! s:Tex_GoToLocation()
	pclose!
	let errmsg = v:errmsg
	let v:errmsg = ''
	exe 'silent! cc ' . line('.')
	" If the current buffer is modified, then split
	if v:errmsg =~ '^E37:'
		split
		exe 'silent! cc ' . line('.')
	endif
	cclose
	let v:errmsg = errmsg
endfunction " }}}

" ==============================================================================
" Bibliography specific functions
" ============================================================================== 
" Tex_GrepForBibItems: grep main filename for bib items {{{
" Description: 
function! Tex_GrepForBibItems(prefix)
	let mainfname = Tex_GetMainFileName(':p:r')

	let toquit = 0
	if bufnr('%') != bufnr(mainfname)
		exec 'split '.mainfname
		let toquit = 1
	endif

	let _path = &path
	let _suffixesadd = &suffixesadd

	let &path = '.,'.g:Tex_BIBINPUTS
	let &suffixesadd = '.tex'

	let pos = line('.').'| normal! '.virtcol('.').'|'
	let foundCiteFile = Tex_ScanFileForCite(a:prefix)
	exec pos

	let &path = _path
	let &suffixesadd = _suffixesadd

	if foundCiteFile
		if toquit
			q
		endif
		return
	endif
endfunction " }}}
" Tex_ScanFileForCite: search for \bibitem's in .bib or .bbl or tex files {{{
" Description: 
" Search for bibliographic entries in the presently edited file in the
" following manner:
" 1. First see if the file has a \bibliography command.
"    If YES:
"    	1. If a .bib file corresponding to the \bibliography command can be
"    	   found, then search for '@.*'.a:prefix inside it.
"    	2. Otherwise, if a .bbl file corresponding to the \bibliography command
"    	   can be found, then search for '\bibitem'.a:prefix inside it.
" 2. Next see if the file has a \thebibliography environment
"    If YES:
"    	1. Search for '\bibitem'.a:prefix in this file.
"
" If neither a \bibliography or \begin{thebibliography} are found, then repeat
" steps 1 and 2 for every file \input'ed into this file. Abort any searching
" as soon as the first \bibliography or \begin{thebibliography} is found.
function! Tex_ScanFileForCite(prefix)
	call Tex_Debug('searching for bibkeys in '.bufname('%').' (buffer #'.bufnr('%').')', 'bib')
	let presBufNum = bufnr('%')

	let foundCiteFile = 0
	" First find out if this file has a \bibliography command in it. If so,
	" assume that this is the only file in the project which defines a
	" bibliography.
	if search('\\bibliography{', 'w')
		call Tex_Debug('found bibliography command in '.bufname('%'), 'bib')
		" convey that we have found a bibliography command. we do not need to
		" proceed any further.
		let foundCiteFile = 1

		" extract the bibliography filenames from the command.
		let bibnames = matchstr(getline('.'), '\\bibliography{\zs.\{-}\ze}')
		let bibnames = substitute(bibnames, '\s', '', 'g')

		call Tex_Debug('trying to search through ['.bibnames.']', 'bib')
		
		let i = 1
		while Tex_Strntok(bibnames, ',', i) != ''
			" first try to find if a .bib file exists. If so do not search in
			" the corresponding .bbl file. (because the .bbl file will most
			" probly be generated automatically from the .bib file with
			" bibtex).
			
			" split a new window so we do not screw with the current buffer.
			split
			let thisbufnum = bufnr('%')
			call Tex_Debug('silent! find '.Tex_Strntok(bibnames, ',', i).'.bib', 'bib')
			exec 'silent! find '.Tex_Strntok(bibnames, ',', i).'.bib'
			if bufnr('%') != thisbufnum
				call Tex_Debug('finding .bib file ['.bufname('%').']', 'bib')
				lcd %:p:h
				" use the appropriate syntax for the .bib file.
				exec "silent! grepadd '".Tex_EscapeForGrep('@.*{'.a:prefix)."' %"
			else
				let thisbufnum = bufnr('%')
				exec 'silent! find '.Tex_Strntok(bibnames, ',', i).'.bbl'
				call Tex_Debug('now in bufnum#'.bufnr('%'), 'bib')
				if bufnr('%') != thisbufnum
					call Tex_Debug('finding .bbl file ['.bufname('.').']', 'bib')
					lcd %:p:h
					exec "silent! grepadd '".Tex_EscapeForGrep('\bibitem{'.a:prefix)."' %"
				endif
			endif
			" close the newly opened window
			q

			let i = i + 1
		endwhile

		if foundCiteFile
			return 1
		endif
	endif

	" If we have a thebibliography environment, then again assume that this is
	" the only file which defines the bib-keys. Aand convey this information
	" upwards by returning 1.
	if search('^\s*\\begin{thebibliography}', 'w')
		call Tex_Debug('got a thebibliography environment in '.bufname('%'), 'bib')
		
		let foundCiteFile = 1

		split
		lcd %:p:h
		exec "silent! grepadd ".Tex_EscapeForGrep('\bibitem{'.a:prefix)."' %")
		q
		
		return 1
	endif

	" If we have not found any \bibliography or \thebibliography environment
	" in this file, search for these environments in all the files which this
	" file includes.
	exec 0
	let wrap = 'w'
	while search('^\s*\\\(input\|include\)', wrap)
		let wrap = 'W'

		let filename = matchstr(getline('.'), '\\\(input\|include\){\zs.\{-}\ze}')

		split
		let thisbufnum = bufnr('%')

		exec 'silent! find '.filename
		if bufnr('%') != thisbufnum
			" DANGER! recursive call.
			call Tex_Debug('scanning recursively in ['.bufname('%').']', 'bib')
			let foundCiteFile = Tex_ScanFileForCite(a:prefix)
		endif
		q

		if foundCiteFile
			return 1
		endif
	endwhile

	return 0
endfunction " }}}

" ==============================================================================
" Custom Completion help functions/settings
" ==============================================================================
" Tex_completion_{var}: similar variables can be set in package files {{{
let g:Tex_completion_bibliographystyle = 'abbr,alpha,plain,unsrt'
let g:Tex_completion_addtocontents = 'lof}{,lot}{,toc}{'
let g:Tex_completion_addcontentsline = 'lof}{figure}{,lot}{table}{,toc}{chapter}{,toc}{part}{,'.
									\ 'toc}{section}{,toc}{subsection}{,toc}{paragraph}{,'.
									\ 'toc}{subparagraph}{'
" }}}
" Tex_PromptForCompletion: prompts for a completion {{{
" Description: 
function! s:Tex_PromptForCompletion(texcommand,ask)

	let common_completion_prompt = 
				\ Tex_CreatePrompt(g:Tex_completion_{a:texcommand}, 2, ',') . "\n" .
				\ 'Enter number or completion: '

	let inp = input(a:ask."\n".common_completion_prompt)
	if inp =~ '^[0-9]\+$'
		let completion = Tex_Strntok(g:Tex_completion_{a:texcommand}, ',', inp)
	else
		let completion = inp
	endif

	return completion
endfunction " }}}
" Tex_DoCompletion: fast insertion of completion {{{
" Description:
"
function! s:Tex_DoCompletion(texcommand)
	let completion = <SID>Tex_PromptForCompletion(a:texcommand, 'Choose a completion to insert: ')
	if completion != ''
		return completion
	else
		return ''
	endif
endfunction " }}}

com! -nargs=0 TClearCiteHist unlet! s:citeSearchHistory

" this statement has to be at the end.
let s:doneOnce = 1

" vim:fdm=marker:nowrap:noet:ff=unix:ts=4:sw=4
