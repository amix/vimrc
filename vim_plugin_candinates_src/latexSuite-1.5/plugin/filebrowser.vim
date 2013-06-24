" filebrowser.vim: utility file for vim 6.2+
"
" Copyright: Srinath Avadhanula <srinath AT fastmail DOT fm>
" 	Parts of this file are taken from explorer.vim which is a plugin file
" 	distributed with vim under the Vim charityware license.
" License: distributed under the Vim charityware license.
"
" Settings:
" FB_CallBackFunction: the function name which gets called when the user
" 		presses <cr> on a file-name in the file browser.
" FB_AllowRegexp: A filename has to match this regexp to be displayed.
" FB_RejectRegexp: If a filename matches this regexp, then its not displayed.
" 		(Both these regexps are '' by default which means no filtering is
" 		done).

"======================================================================
" Globally visible functions (API)
"======================================================================
" FB_OpenFileBrowser: opens a new buffer and displays the file list {{{
" Description: 
function! FB_OpenFileBrowser(dir)
	if !isdirectory(a:dir)
		return
	endif
	if exists('s:FB_BufferNumber')
		if bufwinnr(s:FB_BufferNumber) != -1
			execute bufwinnr(s:FB_BufferNumber).' wincmd w'
			return
		endif
		execute 'aboveleft split #'.s:FB_BufferNumber
	else
		aboveleft split __Choose_File__
		let s:FB_BufferNumber = bufnr('%')
	endif
	call FB_DisplayFiles(a:dir)
endfunction " }}}
" FB_DisplayFiles: displays the files in a given directory {{{
" Description: 
" 	Call this function only when the cursor is in a temporary buffer
function! FB_DisplayFiles(dir)
	if !isdirectory(a:dir)
		return
	endif
	call s:FB_SetSilentSettings()
	" make this a "scratch" buffer
	call s:FB_SetScratchSettings()

	let allowRegexp = s:FB_GetVar('FB_AllowRegexp', '')
	let rejectRegexp = s:FB_GetVar('FB_RejectRegexp', '')

	" change to the directory to make processing simpler.
	execute "lcd ".a:dir
	" delete everything in the buffer.
	" IMPORTANT: we need to be in a scratch buffer
	0,$ d_

	let allFilenames = glob('*')
	let dispFiles = ""
	let subDirs = "../\n"

	let i = 1
	while 1
		let filename = s:FB_Strntok(allFilenames, "\n", i)
		if filename == ''
			break
		endif
		if isdirectory(filename)
			let subDirs = subDirs.filename."/\n"
		else
			if allowRegexp != '' && filename !~ allowRegexp
			elseif rejectRegexp != '' && filename =~ rejectRegexp
			else
				let dispFiles = dispFiles.filename."\n"
			endif
		endif
		let i = i + 1
	endwhile
	0put!=dispFiles
	0put!=subDirs
	" delte the last empty line resulting from the put
	$ d_

	call s:FB_SetHighlighting()
	call s:FB_DisplayHelp()
	call s:FB_SetMaps()

	" goto the first file/directory
	0
	call search('^"=', 'w')
	normal! j:<bs>

	set nomodified nomodifiable

	call s:FB_ResetSilentSettings()
endfunction " }}}
" FB_SetVar: sets script local variables from outside this script {{{
" Description: 
function! FB_SetVar(varname, value)
	let s:{a:varname} = a:value
endfunction " }}}

" FB_SetHighlighting: sets syntax highlighting for the buffer {{{
" Description:
" Origin: from explorer.vim in vim
function! <SID>FB_SetHighlighting()
	" Set up syntax highlighting
	" Something wrong with the evaluation of the conditional though...
	if has("syntax") && exists("g:syntax_on") && !has("syntax_items")
		syn match browseSynopsis    "^\"[ -].*"
		syn match browseDirectory   "[^\"].*/ "
		syn match browseDirectory   "[^\"].*/$"
		syn match browseCurDir      "^\"= .*$"
		syn match browseSortBy      "^\" Sorted by .*$"  contains=browseSuffixInfo
		syn match browseSuffixInfo  "(.*)$"  contained
		syn match browseFilter      "^\" Not Showing:.*$"
		syn match browseFiletime    "«\d\+$"

		"hi def link browseSynopsis    PreProc
		hi def link browseSynopsis    Special
		hi def link browseDirectory   Directory
		hi def link browseCurDir      Statement
		hi def link browseSortBy      String
		hi def link browseSuffixInfo  Type
		hi def link browseFilter      String
		hi def link browseFiletime    Ignore
		hi def link browseSuffixes    Type
	endif
endfunction " }}}
" FB_SetMaps: sets buffer local maps {{{
" Description: 
function! <SID>FB_SetMaps()
	nnoremap <buffer> <silent> q :bdelete<cr>
	nnoremap <buffer> <silent> C :call FB_DisplayFiles(getcwd())<CR>
	nnoremap <buffer> <silent> <esc> :bdelete<cr>
	nnoremap <buffer> <silent> <CR> :call <SID>FB_EditEntry()<CR>
	nnoremap <buffer> <silent> ? :call <SID>FB_ToggleHelp()<CR>

	" lock the user in this window
	nnoremap <buffer> <C-w> <nop>
endfunction " }}}
" FB_SetSilentSettings: some settings which make things silent {{{
" Description: 
" Origin: from explorer.vim distributed with vim.
function! <SID>FB_SetSilentSettings()
	let s:save_report=&report
	let s:save_showcmd = &sc
	set report=10000 noshowcmd
endfunction 
" FB_ResetSilentSettings: reset settings set by FB_SetSilentSettings
" Description: 
function! <SID>FB_ResetSilentSettings()
	let &report=s:save_report
	let &showcmd = s:save_showcmd
endfunction " }}}
" FB_SetScratchSettings: makes the present buffer a scratch buffer {{{
" Description: 
function! <SID>FB_SetScratchSettings()
	" Turn off the swapfile, set the buffer type so that it won't get
	" written, and so that it will get deleted when it gets hidden.
	setlocal noreadonly modifiable
	setlocal noswapfile
	setlocal buftype=nowrite
	setlocal bufhidden=delete
	" Don't wrap around long lines
	setlocal nowrap
endfunction 

" }}}
" FB_ToggleHelp: toggles verbosity of help {{{
" Description: 
function! <SID>FB_ToggleHelp()
	let s:FB_VerboseHelp = 1 - s:FB_GetVar('FB_VerboseHelp', 0)

	call FB_DisplayFiles('.')
endfunction " }}}
" FB_DisplayHelp: displays a helpful header {{{
" Description: 
function! <SID>FB_DisplayHelp()
	let verboseHelp = s:FB_GetVar('FB_VerboseHelp', 0)
	if verboseHelp
		let txt = 
			\  "\" <cr>: on file, choose the file and quit\n"
			\ ."\"       on dir, enter directory\n"
			\ ."\" q/<esc>: quit without choosing\n"
			\ ."\" C: change directory to getcwd()\n"
			\ ."\" ?: toggle help verbosity\n"
			\ ."\"= ".getcwd()
	else
		let txt = "\" ?: toggle help verbosity\n"
			\ ."\"= ".getcwd()
	endif
	0put!=txt
endfunction " }}}

" Handles various actions in the file-browser
" FB_EditEntry: handles the user pressing <enter> on a line {{{
" Description: 
function! <SID>FB_EditEntry()
	let line = getline('.')

	if isdirectory(line)
		call FB_DisplayFiles(line)
	endif

	" If the user has a call back function defined on choosing a file, handle
	" it.
	let cbf = s:FB_GetVar('FB_CallBackFunction', '')
	if cbf != '' && line !~ '^" ' && filereadable(line) 
		let fname = fnamemodify(line, ':p')
		bdelete

		let arguments = s:FB_GetVar('FB_CallBackFunctionArgs', '')
		if arguments != ''
			let arguments = ','.arguments
		endif
		call Tex_Debug('arguments = '.arguments, 'fb')
		call Tex_Debug("call ".cbf."('".fname."'".arguments.')', 'fb')
		exec "call ".cbf."('".fname."'".arguments.')'
	endif
endfunction " }}}

"  FB_Strntok (string, tok, n) {{{
" extract the n^th token from s seperated by tok.
" example: FB_Strntok('1,23,3', ',', 2) = 23
fun! <SID>FB_Strntok(s, tok, n)
	return matchstr( a:s.a:tok[0], '\v(\zs([^'.a:tok.']*)\ze['.a:tok.']){'.a:n.'}')
endfun " }}}
" FB_GetVar: gets the most local value of a variable {{{
function! <SID>FB_GetVar(name, default)
	if exists('s:'.a:name)
		return s:{a:name}
	elseif exists('w:'.a:name)
		return w:{a:name}
	elseif exists('b:'.a:name)
		return b:{a:name}
	elseif exists('g:'.a:name)
		return g:{a:name}
	else
		return a:default
	endif
endfunction

" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4:nowrap
