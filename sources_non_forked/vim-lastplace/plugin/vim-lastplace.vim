" ============================================================================
" File:        vim-lastplace.vim
" Description: Reopen files where you left off. Configurable.
" Author:      Gregory L. Dietsche <vim@gregd.org>
" Licence:     MIT
" Website:     https://www.gregd.org/
" Version:     3.1.1
" ============================================================================

if exists("b:loaded_lastplace_plugin") || &cp
	finish
endif
let b:loaded_lastplace_plugin = 1

scriptencoding utf-8

if !exists('g:lastplace_ignore')
	let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
endif

if !exists('g:lastplace_open_folds')
	let g:lastplace_open_folds = 1
endif

if !exists('g:lastplace_ignore_buftype')
	let g:lastplace_ignore_buftype = "quickfix,nofile,help"
endif

fu! s:lastplace()
	if index(split(g:lastplace_ignore_buftype, ","), &buftype) != -1 
		return
   	endif

	if index(split(g:lastplace_ignore, ","), &filetype) != -1
		return
	endif

	try
		"if the file does not exist on disk (a new, unsaved file) then do nothing
		if empty(glob(@%))
			return
		endif
	catch
		return
	endtry

	if line("'\"") > 0 && line("'\"") <= line("$")
		"if the last edit position is set and is less than the
		"number of lines in this buffer.

		if line("w$") == line("$")
			"if the last line in the current buffer is
			"also the last line visible in this window
			execute "normal! g`\""

		elseif line("$") - line("'\"") > ((line("w$") - line("w0")) / 2) - 1
			"if we're not at the bottom of the file, center the
			"cursor on the screen after we make the jump
			execute "normal! g`\"zz"

		else
			"otherwise, show as much context as we can by jumping
			"to the end of the file and then to the mark. If we
			"pressed zz here, there would be blank lines at the
			"bottom of the screen. We intentionally leave the
			"last line blank by pressing <c-e> so the user has a
			"clue that they are near the end of the file.
			execute "normal! \G'\"\<c-e>"
		endif
	endif
	if foldclosed(".") != -1 && g:lastplace_open_folds
		"if we're in a fold, make the current line visible and recenter screen
		execute "normal! zvzz"
	endif
endf

augroup lastplace_plugin
	autocmd!
	autocmd BufWinEnter * call s:lastplace()
augroup END
