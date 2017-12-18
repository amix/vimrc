" File:         pum_snippet.vim
" Author:       Michael Sanders (msanders42 [at] gmail [dot] com)
" Last Updated: June 12, 2009
" Description:  Uses snipMate to jump through function or method objc
"               parameters when autocompleting. Used in cocoacomplete.vim.

" This function is invoked whenever pum_snippet is to be used; the keys are
" only mapped when used as the trigger, and then immediately unmapped to avoid
" breaking abbreviations, as well as other things.
fun! objc#pum_snippet#Map()
	ino <silent> <buffer> <space> <c-r>=objc#pum_snippet#Trigger(' ')<cr>
	if !exists('g:SuperTabMappingForward') " Only map tab if not using supertab.
	\ || (g:SuperTabMappingForward != '<tab>' && g:SuperTabMappingForward != '<tab>')
		ino <silent> <buffer> <tab> <c-r>=objc#pum_snippet#Trigger("\t")<cr>
	endif
	ino <silent> <buffer> <return> <c-r>=objc#pum_snippet#Trigger("\n")<cr>
	let s:start = col('.')
	" Completion menu can only be detected when the popup menu is visible, so
	" 'menuone' needs to be temporarily set:
	let s:cot = &cot
	set cot+=menuone
endf

fun! objc#pum_snippet#Unmap()
	call s:UnmapKey('<space>')
	call s:UnmapKey('<tab>')
	call s:UnmapKey('<return>')
endf

fun s:UnmapKey(key)
	if maparg(a:key, 'i') =~? '^<C-R>=objc#pum_snippet#Trigger('
		sil exe 'iunmap <buffer> '.a:key
	endif
endf

fun! objc#pum_snippet#Trigger(key)
	call objc#pum_snippet#Unmap()
	if pumvisible()
		let line = getline('.')
		let col = col('.')
		let word = matchstr(line, '\%'.s:start.'c\k\+(.\{-})\%'.col.'c')
		if word != ''
			let ConvertWord = function('s:ConvertFunction')
		elseif match(line, '\%'.s:start.'c\k\+[^()]*:[^()]*\%'.col.'c') != -1
			let word = matchstr(line, '\%'.s:start.'c\k\+[^()]*\%'.col.'c')
			let ConvertWord = function('s:ConvertMethod')
		endif
		if word != ''
			call s:ResetOptions()
			let col -= len(word)
			sil exe 's/\V'.escape(word, '\/').'\%#//'
			return snipMate#expandSnip(ConvertWord(word), col)
		endif
	endif
	call s:ResetOptions()
	return a:key
endf

fun s:ResetOptions()
	let &cot = s:cot
	unl s:start s:cot
endf

fun s:ConvertFunction(function)
	let name = matchstr(a:function, '^\k\+')
	let params = matchstr(a:function, '^\k\+(\zs.*')
	let snippet = name.'('.substitute(params, '\v(.{-1})(, |\))', '${0:\1}\2', 'g').'${0}'
	return s:OrderSnippet(snippet)
endf

fun s:ConvertMethod(method)
	if stridx(a:method, ':') == -1 | return a:method | endif
	let snippet = substitute(a:method, '\v\k+:\zs.{-}\ze(\s*\k+:|$)', '${0:&}', 'g')
	return s:OrderSnippet(snippet)
endf

" Converts "${0} foo ${0} bar ..." to "${1} foo ${2} bar", etc.
fun s:OrderSnippet(snippet)
	let snippet = a:snippet
	let i = 1
	while stridx(snippet, '${0') != -1
		let snippet = substitute(snippet, '${0', '${'.i, '')
		let i += 1
	endw
	return snippet
endf
" vim:noet:sw=4:ts=4:ft=vim
