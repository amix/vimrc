" File:         cocoacomplete.vim (part of the cocoa.vim plugin)
" Author:       Michael Sanders (msanders42 [at] gmail [dot] com)
" Last Updated: June 30, 2009
" Description:  An omni-completion plugin for Cocoa/Objective-C.

let s:lib_dir = fnameescape(expand('<sfile>:p:h:h:h').'/lib/')
let s:cocoa_indexes = s:lib_dir.'cocoa_indexes/'

if !isdirectory(s:cocoa_indexes)
	echom 'Error in cocoacomplete.vim: could not find ~/.vim/lib/cocoa_indexes directory'
endif

fun! objc#cocoacomplete#Complete(findstart, base)
	if a:findstart
		" Column where completion starts:
		return match(getline('.'), '\k\+\%'.col('.').'c')
	else
		let matches = []
		let complete_type = s:GetCompleteType(line('.'), col('.') - 1)

		if complete_type == 'methods'
			call s:Complete(a:base, ['alloc', 'init', 'retain', 'release',
			                       \ 'autorelease', 'retainCount',
			                       \ 'description', 'class', 'superclass',
			                       \ 'self', 'zone', 'isProxy', 'hash'])
			let obj_pos = s:GetObjPos(line('.'), col('.'))
			call extend(matches, s:CompleteMethod(line('.'), obj_pos, a:base))
		elseif complete_type == 'types' || complete_type == 'returntypes'
			let opt_types = complete_type == 'returntypes' ? ['IBAction'] : []
			call s:Complete(a:base, opt_types + ['void', 'id', 'BOOL', 'int',
			                                   \ 'double', 'float', 'char'])
			call extend(matches, s:CompleteCocoa(a:base, 'classes', 'types',
			                                           \ 'notifications'))
		elseif complete_type != ''
			if complete_type =~ 'function_params$'
				let complete_type = substitute(complete_type, 'function_params$', '', '')
				let functions = s:CompleteFunction(a:base)
			endif

			" Mimic vim's dot syntax for other complete types (see :h ft).
			let word = a:base == '' ? 'NS' : a:base
			let args = [word] + split(complete_type, '\.')
			call extend(matches, call('s:CompleteCocoa', args))

			" List functions after the other items in the menu.
			if exists('functions') | call extend(matches, functions) | endif
		endif
		return matches
	endif
endf

fun s:GetCompleteType(lnum, col)
	let scopelist = map(synstack(a:lnum, a:col), 'synIDattr(v:val, "name")')
	if empty(scopelist) | return 'types' | endif

	let current_scope = scopelist[-1]
	let beforeCursor = strpart(getline(a:lnum), 0, a:col)

	" Completing a function name:
	if getline(a:lnum) =~ '\%'.(a:col + 1).'c\s*('
		return 'functions'
	elseif current_scope == 'objcSuperclass'
		return 'classes'
	" Inside brackets "[ ... ]":
	elseif index(scopelist, 'objcMethodCall') != -1
		return beforeCursor =~ '\[\k*$' ? 'classes' : 'methods'
	" Inside parentheses "( ... )":
	elseif current_scope == 'cParen'
		" Inside parentheses for method definition:
		if beforeCursor =~ '^\s*[-+]\s*([^{;]*'
			return beforeCursor =~ '^\s*[-+]\s*([^)]*$' ? 'returntypes' : 'types'
		" Inside function, loop, or conditional:
		else
			return 'classes.types.constants.function_params'
		endif
	" Inside braces "{ ... }" or after equals "=":
	elseif current_scope == 'cBlock' || current_scope == 'objcAssign' || current_scope == ''
		let type = current_scope == 'cBlock' ? 'types.constants.' : ''
		let type = 'classes.'.type.'function_params'

		if beforeCursor =~ 'IBOutlet' | return 'classes' | endif
		return beforeCursor =~ '\v(^|[{};=\])]|return)\s*\k*$'? type : 'methods'
	" Directly inside "@implementation ... @end" or "@interface ... @end"
	elseif current_scope == 'objcImp' || current_scope  == 'objcHeader'
		" TODO: Complete delegate/subclass methods
	endif
	return ''
endf

" Adds item to the completion menu if they match the base.
fun s:Complete(base, items)
	for item in a:items
		if item =~ '^'.a:base | call complete_add(item) | endif
	endfor
endf

" Returns position of "foo" in "[foo bar]" or "[baz bar: [foo bar]]".
fun s:GetObjPos(lnum, col)
	let beforeCursor = strpart(getline(a:lnum), 0, a:col)
	return match(beforeCursor, '\v.*(^|[\[=;])\s*\[*\zs[A-Za-z0-9_@]+') + 1
endf

" Completes a method given the position of the object and the method
" being completed.
fun s:CompleteMethod(lnum, col, method)
	let class = s:GetCocoaClass(a:lnum, a:col)
	if class == ''
		let object = matchstr(getline(a:lnum), '\%'.a:col.'c\k\+')
		let class = s:GetDeclWord(object)
		if class == '' | return [] | endif
	endif
	let method = s:GetMethodName(a:lnum, a:col, a:method)
	let matches = split(system(s:lib_dir.'get_methods.sh '.class.
	                                   \ '|grep "^'.method.'"'), "\n")
	if exists('g:loaded_snips') " Use snipMate if it's installed
		call objc#pum_snippet#Map()
	else " Otherwise, only complete the method name.
		call map(matches, 'substitute(v:val, ''\v:\zs.{-}\ze(\w+:|$)'', " ", "g")')
	endif

	" If dealing with a partial method name, only complete past it. E.g., in
	" "[NSString stringWithCharacters:baz l|]" (where | is the cursor),
	" only return "length", not "stringWithCharacters:length:".
	if stridx(method, ':') != -1
		let method = substitute(method, a:method.'$', '\\\\zs&', '')
		call map(matches, 'matchstr(v:val, "'.method.'.*")')
	endif
	return matches
endf

" Returns the Cocoa class at a given position if it exists, or
" an empty string "" if it doesn't.
fun s:GetCocoaClass(lnum, col)
	let class = matchstr(getline(a:lnum), '\%'.a:col.'c[A-Za-z0-9_"@]\+')
	if class =~ '^@"' | return 'NSString' | endif " Treat @"..." as an NSString
	let v:errmsg = ''
	sil! hi cocoaClass
	if v:errmsg == '' && synIDattr(synID(a:lnum, a:col, 0), 'name') == 'cocoaClass'
		return class " If cocoaClass is defined, try using that.
	endif
	return system('grep ^'.class.' '.s:cocoa_indexes.'classes.txt') != ''
	              \ ? class : '' " Use grep as a fallback.
endf

" Returns the word before a variable declaration.
fun s:GetDeclWord(var)
	let startpos = [line('.'), col('.')]
	let line_found = searchdecl(a:var) != 0 ? 0 : line('.')
	call cursor(startpos)
	let matchstr = '\v(IBOutlet\s+)=\zs\k+\s*\ze\**\s*'

	" If the declaration was not found in the implementation file, check
	" the header.
	if !line_found && expand('%:e') == 'm'
		let header_path = expand('%:p:r').'.h'
		if filereadable(header_path)
			for line in readfile(header_path)
				if line =~ '^\s*\(IBOutlet\)\=\s*\k*\s*\ze\**\s*'.a:var.'\s*'
					return matchstr(line, matchstr)
				endif
			endfor
			return ''
		endif
	endif

	return matchstr(getline(line_found), matchstr.a:var)
endf

fun s:SearchList(list, regex)
	for line in a:list
		if line =~ a:regex
			return line
		endif
	endfor
	return ''
endf

" Returns the method name, ready to be searched by grep.
" The "base" word needs to be passed in separately, because
" Vim apparently removes it from the line during completions.
fun s:GetMethodName(lnum, col, base)
	let line = getline(a:lnum)
	let col = matchend(line, '\%'.a:col.'c\S\+\s\+') + 1 " Skip past class name.
	if line =~ '\%'.col.'c\k\+:'
		let base = a:base == '' ? '' : ' '.a:base
		let method = matchstr(line, '\%'.col.'c.\{-}\ze]').base
		return substitute(method, '\v\k+:\zs.{-}\ze(\s*\k+:|'.base.'$)', '[^:]*', 'g')
	else
		return a:base
	endif
endf

" Completes Cocoa functions, using snippets for the parameters if possible.
fun s:CompleteFunction(word)
	let files = s:cocoa_indexes.'functions.txt' " TODO: Add C functions.
	let matches = split(system('zgrep -h "^'.a:word.'" '.files), "\n")
	if exists('g:loaded_snips') " Use snipMate if it's installed
		call objc#pum_snippet#Map()
	else " Otherwise, just complete the function name
		call map(matches, "{'word':matchstr(v:val, '^\\k\\+'), 'abbr':v:val}")
	endif
	return matches
endf

" Completes word for Cocoa "classes", "types", "notifications", or "constants".
" (supplied as the optional parameters).
fun s:CompleteCocoa(word, file, ...)
	let files = ''
	for file in [a:file] + a:000
		let files .= ' '.s:cocoa_indexes.file.'.txt'
	endfor

	return split(system('grep -ho "^'.a:word.'[A-Za-z0-9_]*" '.files), "\n")
endf
" vim:noet:sw=4:ts=4:ft=vim
