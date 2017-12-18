" File:         objc#method_builder.vim (part of the cocoa.vim plugin)
" Author:       Michael Sanders (msanders42 [at] gmail [dot] com)
" Description:  Builds an empty implementation (*.m) file given a header (*.h)
"               file. When called with no arguments (simply ":BuildMethods"),
"               it looks for the corresponding header file of the current *.m
"               file (e.g. "foo.m" -> "foo.h").
" Last Updated: June 03, 2009
"       - make sure you're not in a comment

" TODO: Relative pathnames
fun objc#method_builder#Completion(ArgLead, CmdLine, CursorPos)
	let dir = stridx(a:ArgLead, '/') == -1 ? getcwd() : fnamemodify(a:ArgLead, ':h')
	let search = fnamemodify(a:ArgLead, ':t')
	let files = split(glob(dir.'/'.search.'*.h')
	            \ ."\n".glob(dir.'/'.search.'*/'), "\n")
	call map(files, 'fnameescape(fnamemodify(v:val, ":."))')
	return files
endf

fun s:Error(msg)
	echoh ErrorMsg | echo a:msg | echoh None
	return -1
endf

fun s:GetDeclarations(file)
	let header     = readfile(a:file)
	let template   = []
	let in_comment = 0
	let in_header  = 0
	let looking_for_semi = 0

	for line in header
		if in_comment
			if stridx(line, '*/') != -1 | let in_comment = 0 | endif
			continue " Ignore declarations inside multi-line comments
		elseif stridx(line, '/*') != -1
			let in_comment = 1 | continue
		endif

		if stridx(line, '@interface') != -1
			let in_header = 1
			let template += ['@implementation'.matchstr(line, '@interface\zs\s\+\w\+'), '']
			continue
		elseif in_header && stridx(line, '@end') != -1
			let in_header = 0
			call add(template, '@end')
			break " Only process one @interface at a time, for now.
		endif
		if !in_header | continue | endif

		let first_char = strpart(line, 0, 1)
		if first_char == '-' || first_char == '+' || looking_for_semi
			let semi_pos = stridx(line, ';')
			let looking_for_semi = semi_pos == -1
			if looking_for_semi
				call add(template, line)
			else
				call add(template, strpart(line, 0, semi_pos))
				let template += ['{', "\t", '}', '']
			endif
		endif
	endfor
	return template
endf

fun objc#method_builder#Build(header)
	let headerfile = a:header == '' ? expand('%:p:r').'.h' : a:header
	if expand('%:e') != 'm'
		return s:Error('Not in an implementation file.')
	elseif !filereadable(headerfile)
		return s:Error('Could not read header file.')
	endif

	let declarations = s:GetDeclarations(headerfile)

	if empty(declarations)
		return s:Error('Header file has no method declarations!')
	endif

	let len = len(declarations)
	let last_change = line('.')

	if search('\V'.substitute(declarations[0], '\s\+', '\\s\\+', ''))
		if !searchpair('@implementation', '', '@end', 'W')
			return s:Error('Missing @end declaration.')
		endif
		let i = 2    " Skip past the @implementation line & blank line
		let len -= 1 " Skip past @end declaration
		let lnum = line('.') - 1
	else
		let i = 0
		let lnum = line('.')
	endif
	let start_line = lnum

	while i < len
		let is_method = declarations[i][0] =~ '@\|+\|-'
		if !is_method || !search('\V'.substitute(escape(declarations[i], '\'),
		                      \ 'void\|IBAction', '\\(void\\|IBAction\\)', 'g'), 'n')
			call append(lnum, declarations[i])
			let lnum += 1
			if is_method | let last_change = lnum | endif
		else " Skip method declaration if it is already declared.
			if declarations[i][0] == '@'
				let i += 1
			else
				while declarations[i] != '}' && i < len
					let i += 1
				endw
				let i += 1
			endif
		endif
		let i += 1
	endw
	call cursor(last_change, 1)

	if lnum == start_line
		echoh WarningMsg
		let class = matchstr(declarations[0], '@implementation\s\+\zs.*')
		echo 'The methods for the "'.class.'" class have already been declared.'
		echoh None
	endif
endf
" vim:noet:sw=4:ts=4:ft=vim
