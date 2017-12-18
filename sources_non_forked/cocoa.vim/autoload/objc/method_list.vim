" File:         objc#method_list.vim (part of the cocoa.vim plugin)
" Author:       Michael Sanders (msanders42 [at] gmail [dot] com)
" Description:  Opens a split window containing the methods of the current file.
" Last Updated: July 13, 2009

au WinLeave Method\ List call<SID>LeaveMethodList()
au WinEnter Method\ List call objc#method_list#Activate(0)

fun objc#method_list#Activate(update)
	let s:opt = {'is':&is, 'hls': &hls} " Save current options.
	let s:last_search = @/
	set is nohls
	" If methodlist has already been opened, reactivate it.
	if exists('s:mlist_buffer') && bufexists(s:mlist_buffer)
		let mlist_win = bufwinnr(s:mlist_buffer)
		if mlist_win == -1
			sil exe 'belowright sbuf '.s:mlist_buffer
			if a:update | call s:UpdateMethodList() | endif
		elseif winbufnr(2) == -1
			quit " If no other windows are open, close the method list automatically.
		else     " If method list is out of focus, bring it back into focus.
			exe mlist_win.'winc w'
		endif
	else " Otherwise, create the method list.
		call s:CreateMethodList()
		call s:UpdateMethodList()
	endif
endf

fun s:CreateMethodList()
	botright new

	let s:sortPref = 0
	let s:mlist_buffer = bufnr('%')

	sil file Method\ List
	setl bt=nofile bh=wipe noswf nobl nonu nowrap syn=objc
	syn match objcPragmark '^[^-+@].*$'
	hi objcPragmark gui=italic term=underline

	nn <silent> <buffer> <cr> :cal<SID>SelectMethod()<cr>
	nn <buffer> q <c-w>q
	nn <buffer> p <c-w>p
	nm <buffer> l p
	nm <buffer> <2-leftmouse> <cr>
endf

" Returns the lines of all the matches in a dictionary
fun s:GetAllMatches(needle)
	let startpos = [line('.'), col('.')]
	call cursor(1, 1)

	let results = {}
	let line = search(a:needle, 'Wc')
	let key = matchstr(getline(line), a:needle)
	if !s:InComment(line, 1) && key != ''
		let results[key] = line
	endif

	while 1
		let line = search(a:needle, 'W')
		if !line | break | endif
		let key = matchstr(getline(line), a:needle)
		if !s:InComment(line, 1) && key != ''
			let results[key] = line
		endif
	endw

	call cursor(startpos)
	return results
endf

fun s:InComment(line, col)
	return stridx(synIDattr(synID(a:line, a:col, 0), 'name'), 'omment') != -1
endf

fun s:UpdateMethodList()
	winc p " Go to source file window
	let s:methods = s:GetAllMatches('^\v(\@(implementation|interface) \w+|'.
	                              \ '\s*(\+|-).*|#pragma\s+mark\s+\zs.+)')
	winc p " Go to method window

	if empty(s:methods)
		winc q
		echoh WarningMsg
		echo 'There are no methods in this file!'
		echoh None
		return
	endif

	call setline(1, sort(keys(s:methods), 's:SortByLineNum'))
	exe "norm! \<c-w>".line('$').'_'
endf

fun s:SortByLineNum(i1, i2)
	let line1 = s:methods[a:i1]
	let line2 = s:methods[a:i2]
	return line1 == line2 ? 0 : line1 > line2 ? 1 : -1
endf

fun s:SelectMethod()
	let number = s:methods[getline('.')]
	winc q
	winc p
	call cursor(number, 1)
endf

fun s:LeaveMethodList()
	for [option, value] in items(s:opt)
		exe 'let &'.option.'='.value
	endfor
	let @/ = s:last_search == '' ? '' : s:last_search
	unl s:opt s:last_search
endf
" vim:noet:sw=4:ts=4:ft=vim
