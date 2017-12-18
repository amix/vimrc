" File:         objc#man.vim (part of the cocoa.vim plugin)
" Author:       Michael Sanders (msanders42 [at] gmail [dot] com)
" Description:  Allows you to look up Cocoa API docs in Vim.
" Last Updated: December 26, 2009
" NOTE:         See http://tinyurl.com/remove-annoying-alert
"               for removing the annoying security alert in Leopard.

" Return all matches in for ":CocoaDoc <tab>" sorted by length.
fun objc#man#Completion(ArgLead, CmdLine, CursorPos)
	return system('grep -ho "^'.a:ArgLead.'\w*" ~/.vim/lib/cocoa_indexes/*.txt'.
	            \ "| perl -e 'print sort {length $a <=> length $b} <>'")
endf

let s:docsets =  []
let locations = [
			\	{'path': '/Developer/Documentation/DocSets/com.apple.ADC_Reference_Library.CoreReference.docset',
			\	'alias': 'Leopard'},
			\	{'path': '/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleSnowLeopard.CoreReference.docset',
			\	'alias': 'Snow Leopard'},
			\	{'path': '/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone3_0.iPhoneLibrary.docset',
			\	'alias': 'iPhone 3.0'}
			\	]
for location in locations
	if isdirectory(location.path)
		call add(s:docsets, location)
	endif
endfor

let s:docset_cmd = '/Developer/usr/bin/docsetutil search -skip-text -query '

fun s:OpenFile(file)
	if a:file =~ '/.*/man/'
		exe ':!'.substitute(&kp, '^man -s', 'man', '').' '.a:file
	else
		" Sometimes Xcode doesn't download a bundle fully, and docsetutil is
		" inaccurate.
		if !filereadable(matchstr(a:file, '^.*\ze#.*$'))
			echoh ErrorMsg
			echom 'File "'.a:file.'" is not readable.'
			echom 'Check that Xcode has fully downloaded the DocSet.'
			echoh None
		else
			" /usr/bin/open strips the #fragments in file:// URLs, which we need,
			" so I'm using applescript instead.
			call system('osascript -e ''open location "file://'.a:file.'"'' &')
		endif
	endif
endf

fun objc#man#ShowDoc(...)
	let word = a:0 ? a:1 : matchstr(getline('.'), '\<\w*\%'.col('.').'c\w\+:\=')

	" Look up the whole method if it takes multiple arguments.
	if !a:0 && word[len(word) - 1] == ':'
		let word = s:GetMethodName()
	endif

	if word == ''
		if !a:0 " Mimic K if using it as such
			echoh ErrorMsg
			echo 'E349: No identifier under cursor'
			echoh None
		endif
		return
	endif

	let references = {}

	" First check Cocoa docs for word using docsetutil
	for location in s:docsets
		let docset = location.path
		let response = split(system(s:docset_cmd.word.' '.docset), "\n")
		let docset .= '/Contents/Resources/Documents/' " Actual path of files
		for line in response
			" Format string is: " Language/type/class/word path"
			let path = matchstr(line, '\S*$')
			if path[0] != '/' | let path = docset.path | endif

			" Ignore duplicate entries
			if has_key(references, path) | continue | endif

			let attrs = split(matchstr(line, '^ \zs*\S*'), '/')[:2]

			" Ignore unrecognized entries.
			if len(attrs) != 3 | continue | endif

			" If no class is given use type instead
			let [lang, type, class] = attrs
			if class == '-' | let class = type | endif
			let references[path] = {'lang': lang, 'class': class,
								  \ 'location': location}
		endfor
	endfor

	" Then try man
	let man = system('man -S2:3 -aW '.word)
	if man !~ '^No manual entry'
		for path in split(man, "\n")
			if !has_key(references, path)
				let references[path] = {'lang': 'C', 'class': 'man'}
			endif
		endfor
	endif

	if len(references) == 1
		return s:OpenFile(keys(references)[0])
	elseif !empty(references)
		echoh ModeMsg | echo word | echoh None
		return s:ChooseFrom(references)
	else
		echoh WarningMsg
		echo "Can't find documentation for ".word
		echoh None
	endif
endf

fun s:ChooseFrom(references)
 	let type_abbr = {'cl' : 'Class', 'intf' : 'Protocol', 'cat' : 'Category',
	               \ 'intfm' : 'Method', 'instm' : 'Method', 'econst' : 'Enum',
	               \ 'tdef' : 'Typedef', 'macro' : 'Macro', 'data' : 'Data',
	               \ 'func' : 'Function'}
	let inputlist = []
	" Don't display "Objective-C" if all items are objc
	let show_lang = !AllKeysEqual(values(a:references), 'lang', 'Objective-C')
	let i = 1
	for ref in values(a:references)
		let class = ref.class
		if has_key(type_abbr, class) | let class = type_abbr[class] | endif
		call add(inputlist, i.'. '.(show_lang ? ref['lang'].' ' : '').class.' ('.ref.location.alias.')')
		let i += 1
	endfor
	let num = inputlist(inputlist)
	return num ? s:OpenFile(keys(a:references)[num - 1]) : -1
endf

fun AllKeysEqual(list, key, item)
	for item in a:list
		if item[a:key] != a:item
			return 0
		endif
	endfor
	return 1
endf

fun s:GetMethodName()
	let pos = [line('.'), col('.')]
	let startpos = searchpos('\v^\s*-.{-}\w+:|\[\s*\w+\s+\w+:|\]\s*\w+:', 'cbW')

	" Method declaration (- (foo) bar:)
	if getline(startpos[0]) =~ '^\s*-.\{-}\w\+:'
		let endpos = searchpos('{', 'W')
	" Message inside brackets ([foo bar: baz])
	else
		let endpos = searchpairpos('\[', '', '\]', 'W')
	endif
	call cursor(pos)

	if startpos[0] == 0 || endpos[0] == 0 | return '' | endif
	let lines = getline(startpos[0], endpos[0])

	let lines[0] = strpart(lines[0], startpos[1] - 1)
	let lines[-1] = strpart(lines[-1], 0, endpos[1])

	" Ignore outer brackets
	let message = substitute(join(lines), '^\[\|\]$', '', '')
	" Ignore nested messages [...]
	let message = substitute(message, '\[.\{-}\]', '', 'g')
	" Ignore strings (could contain colons)
	let message = substitute(message, '".\{-}"', '', 'g')
	" Ignore @selector(...)
	let message = substitute(message, '@selector(.\{-})', '', 'g')

	return s:MatchAll(message, '\w\+:')
endf

fun s:MatchAll(haystack, needle)
    let matches = matchstr(a:haystack, a:needle)
    let index = matchend(a:haystack, a:needle)
    while index != -1
		let matches .= matchstr(a:haystack, a:needle, index + 1)
        let index = matchend(a:haystack, a:needle, index + 1)
    endw
    return matches
endf
" vim:noet:sw=4:ts=4:ft=vim
