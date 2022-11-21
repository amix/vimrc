" config which can be overridden (shared lines)
if !exists('g:snipMate')
  let g:snipMate = {}
endif

try
	call tlib#input#List('mi', '', [])
catch /.*/
	echoe "tlib is missing. See install instructions at ".expand('<sfile>:h:h').'/README.md'
endtry

fun! Filename(...) abort
	let filename = expand('%:t:r')
	if filename == '' | return a:0 == 2 ? a:2 : '' | endif
	return !a:0 || a:1 == '' ? filename : substitute(a:1, '$1', filename, 'g')
endf

let s:cache = {}

function! snipMate#expandSnip(snip, version, col) abort
	let lnum = line('.')
	let col = a:col
	let line = getline(lnum)
	let indent = match(line, '\S\|$') + 1
	let b:snip_state = snipmate#jumping#state()

	if a:version == 1
		let [snippet, b:snip_state.stops] = snipmate#parse#snippet(a:snip)
		" Build stop/mirror info
		let b:snip_state.stop_count = s:build_stops(snippet, b:snip_state.stops, lnum, col, indent)
	else
		let snippet = snipmate#legacy#process_snippet(a:snip)
		let [b:snip_state.stops, b:snip_state.stop_count] = snipmate#legacy#build_stops(snippet, lnum, col - indent, indent)
	endif

	" Abort if the snippet is empty
	if empty(snippet)
		return ''
	endif

	let col = s:insert_snippet_text(snippet, lnum, col, indent)

	" Open any folds snippet expands into
	if &foldenable
		silent! exec lnum . 'foldopen!'
	endif

	aug snipmate_changes
		au CursorMoved,CursorMovedI <buffer> if exists('b:snip_state') |
					\     call b:snip_state.update_changes() |
					\ else |
					\     silent! au! snipmate_changes * <buffer> |
					\ endif
	aug END

	let b:snip_state.stop_no = 0
	return b:snip_state.set_stop(0)
endfunction

function! s:insert_snippet_text(snippet, lnum, col, indent)
	let line = getline(a:lnum)
	let col = a:col
	let snippet = type(a:snippet) == type([]) ? a:snippet : split(a:snippet, "\n", 1)
	let lnum = a:lnum

	" Keep text after the cursor
	let afterCursor = strpart(line, col - 1)
	if afterCursor != "\t" && afterCursor != ' '
		let line = strpart(line, 0, col - 1)
	else
		let afterCursor = ''
		" For some reason the cursor needs to move one right after this
		if line != '' && col == 1 && &ve != 'all' && &ve != 'onemore'
			let col += 1
		endif
	endif

	call setline(lnum, '')
	call append(lnum, repeat([''], len(snippet) - 1))

	for item in snippet
		let add = lnum == a:lnum ? line : strpart(line, 0, a:indent - 1)

		if !(empty(item) || (type(item) == type([]) && empty(item[0])))
			if type(item) == type([])
				call setline(lnum, add .
							\ snipMate#sniplist_str(item, b:snip_state.stops))
			else
				call setline(lnum, add .
							\ substitute(item, printf('%s\d\+\|%s{\d\+.\{-}}',
							\ g:snipmate#legacy#sigil, g:snipmate#legacy#sigil),
							\ '', 'g'))
			endif
		endif

		let lnum += 1
	endfor

	call setline(lnum - 1, getline(lnum - 1) . afterCursor)

	return col
endfunction

function! snipMate#placeholder_str(num, stops) abort
	return snipMate#sniplist_str(a:stops[a:num].placeholder, a:stops)
endfunction

function! snipMate#sniplist_str(snippet, stops) abort
	let str = ''
	let pos = 0
	let add_to = 1
	let seen_stops = []

	while pos < len(a:snippet)
		let item = a:snippet[pos]

		if type(item) == type('')
			let str .= item
		elseif type(item) == type([])
			let placeholder = snipMate#placeholder_str(item[0], a:stops)
			if len(item) > 1 && type(item[1]) == type({})
				let placeholder = substitute(placeholder,
							\ get(item[1], 'pat', ''),
							\ get(item[1], 'sub', ''),
							\ get(item[1], 'flags', ''))
			endif
			let str .= placeholder
		endif

		let pos += 1
		unlet item " avoid E706
	endwhile

	return str
endfunction

function! s:build_stops(snippet, stops, lnum, col, indent) abort
	let stops = a:stops
	let lnum  = a:lnum
	let col   = a:col

	for line in a:snippet
		let col = s:build_loc_info(line, stops, lnum, col, [])
		if line isnot a:snippet[-1]
			let lnum += 1
			let col = a:indent
		endif
	endfor

	" add zero tabstop if it doesn't exist and then link it to the highest stop
	" number
	let stops[0] = get(stops, 0,
				\ { 'placeholder' : [], 'line' : lnum, 'col' : col })
	let stop_count = max(keys(stops)) + 2
	let stops[stop_count - 1] = stops[0]

	return stop_count
endfunction

function! s:build_loc_info(snippet, stops, lnum, col, seen_items) abort
	let stops   = a:stops
	let lnum    = a:lnum
	let col     = a:col
	let pos     = 0
	let in_text = 0
	let seen_items = a:seen_items

	for item in a:snippet
		if type(item) == type('')
			let col += len(item)
		elseif type(item) == type([])
			let id = item[0]
			let stub = item[-1]
			let stub.line = lnum
			let stub.col = col
			call s:add_update_objects(stub, seen_items)

			if len(item) > 2 && type(item[1]) != type({})
				let col = s:build_loc_info(item[1:-2], stops, lnum, col, seen_items)
			else
				let col += len(snipMate#placeholder_str(id, stops))
			endif

			let in_text = 0
		endif
		unlet item " avoid E706
	endfor

	return col
endfunction

function! s:add_update_objects(object, targets) abort
	let targets = a:targets

	for item in targets
		let item.update_objects = get(item, 'update_objects', [])
		call add(item.update_objects, a:object)
	endfor

	call add(targets, a:object)
endfunction

" reads a .snippets file
" returns list of
" ['triggername', 'name', 'contents']
" if triggername is not set 'default' is assumed
" TODO: better error checking
fun! snipMate#ReadSnippetsFile(file) abort
	let result = []
	let new_scopes = []
	if !filereadable(a:file) | return [result, new_scopes] | endif
	let inSnip = 0
	let line_no = 0
	let snipversion = get(g:snipMate, 'snippet_version', 0)
	for line in readfile(a:file) + ["\n"]
		let line_no += 1

		if inSnip && (line[0] == "\t" || line == '')
			let content .= strpart(line, 1)."\n"
			continue
		elseif inSnip
			call add(result, [trigger, name,
						\     content[:-2], bang, snipversion])
			let inSnip = 0
		endif

		if line[:6] == 'snippet'
			let inSnip = 1
			let bang = (line[7] == '!')
			if bang
				let bang += line[8] == '!'
			endif
			let trigger = strpart(line, 8 + bang)
			let name = ''
			let space = stridx(trigger, ' ') + 1
			if space " Process multi snip
				let name = strpart(trigger, space)
				let trigger = strpart(trigger, 0, space - 1)
			endif
			let content = ''
			if trigger =~ '^\s*$' " discard snippets with empty triggers
				echom 'Invalid snippet in' a:file 'near line' line_no
				let inSnip = 0
			endif
		elseif line[:6] == 'extends'
			call extend(new_scopes, map(split(strpart(line, 8)),
						\ "substitute(v:val, ',*$', '', '')"))
		elseif line[:6] == 'version'
			let snipversion = +strpart(line, 8)
		endif
	endfor
	return [result, new_scopes]
endf

function! s:GetScopes() abort
	let ret = exists('b:snipMate.scope_aliases') ? copy(b:snipMate.scope_aliases) : {}
	let global = get(g:snipMate, 'scope_aliases', {})
	for alias in keys(global)
		if has_key(ret, alias)
			let ret[alias] = join(split(ret[alias], ',')
						\ + split(global[alias], ','), ',')
		else
			let ret[alias] = global[alias]
		endif
	endfor
	return ret
endfunction

" adds scope aliases to list.
" returns new list
" the aliases of aliases are added recursively
fun! s:AddScopeAliases(list) abort
  let did = {}
  let scope_aliases = s:GetScopes()
  let new = a:list
  let new2 =  []
  while !empty(new)
	for i in new
	  if !has_key(did, i)
		let did[i] = 1
		call extend(new2, split(get(scope_aliases,i,''),','))
	  endif
	endfor
	let new = new2
	let new2 = []
  endwhile
  return keys(did)
endf

augroup SnipMateSource
	au SourceCmd *.snippet,*.snippets call s:source_snippet()
augroup END

function! s:info_from_filename(file) abort
	let parts = split(fnamemodify(a:file, ':r'), '/')
	let snipidx = len(parts) - index(reverse(copy(parts)), 'snippets') - 1
	let rtp_prefix = join(parts[(snipidx -
				\ (parts[snipidx - 1] == 'after' ? 3 : 2)):snipidx - 1], '/')
	let trigger = get(parts, snipidx + 2, '')
	let desc = get(parts, snipidx + 3, get(g:snipMate, 'override', 0) ?
				\ '' : fnamemodify(a:file, ':t'))
	return [rtp_prefix, trigger, desc]
endfunction

function! s:source_snippet() abort
	let file = expand('<afile>:p')
	let [rtp_prefix, trigger, desc] = s:info_from_filename(file)
	let new_snips = []
	if fnamemodify(file, ':e') == 'snippet'
		call add(new_snips, [trigger, desc, join(readfile(file), "\n"), 0,
					\ get(g:snipMate, 'snippet_version', 0)])
	else
		let [snippets, extends] = s:CachedSnips(file)
		let new_snips = deepcopy(snippets)
		call extend(s:lookup_state.extends, extends)
	endif
	for snip in new_snips
		if get(g:snipMate, 'override', 0)
			let snip[1] = join([s:lookup_state.scope, snip[1]])
		else
			let snip[1] = join([s:lookup_state.scope, rtp_prefix,
						\ empty(snip[1]) ? desc : snip[1]])
		endif
	endfor
	call extend(s:lookup_state.snips, new_snips)
endfunction

function! s:CachedSnips(file) abort
	let mtime = getftime(a:file)
	if has_key(s:cache, a:file) && s:cache[a:file].mtime >= mtime
		return s:cache[a:file].contents
	endif
	let s:cache[a:file] = {}
	let s:cache[a:file].mtime = mtime
	let s:cache[a:file].contents = snipMate#ReadSnippetsFile(a:file)
	return s:cache[a:file].contents
endfunction

function! s:snippet_filenames(scope, trigger) abort
	let mid = ['', '_*', '/*']
	let mid += map(copy(mid), "'/' . a:trigger . '*' . v:val")
	call map(mid, "'snippets/' . a:scope . v:val . '.snippet'")
	return map(mid[:2], 'v:val . "s"') + mid[3:]
endfunction

function! snipMate#SetByPath(dict, trigger, path, snippet, bang, snipversion) abort
	let d = a:dict
	if a:bang == 2
		unlet! d[a:trigger]
		return
	elseif !has_key(d, a:trigger) || a:bang == 1
		let d[a:trigger] = {}
	endif
	let d[a:trigger][a:path] = [a:snippet, a:snipversion]
endfunction

if v:version < 704 || has('win32')
	function! s:Glob(path, expr)
		let res = []
		for p in split(a:path, ',')
			let h = split(fnamemodify(a:expr, ':h'), '/')[0]
			if isdirectory(p . '/' . h)
				call extend(res, split(glob(p . '/' . a:expr), "\n"))
			endif
		endfor
		return filter(res, 'filereadable(v:val)')
	endfunction
else
	function! s:Glob(path, expr)
		return split(globpath(a:path, a:expr), "\n")
	endfunction
endif

" default triggers based on paths
function! snipMate#DefaultPool(scopes, trigger, result) abort
	let scopes = s:AddScopeAliases(a:scopes)
	let scopes_done = []
	let s:lookup_state = {}
	let s:lookup_state.snips = []

	while !empty(scopes)
		let scope = remove(scopes, 0)
		let s:lookup_state.scope = scope
		let s:lookup_state.extends = []

		for expr in s:snippet_filenames(scope, escape(a:trigger, "*[]?{}`'$|#%"))
			for path in s:snippet_dirs()
				for file in s:Glob(path, expr)
					source `=file`
				endfor
			endfor
		endfor

		call add(scopes_done, scope)
		call extend(scopes, s:lookup_state.extends)
		call filter(scopes, 'index(scopes_done, v:val) == -1')
	endwhile

	for [trigger, desc, contents, bang, snipversion] in s:lookup_state.snips
		if trigger =~ '\V\^' . escape(a:trigger, '\')
			call snipMate#SetByPath(a:result, trigger, desc, contents, bang, snipversion)
		endif
	endfor
endfunction

" return a dict of snippets found in runtimepath matching trigger
" scopes: list of scopes. usually this is the filetype. eg ['c','cpp']
" trigger may contain glob patterns. Thus use '*' to get all triggers
"
fun! snipMate#GetSnippets(scopes, trigger) abort
	let result = {}

	for F in values(g:snipMateSources)
	  call funcref#Call(F, [a:scopes, a:trigger, result])
	endfor
	return result
endf

function! s:snippet_dirs() abort
	return get(g:snipMate, 'snippet_dirs', split(&rtp, ','))
endfunction

function! snipMate#OpenSnippetFiles() abort
	let files = []
	let scopes_done = []
	let exists = []
	let notexists = []
	for scope in s:AddScopeAliases(snipMate#ScopesByFile())
		let files += s:snippet_filenames(scope, '')
	endfor
	call filter(files, "v:val !~# '\\*'")
	for path in s:snippet_dirs()
		let fullpaths = map(copy(files), 'printf("%s/%s", path, v:val)')
		let exists += filter(copy(fullpaths), 'filereadable(v:val)')
		let notexists += map(filter(copy(fullpaths),
					\ 'v:val =~# "\.snippets" && !filereadable(v:val)'),
					\       '"does not exist: " . v:val')
	endfor
	let all = exists + notexists
	let select = tlib#input#List('mi', 'select files to be opened in splits', all)
	for idx in select
		exec 'sp' all[idx - 1]
	endfor
endfunction

fun! snipMate#ScopesByFile() abort
	" duplicates are removed in AddScopeAliases
	return filter(funcref#Call(g:snipMate.get_scopes), "v:val != ''")
endf

" used by both: completion and insert snippet
fun! snipMate#GetSnippetsForWordBelowCursor(word, exact) abort
	" Split non-word characters into their own piece
	" so 'foo.bar..baz' becomes ['foo', '.', 'bar', '.', '.', 'baz']
	" First split just after a \W and then split each resultant string just
	" before a \W
	let parts = filter(tlib#list#Flatten(
				\ map(split(a:word, '\W\zs'), 'split(v:val, "\\ze\\W")')),
				\ '!empty(v:val)')
	" Only look at the last few possibilities. Too many can be slow.
	if len(parts) > 5
		let parts = parts[-5:]
	endif
	let lookups = [a:word]
	let lookup = ''
	for w in reverse(parts)
		let lookup = w . lookup
		if index(lookups, lookup) == -1
			call add(lookups, lookup)
		endif
	endfor

	" Remove empty lookup entries, but only if there are other nonempty lookups
	if len(lookups) > 1
		call filter(lookups, 'v:val != ""')
	endif

	let matching_snippets = []
	let snippet = ''
	" prefer longest word
	for word in lookups
		let g:snipMate.word = word
		for [k,snippetD] in items(funcref#Call(g:snipMate['get_snippets'], [snipMate#ScopesByFile(), word]))
			" hack: require exact match
			if a:exact && k !=# word
				continue
			endif
			call add(matching_snippets, [k, snippetD])
			if a:exact
				break
			endif
		endfor
	endfor
	return matching_snippets
endf

" snippets: dict containing snippets by name
" usually this is just {'default' : snippet_contents }
fun! s:ChooseSnippet(snippets) abort
	let snippet = []
	let keys = keys(a:snippets)
	let i = 1
	for snip in keys
		let snippet += [i.'. '.snip]
		let i += 1
	endfor
	if len(snippet) == 1 || get(g:snipMate, 'always_choose_first', 0) == 1
		" there's only a single snippet, choose it
		let idx = 0
	else
		let idx = tlib#input#List('si','select snippet by name',snippet) -1
		if idx == -1
			return ''
		endif
	endif
	" if a:snippets[..] is a String Call returns it
	" If it's a function or a function string the result is returned
	return funcref#Call(a:snippets[keys(a:snippets)[idx]])
endf

fun! snipMate#WordBelowCursor() abort
	return matchstr(getline('.'), '\S\+\%' . col('.') . 'c')
endf

fun! snipMate#GetSnippetsForWordBelowCursorForComplete(word) abort
	let matches = snipMate#GetSnippetsForWordBelowCursor(a:word, 0)
	let snippets = []
	for [trigger, dict] in matches
		if get(g:snipMate, 'description_in_completion', 0)
			call extend(snippets, map(keys(dict),
						\ '{ "word" : trigger, "menu" : v:val, "dup" : 1 }'))
		else
			call add(snippets, { "word" : trigger })
		endif
	endfor
	return filter(snippets,
				\ 'v:val.word =~# "\\V\\^' . escape(a:word, '"\') . '"')
endf

fun! snipMate#CanBeTriggered() abort
	let word    = snipMate#WordBelowCursor()
	let matches = snipMate#GetSnippetsForWordBelowCursorForComplete(word)
	return len(matches) > 0
endf

fun! snipMate#ShowAvailableSnips() abort
	let col     = col('.')
	let word    = snipMate#WordBelowCursor()
	let matches = snipMate#GetSnippetsForWordBelowCursorForComplete(word)

	" Pretty hacky, but really can't have the tab swallowed!
	if len(matches) == 0
		call feedkeys(g:snipMate['no_match_completion_feedkeys_chars'], 'n')
		return ""
	endif

	call complete(col - len(word), sort(matches))
	return ''
endf

" Pass an argument to force snippet expansion instead of triggering or jumping
function! snipMate#TriggerSnippet(...) abort
	if exists('g:SuperTabMappingForward')
		if g:SuperTabMappingForward == "<tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabForward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-n>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		elseif g:SuperTabMappingBackward == "<tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabBackward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-p>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		endif
	endif

	if pumvisible() " Update snippet if completion is used, or deal with supertab
		if exists('SuperTabKey')
			call feedkeys(SuperTabKey) | return ''
		endif
		call feedkeys("\<esc>a", 'n') " Close completion menu
		" Once we've dismissed the completion menu, we have to cause this
		" function to be executed over again, so that we actually get the
		" snippet triggered. (Simply continuing to execute fails because
		" we have to finish this function before the results of feedkeys take
		" effect and dismiss the completion menu. Recursing also fails for
		" similar reasons.)
		if a:0 == 0
			" Would be nice to have a more robust solution than manually
			" branching on the arguments. I tried to do something like:
			" call call(function('snipMate#TriggerSnippet'), a:000)
			" But I couldn't quite get it working. Maybe somebody else with
			" better vimscript skills can find a way to make it work, though?
			call feedkeys("\<Plug>snipMateNextOrTrigger") | return ''
		else
			call feedkeys("\<Plug>snipMateTrigger") | return ''
		endif
	endif

	if exists('b:snip_state') && a:0 == 0 " Jump only if no arguments
		let jump = b:snip_state.jump_stop(0)
		if type(jump) == 1 " returned a string
			return jump
		endif
	endif

	let word = matchstr(getline('.'), '\S\+\%'.col('.').'c')
	let list = snipMate#GetSnippetsForWordBelowCursor(word, 1)
	if empty(list)
		let snippet = ''
	else
		let [trigger, snippetD] = list[0]
		let snippet = s:ChooseSnippet(snippetD)
		" Before expanding snippet, create new undo point |i_CTRL-G|
		let &undolevels = &undolevels
		let col = col('.') - len(trigger)
		sil exe 's/\V'.escape(trigger, '/\.').'\%#//'
		return snipMate#expandSnip(snippet[0], snippet[1], col)
	endif

	" should allow other plugins to register hooks instead (duplicate code)
	if exists('SuperTabKey')
		call feedkeys(SuperTabKey)
		return ''
	endif
	return word == ''
	  \ ? "\<tab>"
	  \ : "\<c-r>=snipMate#ShowAvailableSnips()\<cr>"
endfunction

fun! snipMate#BackwardsSnippet() abort
	if exists('b:snip_state') | return b:snip_state.jump_stop(1) | endif

	if exists('g:SuperTabMappingForward')
		if g:SuperTabMappingForward == "<s-tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabForward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-n>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		elseif g:SuperTabMappingBackward == "<s-tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabBackward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-p>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		endif
	endif
	" should allow other plugins to register hooks instead (duplicate code)
	if exists('SuperTabKey')
		call feedkeys(SuperTabKey)
		return ''
	endif
	return "\<s-tab>"
endf

" vim:noet:sw=4:ts=4:ft=vim
