" config which can be overridden (shared lines)
if !exists('g:snipMate')
  let g:snipMate = {}
endif

try
	call tlib#input#List('mi', '', [])
catch /.*/
	echoe "you're missing tlib. See install instructions at ".expand('<sfile>:h:h').'/README.md'
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
		let snipLines = snipMate#sniplist_str(snippet, b:snip_state.stops)
	else
		let snippet = snipmate#legacy#process_snippet(a:snip)
		let [b:snip_state.stops, b:snip_state.stop_count] = snipmate#legacy#build_stops(snippet, lnum, col - indent, indent)
		let snipLines = split(substitute(snippet, printf('%s\d\+\|%s{\d\+.\{-}}',
					\ g:snipmate#legacy#sigil, g:snipmate#legacy#sigil), '', 'g'), "\n", 1)
	endif

	" Abort if the snippet is empty
	if empty(snippet)
		return ''
	endif

	" Expand snippet onto current position
	let afterCursor = strpart(line, col - 1)
	" Keep text after the cursor
	if afterCursor != "\t" && afterCursor != ' '
		let line = strpart(line, 0, col - 1)
		let snipLines[-1] .= afterCursor
	else
		let afterCursor = ''
		" For some reason the cursor needs to move one right after this
		if line != '' && col == 1 && &ve != 'all' && &ve != 'onemore'
			let col += 1
		endif
	endif

	" Insert snippet with proper indentation
	call setline(lnum, line . snipLines[0])
	call append(lnum, map(snipLines[1:], "empty(v:val) ? v:val : '" . strpart(line, 0, indent - 1) . "' . v:val"))

	" Open any folds snippet expands into
	if &foldenable
		silent! exec lnum . ',' . (lnum + len(snipLines) - 1) . 'foldopen'
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

function! snipMate#placeholder_str(num, stops) abort
	return snipMate#sniplist_str(a:stops[a:num].placeholder, a:stops)[0]
endfunction

function! snipMate#sniplist_str(snippet, stops) abort
	let lines = ['']
	let pos = 0
	let add_to = 1
	let seen_stops = []

	while pos < len(a:snippet)
		let item = a:snippet[pos]

		if type(item) == type('')
			if add_to
				let lines[-1] .= item
			else
				call add(lines, item)
			endif
			let add_to = 0
		elseif type(item) == type([])
			let lines[-1] .= snipMate#placeholder_str(item[0], a:stops)
			let add_to = 1
		endif

		let pos += 1
		unlet item " avoid E706
	endwhile

	return lines
endfunction

function! s:build_stops(snippet, stops, lnum, col, indent) abort
	let stops = a:stops
	let line  = a:lnum
	let col   = a:col

	for [id, dict] in items(stops)
		for i in dict.instances
			if len(i) > 1 && type(i[1]) != type({})
				if !has_key(dict, 'placeholder')
					let dict.placeholder = i[1:]
				else
					unlet i[1:]
				endif
			endif
		endfor
		if !has_key(dict, 'placeholder')
			let dict.placeholder = []
			let j = 0
			while len(dict.instances[j]) > 1
				let j += 1
			endwhile
			call add(dict.instances[j], '')
		endif
		unlet dict.instances
	endfor

	let [line, col] = s:build_loc_info(a:snippet, stops, line, col, a:indent)

	" add zero tabstop if it doesn't exist and then link it to the highest stop
	" number
	let stops[0] = get(stops, 0,
				\ { 'placeholder' : [], 'line' : line, 'col' : col })
	let stop_count = max(keys(stops)) + 2
	let stops[stop_count - 1] = stops[0]

	return stop_count
endfunction

function! s:build_loc_info(snippet, stops, line, col, indent) abort
	let stops   = a:stops
	let line    = a:line
	let col     = a:col
	let pos     = 0
	let in_text = 0

	while pos < len(a:snippet)
		let item = a:snippet[pos]

		if type(item) == type('')
			if in_text
				let line += 1
				let col = a:indent
			endif
			let col += len(item)
			let in_text = 1
		elseif type(item) == type([])
			let id = item[0]
			if len(item) > 1 && type(item[1]) != type({})
				let stops[id].line = line
				let stops[id].col = col
				let [line, col] = s:build_loc_info(item[1:], stops, line, col, a:indent)
			else
				call s:add_mirror(stops, id, line, col, item)
				let col += len(snipMate#placeholder_str(id, stops))
			endif
			let in_text = 0
		endif

		let pos += 1
		unlet item " avoid E706
	endwhile

	return [line, col]
endfunction

function! s:add_mirror(stops, id, line, col, item) abort
	let stops = a:stops
	let item = a:item
	let stops[a:id].mirrors = get(stops[a:id], 'mirrors', [])
	let mirror = get(a:item, 1, {})
	let mirror.line = a:line
	let mirror.col = a:col
	call add(stops[a:id].mirrors, mirror)
	if len(item) == 1
		call add(item, mirror)
	endif
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

au SourceCmd *.snippet,*.snippets call s:source_snippet()

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
	if !has_key(d, a:trigger) || a:bang
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
			for path in g:snipMate.snippet_dirs
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

function! snipMate#OpenSnippetFiles() abort
	let files = []
	let scopes_done = []
	let exists = []
	let notexists = []
	for scope in s:AddScopeAliases(snipMate#ScopesByFile())
		let files += s:snippet_filenames(scope, '')
	endfor
	call filter(files, "v:val !~# '\\*'")
	for path in g:snipMate.snippet_dirs
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
	" Setup lookups: '1.2.3' becomes [1.2.3] + [3, 2.3]
	let parts = split(a:word, '\W\zs')
	" Since '\W\zs' results in splitting *after* a non-keyword character, the
	" first \W stays connected to whatever's before it, so split it off
	if !empty(parts) && parts[0] =~ '\W$'
		let parts = [ parts[0][:-2], strpart(parts[0], len(parts[0]) - 1) ]
					\ + parts[1:]
	endif
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
	if len(snippet) == 1
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
	let snippets = map(snipMate#GetSnippetsForWordBelowCursor(a:word, 0), 'v:val[0]')
	return filter(snippets, 'v:val =~# "\\V\\^' . escape(a:word, '"\') . '"')
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
		call feedkeys("\<tab>") | return ''
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
