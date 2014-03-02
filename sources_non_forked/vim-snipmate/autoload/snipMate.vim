" config which can be overridden (shared lines)
if !exists('g:snipMate')
  let g:snipMate = {}
endif
let s:c = g:snipMate

try
	call tlib#input#List('mi', '', [])
catch /.*/
	echoe "you're missing tlib. See install instructions at ".expand('<sfile>:h:h').'/README.md'
endtry

" match $ which doesn't follow a \
let s:d = '\%([\\]\@<!\$\)'

" if filetype is objc, cpp, cs or cu also append snippets from scope 'c'
" you can add multiple by separating scopes by ',', see s:AddScopeAliases
let s:c.scope_aliases = get(s:c, 'scope_aliases', {})
if !exists('g:snipMate_no_default_aliases') || !g:snipMate_no_default_aliases
	let s:c.scope_aliases.objc = get(s:c.scope_aliases, 'objc', 'c')
	let s:c.scope_aliases.cpp = get(s:c.scope_aliases, 'cpp', 'c')
	let s:c.scope_aliases.cu = get(s:c.scope_aliases, 'cu', 'c')
	let s:c.scope_aliases.xhtml = get(s:c.scope_aliases, 'xhtml', 'html')
	let s:c.scope_aliases.html = get(s:c.scope_aliases, 'html', 'javascript')
	let s:c.scope_aliases.php = get(s:c.scope_aliases, 'php', 'php,html,javascript')
	let s:c.scope_aliases.ur = get(s:c.scope_aliases, 'ur', 'html,javascript')
	let s:c.scope_aliases.mxml = get(s:c.scope_aliases, 'mxml', 'actionscript')
	let s:c.scope_aliases.eruby = get(s:c.scope_aliases, 'eruby', 'eruby-rails,html')
endif

" set this to "\<tab>" to make snipmate not swallow tab (make sure to not have
" expandtab set). Remember that you can always enter tabs by <c-v> <tab> then
" you don't need this
let s:c['no_match_completion_feedkeys_chars'] = get(s:c, 'no_match_completion_feedkeys_chars', "\t")

fun! Filename(...)
	let filename = expand('%:t:r')
	if filename == '' | return a:0 == 2 ? a:2 : '' | endif
	return !a:0 || a:1 == '' ? filename : substitute(a:1, '$1', filename, 'g')
endf

let s:state_proto = {}
let s:cache = {}

fun! s:state_proto.remove()
	unlet! b:snip_state
	" Remove all buffer-local autocommands in the snipmate_changes group
	au! snipmate_changes * <buffer>
endf

fun! snipMate#expandSnip(snip, col)
	let lnum = line('.') | let col = a:col

	let snippet = s:ProcessSnippet(a:snip)
	" Avoid error if eval evaluates to nothing
	if snippet == '' | return '' | endif

	" Expand snippet onto current position with the tab stops removed
	let snipLines = split(substitute(snippet, ''.s:d .'\d\+\|'.s:d .'{\d\+.\{-}}', '', 'g'), "\n", 1)

	let line = getline(lnum)
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
	let indent = match(line, '\S\|$') + 1
	call setline(lnum, line . snipLines[0])
	call append(lnum, map(snipLines[1:], "empty(v:val) ? v:val : '" . strpart(line, 0, indent - 1) . "' . v:val"))

	" Open any folds snippet expands into
	if &fen | sil! exe lnum.','.(lnum + len(snipLines) - 1).'foldopen' | endif

	let b:snip_state = copy(s:state_proto)
	let [b:snip_state.stops, b:snip_state.stop_count] = s:BuildTabStops(snippet, lnum, col - indent, indent)

	if b:snip_state.stop_count
		aug snipmate_changes
			au CursorMoved,CursorMovedI <buffer> call b:snip_state.update_changes()
		aug END
		call b:snip_state.set_stop(0)

		return b:snip_state.select_word()
	else
		unlet b:snip_state
		" Place cursor at end of snippet if no tab stop is given
		let newlines = len(snipLines) - 1
		call cursor(lnum + newlines, indent + len(snipLines[-1]) - len(afterCursor)
					\ + (newlines ? 0: col - 1))
	endif
	return ''
endf

" Update state information to correspond to the given tab stop
function! s:state_proto.set_stop(stop)
	let self.stop_no   = a:stop
	let self.cur_stop  = self.stops[self.stop_no]
	let self.end_col   = self.cur_stop[1] + self.cur_stop[2]
	let self.start_col = self.cur_stop[1]
	call cursor(self.cur_stop[0], self.cur_stop[1])
	let self.prev_len  = col('$')
	let self.has_vars  = exists('self.cur_stop[3]')
	let self.old_vars  = self.has_vars ? deepcopy(self.cur_stop[3]) : []
endfunction

" Prepare snippet to be processed by s:BuildTabStops
fun! s:ProcessSnippet(snip)
	let snippet = a:snip

	if exists('b:snipmate_content_visual')
		let visual = b:snipmate_content_visual
		unlet b:snipmate_content_visual
	else
		let visual = ''
	endif
	let snippet = substitute(snippet,'{VISUAL}', escape(visual,'%\'), 'g')

	" Evaluate eval (`...`) expressions.
	" Backquotes prefixed with a backslash "\" are ignored.
	" And backslash can be escaped by doubling it.
	" Using a loop here instead of a regex fixes a bug with nested "\=".
	if stridx(snippet, '`') != -1
		let new = []
		let snip = split(snippet, '\%(\\\@<!\%(\\\\\)*\)\@<=`', 1)
		let isexp = 0
		for i in snip
			if isexp
				call add(new, substitute(eval(i), "\n\\%$", '', ''))
			else
				call add(new, i)
			endif
			let isexp = !isexp
		endfor
		let snippet = join(new, '')
		let snippet = substitute(snippet, "\r", "\n", 'g')
		let snippet = substitute(snippet, '\\`', "`", 'g')
		let snippet = substitute(snippet, '\\\\', "\\", 'g')
	endif

	" Place all text after a colon in a tab stop after the tab stop
	" (e.g. "${#:foo}" becomes "${:foo}foo").
	" This helps tell the position of the tab stops later.
	let snippet = substitute(snippet, s:d.'{\d\+:\(.\{-}\)}', '&\1', 'g')

	" Update the a:snip so that all the $# become the text after
	" the colon in their associated ${#}.
	" (e.g. "${1:foo}" turns all "$1"'s into "foo")
	let i = 1
	while snippet =~ s:d.'{'.i
		let s = matchstr(snippet, s:d.'{'.i.':\zs.\{-}\ze}')
		if s != ''
			let snippet = substitute(snippet, s:d.i, s.'&', 'g')
		endif
		let i += 1
	endw

	" Add ${0} tab stop if found
	if snippet =~ s:d . '{0'
		let snippet = substitute(snippet, s:d.'{0', '${'.i, '')
		let s = matchstr(snippet, s:d.'{'.i.':\zs.\{-}\ze}')
		if s != ''
			let snippet = substitute(snippet, s:d.'0', '$'.i, 'g')
			let snippet = substitute(snippet, s:d.i, s.'&', 'g')
		endif
	else
		let snippet .= '${'.i.'}'
	endif

	if &et " Expand tabs to spaces if 'expandtab' is set.
		return substitute(snippet, '\t', repeat(' ', (&sts > 0) ? &sts : &sw), 'g')
	endif
	return snippet
endf

" Counts occurences of haystack in needle
fun! s:Count(haystack, needle)
	let counter = 0
	let index = stridx(a:haystack, a:needle)
	while index != -1
		let index = stridx(a:haystack, a:needle, index+1)
		let counter += 1
	endw
	return counter
endf

" Builds a list of a list of each tab stop in the snippet containing:
" 1.) The tab stop's line number.
" 2.) The tab stop's column number
"     (by getting the length of the string between the last "\n" and the
"     tab stop).
" 3.) The length of the text after the colon for the current tab stop
"     (e.g. "${1:foo}" would return 3).
" 4.) If the "${#:}" construct is given, another list containing all
"     the matches of "$#", to be replaced with the placeholder. This list is
"     composed the same way as the parent; the first item is the line number,
"     and the second is the column.
fun! s:BuildTabStops(snip, lnum, col, indent)
	let snipPos = []
	let i = 1
	let withoutVars = substitute(a:snip, '$\d\+', '', 'g')
	while a:snip =~ s:d.'{'.i
		let beforeTabStop = matchstr(withoutVars, '^.*\ze'.s:d .'{'.i.'\D')
		let withoutOthers = substitute(withoutVars, ''.s:d .'{\('.i.'\D\)\@!\d\+.\{-}}', '', 'g')

		let j = i - 1
		call add(snipPos, [0, 0, 0])
		let snipPos[j][0] = a:lnum + s:Count(beforeTabStop, "\n")
		let snipPos[j][1] = a:indent + len(matchstr(withoutOthers, '.*\(\n\|^\)\zs.*\ze'.s:d .'{'.i.'\D'))
		if snipPos[j][0] == a:lnum | let snipPos[j][1] += a:col | endif

		" Get all $# matches in another list, if ${#:name} is given
		if withoutVars =~ ''.s:d .'{'.i.':'
			let snipPos[j][2] = len(matchstr(withoutVars, ''.s:d .'{'.i.':\zs.\{-}\ze}'))
			let dots = repeat('.', snipPos[j][2])
			call add(snipPos[j], [])
			let withoutOthers = substitute(a:snip, ''.s:d .'{\d\+.\{-}}\|'.s:d .''.i.'\@!\d\+', '', 'g')
			while match(withoutOthers, ''.s:d .''.i.'\(\D\|$\)') != -1
				let beforeMark = matchstr(withoutOthers, '^.\{-}\ze'.dots.''.s:d .''.i.'\(\D\|$\)')
				call add(snipPos[j][3], [0, 0])
				let snipPos[j][3][-1][0] = a:lnum + s:Count(beforeMark, "\n")
				let snipPos[j][3][-1][1] = a:indent + (snipPos[j][3][-1][0] > a:lnum
				                           \ ? len(matchstr(beforeMark, '.*\n\zs.*'))
				                           \ : a:col + len(beforeMark))
				let withoutOthers = substitute(withoutOthers, ''.s:d .''.i.'\ze\(\D\|$\)', '', '')
			endw
		endif
		let i += 1
	endw
	return [snipPos, i - 1]
endf

function! s:state_proto.jump_stop(backwards)
	" Update changes just in case
	" This seems to be only needed because insert completion does not trigger
	" the CursorMovedI event
	call self.update_changes()

	" Update stop and var locations
	call self.update_stops()

	" Store the changed col/length of the current stop
	let self.cur_stop[1] = self.start_col
	let self.cur_stop[2] = self.end_col - self.start_col

	let self.stop_no += a:backwards ? -1 : 1
	" Loop over the snippet when going backwards from the beginning
	if self.stop_no < 0 | let self.stop_no = self.stop_count - 1 | endif

	if self.stop_no == self.stop_count
		call self.remove()
		return ''
	endif

	call self.set_stop(self.stop_no)
	return self.select_word()
endfunction

" Updates tab stops/vars
function! s:state_proto.update_stops()
	let changeLen = self.end_col - self.cur_stop[2] - self.start_col
	" Update tab stops in snippet if text has been added via "$#"
	" (e.g., in "${1:foo}bar$1${2}").
	if changeLen != 0
		let curLine = line('.')

		for pos in self.stops
			if pos == self.cur_stop | continue | endif
			let changed = pos[0] == curLine && pos[1] > self.start_col
			let changedVars = 0
			let endPlaceholder = pos[2] - 1 + pos[1]
			" Subtract changeLen from each tab stop that was after any of
			" the current tab stop's placeholders.
			for [lnum, col] in self.old_vars
				if lnum > pos[0] | break | endif
				if pos[0] == lnum
					if pos[1] > col || (pos[2] == -1 && pos[1] == col)
						let changed += 1
					elseif col < endPlaceholder
						let changedVars += 1
					endif
				endif
			endfor
			let pos[1] += changeLen * changed
			" Parse variables within placeholders, e.g., "${1:foo} ${2:$1bar}"
			let pos[2] += changeLen * changedVars

			" Do the same to any placeholders in the other tab stops.
			if exists('pos[3]')
				for nPos in pos[3]
					let changed = nPos[0] == curLine && nPos[1] > self.start_col
					if changed && nPos[1] < self.start_col + self.cur_stop[2]
						call remove(pos, index(pos, nPos))
						continue
					endif
					for [lnum, col] in self.old_vars
						if lnum > nPos[0] | break | endif
						if nPos[0] == lnum && nPos[1] > col
							let changed += 1
						endif
					endfor
					let nPos[1] += changeLen * changed
				endfor
			endif
		endfor
	endif
endfunction

" Select the placeholder for the current tab stop
function! s:state_proto.select_word()
	let len = self.cur_stop[2]
	if !len | return '' | endif
	let l = col('.') != 1 ? 'l' : ''
	if &sel == 'exclusive'
		return "\<esc>".l.'v'.len."l\<c-g>"
	endif
	return len == 1 ? "\<esc>".l.'gh' : "\<esc>".l.'v'.(len - 1)."l\<c-g>"
endfunction

" Update the snippet as text is typed. The self.update_vars() function does
" the actual work.
" If the cursor moves outside of a placeholder, call self.remove()
function! s:state_proto.update_changes()
	let change_len = col('$') - self.prev_len
	let self.end_col += change_len

	let col = col('.')
	if line('.') != self.cur_stop[0] || col < self.start_col || col > self.end_col
		call self.remove()
	elseif self.has_vars
		call self.update_vars(change_len)
	endif

	let self.prev_len = col('$')
endfunction

" Actually update the vars for any changed text
function! s:state_proto.update_vars(change)
	let newWordLen = self.end_col - self.start_col
	let newWord = strpart(getline('.'), self.start_col - 1, newWordLen)
	let changeLen = a:change
	let curLine = line('.')
	let oldStartSnip = self.start_col
	let updateTabStops = changeLen != 0
	let i = 0

	for [lnum, col] in self.cur_stop[3]
		if updateTabStops
			let start = self.start_col
			if lnum == curLine && col <= start
				let self.start_col += changeLen
				let self.end_col += changeLen
			endif
			for nPos in self.cur_stop[3][(i):]
				" This list is in ascending order, so quit if we've gone too far.
				if nPos[0] > lnum | break | endif
				if nPos[0] == lnum && nPos[1] > col
					let nPos[1] += changeLen
				endif
			endfor
			if lnum == curLine && col > start
				let col += changeLen
				let self.cur_stop[3][i][1] = col
			endif
			let i += 1
		endif

		" Split the line into three parts: the mirror, what's before it, and
		" what's after it. Then combine them using the new mirror string.
		" Subtract one to go from column index to byte index
		let theline = getline(lnum)
		let update  = strpart(theline, 0, col - 1)
		let update .= newWord
		let update .= strpart(theline, col + self.end_col - self.start_col - a:change - 1)
		call setline(lnum, update)
	endfor

	" Reposition the cursor in case a var updates on the same line but before
	" the current tabstop
	if oldStartSnip != self.start_col || mode() == 'i'
		call cursor(0, col('.') + self.start_col - oldStartSnip)
	endif
endfunction

" reads a .snippets file
" returns list of
" ['triggername', 'name', 'contents']
" if triggername is not set 'default' is assumed
fun! snipMate#ReadSnippetsFile(file)
	let result = []
	let new_scopes = []
	if !filereadable(a:file) | return [result, new_scopes] | endif
	let inSnip = 0
	for line in readfile(a:file) + ["\n"]
		if inSnip && (line[0] == "\t" || line == '')
			let content .= strpart(line, 1)."\n"
			continue
		elseif inSnip
			call add(result, [trigger, name == '' ? 'default' : name, content[:-2]])
			let inSnip = 0
		endif

		if line[:6] == 'snippet'
			let inSnip = 1
			let trigger = strpart(line, 8)
			let name = ''
			let space = stridx(trigger, ' ') + 1
			if space " Process multi snip
				let name = strpart(trigger, space)
				let trigger = strpart(trigger, 0, space - 1)
			endif
			let content = ''
		elseif line[:6] == 'extends'
			call extend(new_scopes, map(split(strpart(line, 8)),
						\ "substitute(v:val, ',*$', '', '')"))
		endif
	endfor
	return [result, new_scopes]
endf

" adds scope aliases to list.
" returns new list
" the aliases of aliases are added recursively
fun! s:AddScopeAliases(list)
  let did = {}
  let scope_aliases = get(s:c,'scope_aliases', {})
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

function! s:Glob(path, expr)
	let res = []
	for p in split(a:path, ',')
		let h = fnamemodify(a:expr, ':h')
		if isdirectory(p . '/' . h)
			call extend(res, split(glob(p . '/' . a:expr), "\n"))
		endif
	endfor
	return filter(res, 'filereadable(v:val)')
endfunction

" returns dict of
" { path: { 'type': one of 'snippet' 'snippets',
"           'exists': 1 or 0
"           " for single snippet files:
"           'name': name of snippet
"           'trigger': trigger of snippet
"         }
" }
" use trigger = '*' to match all snippet files
" use mustExist = 1 to return existing files only
"
"     mustExist = 0 is used by OpenSnippetFiles
function! snipMate#GetSnippetFiles(mustExist, scopes, trigger)
	let paths = join(funcref#Call(s:c.snippet_dirs), ',')
	let result = {}
	let scopes = s:AddScopeAliases(a:scopes)
	let trigger = escape(a:trigger, "*[]?{}`'$")

	" collect existing files
	for scope in scopes

		for f in s:Glob(paths, 'snippets/' . scope . '.snippets') +
					\ s:Glob(paths, 'snippets/' . scope . '/*.snippets')
			let result[f] = { 'exists' : 1, 'type' : 'snippets',
						\ 'name_prefix' : fnamemodify(f, ':t:r') }
		endfor

		for f in s:Glob(paths, 'snippets/'.scope.'/'.trigger.'.snippet')
			let result[f] = {'exists': 1, 'type': 'snippet', 'name': 'default',
						\ 'trigger': a:trigger, 'name_prefix' : scope }
		endfor

		for f in s:Glob(paths, 'snippets/'.scope.'/'.trigger.'/*.snippet')
			let result[f] = {'exists': 1, 'type': 'snippet', 'name' : fnamemodify(f, ':t:r'),
						\ 'trigger': a:trigger, 'name_prefix' : scope }
		endfor

		if !a:mustExist
			for p in split(paths, ',')
				let p .= '/snippets/' . scope . '.snippets'
				let result[p] = get(result, p, {'exists': 0, 'type': 'snippets'})
			endfor
		endif

	endfor
	return result
endfunction

" should be moved to utils or such?
function! snipMate#SetByPath(dict, path, value)
	let d = a:dict
	for p in a:path[:-2]
		if !has_key(d,p) | let d[p] = {} | endif
		let d = d[p]
	endfor
	let d[a:path[-1]] = a:value
endfunction

function! s:ReadFile(file)
	if a:file =~ '\.snippet$'
		return [['', '', readfile(a:file), '1']]
	else
		return snipMate#ReadSnippetsFile(a:file)
	endif
endfunction

function! s:CachedSnips(file)
	let mtime = getftime(a:file)
	if has_key(s:cache, a:file) && s:cache[a:file].mtime >= mtime
		return s:cache[a:file].contents
	endif
	let s:cache[a:file] = {}
	let s:cache[a:file].mtime = mtime
	let s:cache[a:file].contents = snipMate#ReadSnippetsFile(a:file)
	return s:cache[a:file].contents
endfunction

" default triggers based on paths
function! snipMate#DefaultPool(scopes, trigger, result)
	let extra_scopes = []
	for [f,opts] in items(snipMate#GetSnippetFiles(1, a:scopes, a:trigger))
		let opts.name_prefix = matchstr(f, '\v/\zs.{-}\ze/snippets') . ' ' . opts.name_prefix
		if opts.type == 'snippets'
			let [snippets, new_scopes] = s:CachedSnips(f)
			call extend(extra_scopes, new_scopes)
			for [trigger, name, contents] in snippets
				if trigger =~ '\V\^' . escape(a:trigger, '\')
					call snipMate#SetByPath(a:result,
								\ [trigger, opts.name_prefix . ' ' . name],
								\ contents)
				endif
			endfor
		elseif opts.type == 'snippet'
			call snipMate#SetByPath(a:result, [opts.trigger, opts.name_prefix.' '.opts.name], readfile(f))
		else
			throw "unexpected"
		endif
	endfor

	if !empty(extra_scopes)
		call snipMate#DefaultPool(extra_scopes, a:trigger, a:result)
	endif
endfunction

" return a dict of snippets found in runtimepath matching trigger
" scopes: list of scopes. usually this is the filetype. eg ['c','cpp']
" trigger may contain glob patterns. Thus use '*' to get all triggers
"
fun! snipMate#GetSnippets(scopes, trigger)
	let result = {}

	for F in values(g:snipMateSources)
	  call funcref#Call(F, [a:scopes, a:trigger, result])
	endfor
	return result
endf

" adds leading tab
" and replaces leading spaces by tabs
" see ftplugin/snippet.vim
fun! snipMate#RetabSnip() range
  let leadingTab = expand('%:e') == 'snippets'

  let lines = getline(a:firstline, a:lastline)

  " remove leading "\t"
  let allIndented = 1
  for l in lines
	if l[0] != '\t' | let allIndented = 0 | endif
  endfor

  " retab
  if allIndented
	call map(lines, 'v:val[1:]')
  endif

  let leadingSp = filter(map(copy(lines),'matchstr(v:val,"^\\s*") '),'v:val !=""')
  if !empty(leadingSp)
	" lines containing leading spaces found
	let smallestInd =  len(sort(leadingSp)[-1])
	let ind = input('retab, spaces per tab: ', smallestInd)
	for i in range(0, len(lines)-1)
	  let ml = matchlist(lines[i], '^\(\s*\)\(.*\)')
	  let lines[i] = repeat("\t", len(ml[1]) / ind)
				 \ . repeat( " ", len(ml[1]) % ind)
				 \ . ml[2]
	endfor
  endif
  " readd tab
  let tab = leadingTab ? "\t" : ""
  for i in range(0,len(lines)-1)
	call setline(a:firstline + i, tab.lines[i])
  endfor
endf

fun! snipMate#OpenSnippetFiles()
  let dict = snipMate#GetSnippetFiles(0, snipMate#ScopesByFile(), '*')
  " sort by files wether they exist - put existing files first
  let exists = []
  let notExists = []
  for [file, v] in items(dict)
	let v['file'] = file
	if v['exists']
	  call add(exists, v)
	else
	  call add(notExists, v)
	endif
  endfor
  let all = exists + notExists
  let show = map(copy(all),'(v:val["exists"] ? "exists:" : "does not exist yet:")." ".v:val["file"]')
  let select = tlib#input#List('mi', 'select files to be opened in splits', show)
  for idx in select
	exec 'sp '.all[idx - 1]['file']
  endfor
endf

fun! snipMate#ScopesByFile()
	" duplicates are removed in AddScopeAliases
	return filter(funcref#Call(s:c.get_scopes), "v:val != ''")
endf

" used by both: completion and insert snippet
fun! snipMate#GetSnippetsForWordBelowCursor(word, suffix, break_on_first_match)
	" Setup lookups: '1.2.3' becomes [1.2.3] + [3, 2.3]
	let parts = split(a:word, '\W\zs')
	if len(parts) > 2
		let parts = parts[-2:] " max 2 additional items, this might become a setting
	endif
	let lookups = [a:word.a:suffix]
	let lookup = ''
	for w in reverse(parts)
		let lookup = w . lookup
		if index(lookups, lookup) == -1
			call add(lookups, lookup.a:suffix)
		endif
	endfor

	" allow matching '.'
	if a:word =~ '\.$'
		call add(lookups, '.'.a:suffix)
	endif

	call filter(lookups, 'v:val != ""')

	let matching_snippets = []
	let snippet = ''
	" prefer longest word
	for word in lookups
		let s:c.word = word
		for [k,snippetD] in items(funcref#Call(s:c['get_snippets'], [snipMate#ScopesByFile(), word]))
			if a:suffix == ''
				" hack: require exact match
				if k !=# word | continue | endif
			endif
			call add(matching_snippets, [k, snippetD])
			if a:break_on_first_match | break| endif
		endfor
	endfor
	return matching_snippets
endf

" snippets: dict containing snippets by name
" usually this is just {'default' : snippet_contents }
fun! s:ChooseSnippet(snippets)
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

fun! snipMate#WordBelowCursor()
	return matchstr(getline('.'), '\S\+\%' . col('.') . 'c')
endf

fun! snipMate#GetSnippetsForWordBelowCursorForComplete(word)
	let snippets = map(snipMate#GetSnippetsForWordBelowCursor(a:word, '*', 0), 'v:val[0]')
	return filter(snippets, 'v:val =~# "\\V\\^' . escape(a:word, '"\') . '"')
endf

fun! snipMate#CanBeTriggered()
	let word    = snipMate#WordBelowCursor()
	let matches = snipMate#GetSnippetsForWordBelowCursorForComplete(word)
	return len(matches) > 0
endf

fun! snipMate#ShowAvailableSnips()
	let col     = col('.')
	let word    = snipMate#WordBelowCursor()
	let matches = snipMate#GetSnippetsForWordBelowCursorForComplete(word)

	" Pretty hacky, but really can't have the tab swallowed!
	if len(matches) == 0
		call feedkeys(s:c['no_match_completion_feedkeys_chars'], 'n')
		return ""
	endif

	call complete(col - len(word), sort(matches))
	return ''
endf

" Pass an argument to force snippet expansion instead of triggering or jumping
function! snipMate#TriggerSnippet(...)
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
	let list = snipMate#GetSnippetsForWordBelowCursor(word, '',  1)
	if empty(list)
		let snippet = ''
	else
		let [trigger, snippetD] = list[0]

		let s = s:ChooseSnippet(snippetD)
		if type(s) == type([])
			let snippet = join(s, "\n")
		else
			let snippet = s
		end

		" Before expanding snippet, create new undo point |i_CTRL-G|
		let &undolevels = &undolevels
		let col = col('.') - len(trigger)
		sil exe 's/\V'.escape(trigger, '/\.').'\%#//'
		return snipMate#expandSnip(snippet, col)
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

fun! snipMate#BackwardsSnippet()
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
