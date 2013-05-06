" config which can be overridden (shared lines)
if !exists('g:snipMate')
  let g:snipMate = {}
endif
let s:c = g:snipMate

try
	call tlib#input#List('mi', '', [])
catch /.*/
	echoe "you're missing tlib. See install instructions at ".expand('<sfile>:h:h').'/README.rst'
endtry

" match $ which doesn't follow a \
let s:d = '\%([\\]\@<!\$\)'


" disable write cache in files
" some people get errors about writing the cache files. Probably there is no
" pay off having slow disks anyway. So disabling the cache by default
let s:c.cache_parsed_snippets_on_disk = get(s:c, 'cache_parsed_snippets_on_disk', 0)
let s:c.read_snippets_cached = get(s:c, 'read_snippets_cached', {'func' : function('snipMate#ReadSnippetsFile'), 'version': 3, 'use_file_cache': s:c.cache_parsed_snippets_on_disk})

" if filetype is objc, cpp, cs or cu also append snippets from scope 'c'
" you can add multiple by separating scopes by ',', see s:AddScopeAliases
" TODO add documentation to doc/*
let s:c.scope_aliases = get(s:c, 'scope_aliases', {})
let s:c.scope_aliases.objc = get(s:c.scope_aliases, 'objc', 'c')
let s:c.scope_aliases.cpp = get(s:c.scope_aliases, 'cpp', 'c')
let s:c.scope_aliases.cu = get(s:c.scope_aliases, 'cu', 'c')
let s:c.scope_aliases.xhtml = get(s:c.scope_aliases, 'xhtml', 'html')
let s:c.scope_aliases.html = get(s:c.scope_aliases, 'html', 'javascript')
let s:c.scope_aliases.php = get(s:c.scope_aliases, 'php', 'php,html,javascript')
let s:c.scope_aliases.ur = get(s:c.scope_aliases, 'ur', 'html,javascript')
let s:c.scope_aliases.mxml = get(s:c.scope_aliases, 'mxml', 'actionscript')
let s:c.scope_aliases.eruby = get(s:c.scope_aliases, 'eruby', 'eruby-rails,html')

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

	call setline(lnum, line.snipLines[0])

	" Autoindent snippet according to previous indentation
	let indent = matchend(line, '^.\{-}\ze\(\S\|$\)') + 1
	call append(lnum, map(snipLines[1:], "'".strpart(line, 0, indent - 1)."'.v:val"))

	" Open any folds snippet expands into
	if &fen | sil! exe lnum.','.(lnum + len(snipLines) - 1).'foldopen' | endif

	let b:snip_state = copy(s:state_proto)
	let [b:snip_state.stops, b:snip_state.stop_count] = s:BuildTabStops(snippet, lnum, col - indent, indent)

	if b:snip_state.stop_count
		aug snipmate_changes
			au CursorMovedI,InsertEnter <buffer> call b:snip_state.update_changes()
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

	if exists('g:snipmate_content_visual')
		let visual = g:snipmate_content_visual | unlet g:snipmate_content_visual
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

	if &et " Expand tabs to spaces if 'expandtab' is set.
		return substitute(snippet, '\t', repeat(' ', &sts ? &sts : &sw), 'g')
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
		return -1
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

		let theline = getline(lnum)
		" subtract -1 to go from column byte index to string byte index
		" subtract another -1 to exclude the col'th element
		call setline(lnum, theline[0:(col-2)] . newWord . theline[(col+self.end_col-self.start_col-a:change-1):])
	endfor

	" Reposition the cursor in case a var updates on the same line but before
	" the current tabstop
	if oldStartSnip != self.start_col
		call cursor(0, col('.') + self.start_col - oldStartSnip)
	endif
endfunction

" should be moved to utils or such?
fun! snipMate#SetByPath(dict, path, value)
	let d = a:dict
	for p in a:path[:-2]
		if !has_key(d,p) | let d[p] = {} | endif
		let d = d[p]
	endfor
	let d[a:path[-1]] = a:value
endf

" reads a .snippets file
" returns list of
" ['triggername', 'name', 'contents']
" if triggername is not set 'default' is assumed
fun! snipMate#ReadSnippetsFile(file)
	let result = []
	if !filereadable(a:file) | return result | endif
	let r_guard = '^guard\s\+\zs.*'
	let inSnip = 0
	let guard = 1
	for line in readfile(a:file) + ["\n"]
		if inSnip == 2 && line =~ r_guard
			let guard = matchstr(line, r_guard)
		elseif inSnip && (line[0] == "\t" || line == '')
			let content .= strpart(line, 1)."\n"
			continue
		elseif inSnip
			call add(result, [trigger, name == '' ? 'default' : name, content[:-2], guard])
			let inSnip = 0
			let guard = "1"
		endif

		if inSnip == 2
			let inSnip = 1
		endif
		if line[:6] == 'snippet'
			" 2 signals first line
			let inSnip = 2
			let trigger = strpart(line, 8)
			let name = ''
			let space = stridx(trigger, ' ') + 1
			if space " Process multi snip
				let name = strpart(trigger, space)
				let trigger = strpart(trigger, 0, space - 1)
			endif
			let content = ''
		endif
	endfor
	return result
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

" don't ask me wy searching for trigger { is soo slow.
fun! s:Glob(dir,  file)
	let f = a:dir.a:file
	if a:dir =~ '\*' || isdirectory(a:dir)
		" vim's glob() is somewhat unreliable since it uses the
		" user's current shell which may accept different patterns
		" (POSIX vs. zsh vs. bash vs. ...). On my system, that
		" leads to glob() sometimes returning files that don't
		" exist, so filter the returned list to make sure that the
		" files really exist in the filesystem.
		let res = split(glob(escape(f,"{}")), "\n")

		if !empty(res)
			return filter(res, 'filereadable(v:val)')
		else
			return []
		endif
	else
		return filereadable(f) ? [f] : []
	endif
endf

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
fun! snipMate#GetSnippetFiles(mustExist, scopes, trigger)
  let paths = funcref#Call(s:c.snippet_dirs)

  let result = {}
  let scopes = s:AddScopeAliases(a:scopes)

  " collect existing files
  for scope in scopes

	for r in paths
	  let rtp_last = fnamemodify(r,':t')

	  " .snippets files (many snippets per file).
	  let glob_p = r.'/snippets/'.scope.'.snippets'
	  for snippetsF in split(glob(glob_p),"\n")
		let scope = fnamemodify(snippetsF,':t:r')
		let result[snippetsF] = {'exists': 1, 'type': 'snippets', 'name_prefix': rtp_last.' '.scope }
	  endfor

	  if !a:mustExist && !has_key(result, glob_p)
		" name_prefix not used
		let result[glob_p] = {'exists': 0, 'type': 'snippets'}
	  endif

	  let glob_p = r.'/snippets/'.scope.'/*.snippets'
	  for snippetsF in split(glob(glob_p),"\n")
		let result[snippetsF] = {'exists': 1, 'type': 'snippets', 'name_prefix' : rtp_last.' '.fnamemodify(snippetsF,':t:r')}
	  endfor

	  " == one file per snippet: ==

	  " without name snippets/<filetype>/<trigger>.snippet
	  for f in s:Glob(r.'/snippets/'.scope,'/'.a:trigger.'.snippet')
		let trigger = fnamemodify(f,':t:r')
		let result[f] = {'exists': 1, 'type': 'snippet', 'name': 'default', 'trigger': trigger, 'name_prefix' : rtp_last.' '.scope}
	  endfor
	  " add /snippets/trigger/*.snippet files (TODO)

	  " with name (multi-snip) snippets/<filetype>/<trigger>/<name>.snippet
	  for f in s:Glob(r.'/snippets/'.scope.'/'.a:trigger,'/*.snippet')
		let name = fnamemodify(f,':t:r')
		let trigger = fnamemodify(f,':h:t')
		let result[f] = {'exists': 1, 'type': 'snippet', 'name': name, 'trigger': trigger, 'name_prefix' : rtp_last.' '.scope}
	  endfor
	endfor
  endfor
  return result
endf

fun! snipMate#EvalGuard(guard)
	" left: everything left of expansion 
	" word: the expanded word
	" are guaranteed to be in scpe

	if a:guard == '1' | return 1 | endif
	let word = s:c.word
	" eval is evil, but backticks are allowed anyway.
	let left = getline('.')[:col('.')-3 - len(word)]
	exec 'return '.a:guard
endf

" default triggers based on paths
fun! snipMate#DefaultPool(scopes, trigger, result)
	let triggerR = substitute(a:trigger,'*','.*','g')
	for [f,opts] in items(snipMate#GetSnippetFiles(1, a:scopes, a:trigger))
		if opts.type == 'snippets'
			for [trigger, name, contents, guard] in cached_file_contents#CachedFileContents(f, s:c.read_snippets_cached, 0)
				if trigger !~ escape(triggerR,'~') | continue | endif
				if snipMate#EvalGuard(guard)
					call snipMate#SetByPath(a:result, [trigger, opts.name_prefix.' '.name], contents)
				endif
			endfor
		elseif opts.type == 'snippet'
			call snipMate#SetByPath(a:result, [opts.trigger, opts.name_prefix.' '.opts.name], funcref#Function('return readfile('.string(f).')'))
		else
			throw "unexpected"
		endif
	endfor
endf

" return a dict of snippets found in runtimepath matching trigger
" scopes: list of scopes. usually this is the filetype. eg ['c','cpp']
" trigger may contain glob patterns. Thus use '*' to get all triggers
"
fun! snipMate#GetSnippets(scopes, trigger)
	let result = {}
	let triggerR = escape(substitute(a:trigger,'*','.*','g'), '~') " escape '~' for use as regexp
	" let scopes = s:AddScopeAliases(a:scopes)

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
	" echo lookups

	let matching_snippets = []
	let snippet = ''
	" prefer longest word
	for word in lookups
		let s:c.word = word
		" echomsg string(lookups).' current: '.word
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

fun! snipMate#ShowAvailableSnips()
	let col   = col('.')
	let word  = matchstr(getline('.'), '\S\+\%'.col.'c')

	let snippets = map(snipMate#GetSnippetsForWordBelowCursor(word, '*', 0),'v:val[0]')
	let matches = filter(snippets, "v:val =~# '\\V\\^" . escape(word, '\') . "'")

	" Pretty hacky, but really can't have the tab swallowed!
	if len(matches) == 0
		call feedkeys(s:c['no_match_completion_feedkeys_chars'], 'n')
		return ""
	endif

	call complete(col - len(word), sort(matches))
	return ''
endf


" user interface implementation {{{1

fun! snipMate#TriggerSnippet()
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

	if exists('b:snip_state')
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
endf


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
