function! s:sfile() abort
	return expand('<sfile>')
endfunction

let s:state_proto = {}

function! snipmate#jumping#state() abort
	return copy(s:state_proto)
endfunction

function! s:listize_mirror(mirrors) abort
	return map(copy(a:mirrors), '[v:val.line, v:val.col]')
endfunction

" Removes snippet state info
function! s:state_remove() dict abort
	" Remove all autocmds in group snipmate_changes in the current buffer
	unlet! b:snip_state
	silent! au! snipmate_changes * <buffer>
endfunction

function! s:state_find_next_stop(backwards) dict abort
	let self.stop_no += a:backwards? -1 : 1
	while !has_key(self.stops, self.stop_no)
		if self.stop_no == self.stop_count
			let self.stop_no = 0
		endif
		if self.stop_no <= 0 && a:backwards
			let self.stop_no = self.stop_count - 1
		endif
		let self.stop_no += a:backwards? -1 : 1
	endwhile
endfunction

" Update state information to correspond to the given tab stop
function! s:state_set_stop(backwards) dict abort
	call self.find_next_stop(a:backwards)

	let self.cur_stop    = self.stops[self.stop_no]
	let self.stop_len = (type(self.cur_stop.placeholder) == type(0))
				\ ? self.cur_stop.placeholder
				\ : len(snipMate#placeholder_str(self.stop_no, self.stops))
	let self.start_col   = self.cur_stop.col
	let self.end_col     = self.start_col + self.stop_len
	let self.mirrors     = get(self.cur_stop, 'mirrors', [])
	let self.old_mirrors = deepcopy(self.mirrors)

	call cursor(self.cur_stop.line, self.cur_stop.col)

	let self.prev_len    = col('$')
	let self.changed = 0

	for mirror in self.mirrors
		let mirror.oldSize = self.stop_len
	endfor

	if exists("self.cur_stop.items")
		let ret = self.select_item()
	else
		let ret = self.select_word()
	endif

	if (self.stop_no == 0 || self.stop_no == self.stop_count - 1) && !a:backwards
		call self.remove()
	endif

	return ret
endfunction

" Jump to the next/previous tab stop
function! s:state_jump_stop(backwards) dict abort
	" Update changes just in case
	" This seems to be only needed because insert completion does not trigger
	" the CursorMovedI event
	call self.update_changes()

	" Store placeholder/location changes
	let self.cur_stop.col = self.start_col
	unlet! self.cur_stop.placeholder " avoid type error for old parsing version
	let self.cur_stop.placeholder = [strpart(getline('.'),
				\ self.start_col - 1, self.end_col - self.start_col)]
	if self.changed
		call self.remove_nested()

		" Remove selection items if the stop has changed and the new placeholder
		" is not one of the selection items
		if exists('self.cur_stop.items') &&
					\ !count(self.cur_stop.items, self.cur_stop.placeholder)
			call remove(self.cur_stop, 'items')
		endif
	endif

	return self.set_stop(a:backwards)
endfunction

function! s:state_remove_nested(...) dict abort
	let id = a:0 ? a:1 : self.stop_no
	if type(self.stops[id].placeholder) == type([])
		for i in self.stops[id].placeholder
			if type(i) == type([])
				if type(i[1]) != type({})
					call self.remove_nested(i[0])
					call remove(self.stops, i[0])
				else
					call filter(self.stops[i[0]].mirrors, 'v:val isnot i[1]')
				endif
			endif
			unlet i " Avoid E706
		endfor
	endif
endfunction

" Select the placeholder for the current tab stop
function! s:state_select_word() dict abort
	let len = self.stop_len
	if !len | return '' | endif
	let l = col('.') != 1 ? 'l' : ''
	if &sel == 'exclusive'
		return "\<esc>".l.'v'.len."l\<c-g>"
	endif
	return len == 1 ? "\<esc>".l.'gh' : "\<esc>".l.'v'.(len - 1)."l\<c-g>"
endfunction

" Update the snippet as text is typed. The self.update_mirrors() function does
" the actual work.
" If the cursor moves outside of a placeholder, call self.remove()
function! s:state_update_changes() dict abort
	let change_len = col('$') - self.prev_len
	let self.changed = self.changed || change_len != 0
	let self.end_col += change_len
	let col = col('.')

	" Increase the endpoint by 1 for &sel = exclusive
	if line('.') != self.cur_stop.line || col < self.start_col
				\ || col > (self.end_col + (&sel == 'exclusive'))
		return self.remove()
	endif

	call self.update(self.cur_stop, change_len, change_len)
	if !empty(self.mirrors)
		call self.update_mirrors(change_len)
	endif

	let self.prev_len = col('$')
endfunction

" Actually update the mirrors for any changed text
function! s:state_update_mirrors(change) dict abort
	let newWordLen = self.end_col - self.start_col
	let newWord = strpart(getline('.'), self.start_col - 1, newWordLen)
	let changeLen = a:change
	let curLine = line('.')
	let curCol = col('.')
	let oldStartSnip = self.start_col
	let i = 0

	for mirror in self.mirrors
		for stop in values(filter(copy(self.stops), 'v:key != 0'))
			if type(stop.placeholder) == type(0)
				if mirror.line == stop.line && mirror.col > stop.col
							\ && mirror.col < stop.col + stop.placeholder
					let stop.placeholder += changeLen
				endif
			endif
		endfor

		if has_key(mirror, 'oldSize')
			" recover the old size deduce the endline
			let oldSize = mirror.oldSize
		else
			" first time, we use the intitial size
			let oldSize = strlen(newWord)
		endif

		" current mirror transformation, and save size
		let wordMirror= substitute(newWord, get(mirror, 'pat', ''), get(mirror, 'sub', ''), get(mirror, 'flags', ''))
		let mirror.oldSize = strlen(wordMirror)

		" Update other objects on the line
		call self.update(mirror, changeLen, mirror.oldSize - oldSize)

		call s:set_line(mirror.line, mirror.col, oldSize, wordMirror)
	endfor

	" Reposition the cursor in case a var updates on the same line but before
	" the current tabstop
	if oldStartSnip != self.start_col || mode() == 'i'
		call cursor(0, curCol + self.start_col - oldStartSnip)
	endif
endfunction

function! s:state_find_update_objects(item) dict abort
	let item = a:item
	let item.update_objects = []

	" Filter the zeroth stop because it's duplicated as the last
	for stop in values(filter(copy(self.stops), 'v:key != 0'))
		if stop.line == item.line && stop.col > item.col
			call add(item.update_objects, stop)
		endif

		let placeholder_len = len(snipMate#sniplist_str(stop.placeholder, b:snip_state.stops))
		for mirror in get(stop, 'mirrors', [])
			let mirror.oldSize = placeholder_len
			if mirror.line == item.line && mirror.col > item.col
				call add(item.update_objects, mirror)
			endif
		endfor
	endfor

	return item.update_objects
endfunction

function! s:state_update(item, change_len, mirror_change) dict abort
	let item = a:item
	if !exists('item.update_objects')
		let item.update_objects = self.find_update_objects(a:item)
	endif
	let to_update = item.update_objects

	for obj in to_update
		" object does not necessarly have the same decalage
		" than mirrors if mirrors use regexp
		let obj.col += a:mirror_change
		if obj is self.cur_stop
			let self.start_col += a:change_len
			let self.end_col += a:change_len
		endif
	endfor
endfunction

" Split the line into three parts: the mirror, what's before it, and
" what's after it. Then combine them using the new mirror string.
" Subtract one to go from column index to byte index
function! s:set_line(line, col, len, word)
	let theline = getline(a:line)
	let begin = strpart(theline, 0, a:col - 1)
	let end = strpart(theline, a:col + a:len - 1)
	call setline(a:line, begin . a:word . end)
endfunction

" If &cotl contains at least one of these three, we need to add one to our menu
" selection hack in s:state_select_item
function! s:cot_count()
	let cotl = split(&cot, ',')
	let c = (has('patch-9.0.0567') && count(cotl, 'longest')) + count(cotl, 'noinsert') + count(cotl, 'noselect')
	return min([1, c])
endfunction

function! s:state_select_item() dict abort
	let items = map(copy(self.cur_stop.items), 'snipMate#sniplist_str(v:val, b:snip_state.stops)')
	call s:set_line(line('.'), self.start_col, self.end_col - self.start_col, '')
	call complete(self.start_col, items)
	for i in range(index(self.cur_stop.items, self.cur_stop.placeholder) + s:cot_count())
		call feedkeys("\<C-N>")
	endfor
	return ''
endfunction

call extend(s:state_proto, snipmate#util#add_methods(s:sfile(), 'state',
			\ [ 'remove', 'set_stop', 'jump_stop', 'remove_nested',
			\ 'select_word', 'update_changes', 'update_mirrors', 'select_item',
			\ 'find_next_stop', 'find_update_objects', 'update' ]), 'error')

" vim:noet:sw=4:ts=4:ft=vim
