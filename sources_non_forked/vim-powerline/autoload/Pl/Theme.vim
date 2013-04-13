function! Pl#Theme#Create(...) " {{{
	let buffer_segments = []

	for buffer_segment in a:000
		" Remove empty segments (e.g. 'Pl#Theme#Function's)
		if empty(buffer_segment)
			continue
		endif

		call add(buffer_segments, buffer_segment)
	endfor

	let buffer_segments = Pl#Colorscheme#Apply(g:Powerline_colorscheme, buffer_segments)

	return buffer_segments
endfunction " }}}
function! Pl#Theme#Callback(name, expr) " {{{
	return ['callback', a:name, a:expr]
endfunction " }}}
function! Pl#Theme#Buffer(ns, ...) " {{{
	let segments = []
	let ns = ! empty(a:ns) ? a:ns .':' : ''

	" Match namespace parameter by default
	let matches = Pl#Match#Any(a:ns)
	let callback = []

	let args = a:000
	let args = Pl#Mod#ApplySegmentMods(args)

	" Fetch segment data dicts
	for item in args
		if type(item) == type([])
			if item[0] == 'match'
				" Match item, overrides default namespace match
				let matches = item

				unlet! item
				continue
			elseif item[0] == 'callback'
				" Store the item as a callback expression
				let matches = ['match', 'none']
				let callback = [a:ns, item[1], item[2]]

				unlet! item
				continue
			endif

			" printf segment, append ns to first item in list
			let item[0] = ns . item[0]
		else
			let item = ns . item
		endif

		let segment = Pl#Segment#Get(item)

		if ! empty(segment)
			" Skip empty (possible disabled) segments
			call add(segments, segment)
		endif

		unlet! item
	endfor

	return {
		\   'matches': matches
		\ , 'callback': callback
		\ , 'segments': segments
		\ }
endfunction " }}}
function! Pl#Theme#InsertSegment(new_segment, where, target_segment) " {{{
	" It's very important to NOT refer to the theme dict until everything's loaded!
	"
	" Because these functions are called from the vimrc, we need to put the
	" actions in a list which will be parsed later.
	"
	" These functions don't accept a name parameter, because they work on the
	" currently selected theme (will change any selected theme)
	call Pl#Mod#AddSegmentMod('insert_segment', {
		\ 'new_segment': a:new_segment,
		\ 'where': a:where,
		\ 'target_segment': a:target_segment
		\ })
endfunction " }}}
function! Pl#Theme#RemoveSegment(target_segment) " {{{
	" It's very important to NOT refer to the theme dict until everything's loaded!
	"
	" Because these functions are called from the vimrc, we need to put the
	" actions in a list which will be parsed later.
	"
	" These functions don't accept a name parameter, because they work on the
	" currently selected theme (will change any selected theme)
	call Pl#Mod#AddSegmentMod('remove_segment', {
		\ 'target_segment': a:target_segment
		\ })
endfunction " }}}
function! Pl#Theme#ReplaceSegment(old_segment, new_segment) " {{{
	call Pl#Theme#InsertSegment(a:new_segment, 'after', a:old_segment)
	call Pl#Theme#RemoveSegment(a:old_segment)
endfunction " }}}
