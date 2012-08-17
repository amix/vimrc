let s:default_modes = ['n', 'N', 'v', 'i', 'r', 's']

function! s:CheckConditions(params) " {{{
	" Check conditions for a segment/group
	" Integer parameters are always conditions
	for param in a:params
		if type(param) == type(0) && param == 0
			" Break here if it's an integer parameter and it's false (0)
			return 0
		endif
		unlet! param
	endfor

	return 1
endfunction " }}}
function! Pl#Segment#Create(name, ...) " {{{
	" Check condition parameters
	if ! s:CheckConditions(a:000)
		return {}
	endif

	let name = a:name
	let modes = s:default_modes
	let padding = 1
	let segments = []

	for param in a:000
		" Lookup modes for this segment/group
		if type(param) == type([]) && param[0] == 'modes'
			let modes = param[1]
		elseif type(param) == type([]) && param[0] == 'nopadding'
			let padding = 0
		elseif type(a:1) == type([]) && a:1[0] == 'segment'
			call add(segments, param[1])
		endif

		unlet! param
	endfor

	if type(a:1) == type([]) && a:1[0] == 'segment'
		" This is a segment group
		return ['segment_group', {
			\   'type': 'segment_group'
			\ , 'name': name
			\ , 'segments': segments
			\ , 'modes': modes
			\ , 'padding': padding
			\ }]
	else
		" This is a single segment
		let text = a:1

		" Search/replace symbols
		for [key, symbol] in items(g:Pl#Parser#Symbols[g:Powerline_symbols].symbols)
			let text = substitute(
				\ text,
				\ '\v\$('. key .')',
				\ '\=Pl#Parser#ParseChars(g:Pl#Parser#Symbols[g:Powerline_symbols].symbols[submatch(1)])',
				\ 'g')

			unlet! key symbol
		endfor

		return ['segment', {
			\   'type': 'segment'
			\ , 'name': name
			\ , 'text': text
			\ , 'modes': modes
			\ , 'padding': padding
			\ }]
	endif

endfunction " }}}
function! Pl#Segment#Init(params) " {{{
	" Check condition parameters
	if ! s:CheckConditions(a:params)
		return {}
	endif

	let segments = {}
	let ns = ''

	for param in a:params
		if type(param) == type('')
			" String parameters is the namespace
			let ns = param
		elseif type(param) == type([])
			" The data dict is in param[1]
			" By default we don't have a namespace for the segment
			let segment = param[1]

			if ! empty(ns)
				" Update segment so that it includes the namespace
				" Add the namespace to the segment dict key
				let segment.ns = ns
				let segment.name = join([segment.ns, segment.name], ':')
			endif

			let key = segment.name

			let segments[key] = segment
		endif

		unlet! param
	endfor

	return segments
endfunction " }}}
function! Pl#Segment#Modes(modes) " {{{
	" Handle modes for both segments and groups
	let modes = split(a:modes, '\zs')

	if modes[0] == '!'
		" Filter modes (e.g. "!nr" will ignore the segment in normal and replace modes)
		let modes = filter(deepcopy(s:default_modes), 'v:val !~# "['. join(modes[1:]) .']"')
	endif

	return ['modes', modes]
endfunction " }}}
function! Pl#Segment#NoPadding() " {{{
	return ['nopadding']
endfunction " }}}
function! Pl#Segment#Split(...) " {{{
	return a:0 ? a:1 .':SPLIT' : 'SPLIT'
endfunction " }}}
function! Pl#Segment#Truncate() " {{{
	return 'TRUNCATE'
endfunction " }}}
function! Pl#Segment#Get(name) " {{{
	" Return a segment data dict
	let args = []

	" Check for printf segments (lists)
	if type(a:name) == type([])
		" We're dealing with a segment with printf arguments
		let seg_orig_name = a:name[0]
		let args = a:name[1:]
	else
		let seg_orig_name = a:name
	endif

	" Fetch namespace and variants for storing in the segment dict
	let seg_ns = ''
	let seg_variants = []

	" Retrieve color scheme variants
	let seg_name_split = split(seg_orig_name, '\v\.')
	if len(seg_name_split) > 1
		let seg_variants = seg_name_split[1:]
	endif

	" Retrieve segment name and namespace
	" Use the first piece of the split string, we can't have variants in the final segment name
	let seg_name_split = split(seg_name_split[0], '\v:')
	let seg_name = seg_name_split[0]

	if len(seg_name_split) > 1
		let seg_ns = seg_name_split[0]
		let seg_name = seg_name_split[-1]
	endif

	try
		" If we have a namespace, try to use the namespaced segment first (i.e. search for the segment in the namespaced file first)
		let return_segment = deepcopy(g:Powerline#Segments#{seg_ns}#segments[seg_ns .':'. seg_name])
	catch
		try
			" We didn't find a namespaced segment, fall back to common segments
			let return_segment = deepcopy(g:Powerline#Segments#segments[seg_name])
		catch
			" Didn't find the segment among the common segments either, just skip it
			return {}
		endtry
	endtry

	if len(args) && has_key(return_segment, 'text')
		" Handle segment printf arguments
		" printf doesn't accept lists as its second argument, so we have to work around that
		let return_segment.text = call('printf', [ return_segment.text ] + args)
	endif

	" Assign namespace, name and variants
	let return_segment.ns = seg_ns
	let return_segment.name = seg_name
	let return_segment.orig_name = seg_orig_name
	let return_segment.variants = seg_variants

	return return_segment
endfunction " }}}
