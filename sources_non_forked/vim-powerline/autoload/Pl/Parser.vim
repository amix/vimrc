let g:Pl#Parser#Symbols = {
	\ 'compatible': {
		\   'dividers': [ '', [0x2502], '', [0x2502] ]
		\ , 'symbols' : {
			\   'BRANCH': 'BR:'
			\ , 'RO'    : 'RO'
			\ , 'FT'    : 'FT'
			\ , 'LINE'  : 'LN'
		\ }
	\ },
	\ 'unicode': {
		\   'dividers': [ [0x25b6], [0x276f], [0x25c0], [0x276e]  ]
		\ , 'symbols' : {
			\   'BRANCH': [0x26a1]
			\ , 'RO'    : [0x2613]
			\ , 'FT'    : [0x2691]
			\ , 'LINE'  : [0x204b]
		\ },
	\ },
	\ 'fancy': {
		\   'dividers': [ [0x2b80], [0x2b81], [0x2b82], [0x2b83] ]
		\ , 'symbols' : {
			\   'BRANCH': [0x2b60]
			\ , 'RO'    : [0x2b64]
			\ , 'FT'    : [0x2b62, 0x2b63]
			\ , 'LINE'  : [0x2b61]
		\ }
	\ }
\ }

" Handle symbol overrides
for [s:key, s:value] in items(g:Powerline_symbols_override)
	let g:Pl#Parser#Symbols[g:Powerline_symbols].symbols[s:key] = s:value

	unlet! s:key s:value
endfor

" Handle divider overrides
if len(g:Powerline_dividers_override) == 4
	let g:Pl#Parser#Symbols[g:Powerline_symbols].dividers = g:Powerline_dividers_override
endif

let s:LEFT_SIDE = 0
let s:RIGHT_SIDE = 2

let s:PADDING = 1

let s:EMPTY_SEGMENT = { 'type': 'empty' }

let s:HARD_DIVIDER = 0
let s:SOFT_DIVIDER = 1

function! Pl#Parser#GetStatusline(segments) " {{{
	let statusline = {
		\   'n': ''
		\ , 'N': ''
		\ , 'v': ''
		\ , 'i': ''
		\ , 'r': ''
		\ , 's': ''
		\ }

	" Run through the different modes and create the statuslines
	for mode in keys(statusline)
		" Create an empty statusline list
		let stl = []

		call extend(stl, s:ParseSegments(mode, s:LEFT_SIDE, a:segments))

		let statusline[mode] .= join(stl, '')
	endfor

	return statusline
endfunction " }}}
function! Pl#Parser#ParseChars(arg) " {{{
	" Handles symbol arrays which can be either an array of hex values,
	" or a string. Will convert the hex array to a string, or return the
	" string as-is.
	let arg = a:arg

	if type(arg) == type([])
		" Hex array
		call map(arg, 'nr2char(v:val)')

		return join(arg, '')
	endif

	" Anything else, just return it as it is
	return arg
endfunction " }}}
function! s:ParseSegments(mode, side, segments, ...) " {{{
	let mode     = a:mode
	let side     = a:side
	let segments = a:segments

	let level      = exists('a:1') ? a:1 : 0
	let base_color = exists('a:2') ? a:2 : {}

	let ret = []

	for i in range(0, len(segments) - 1)
		unlet! seg_prev seg_curr seg_next

		" Prepare some resources (fetch previous, current and next segment)
		let seg_curr = deepcopy(get(segments, i))

		" Find previous segment
		let seg_prev = s:EMPTY_SEGMENT

		" If we're currently at i = 0 we have to start on 0 or else we will start on the last segment (list[-1])
		let range_start = (i == 0 ? i : i - 1)
		for j in range(range_start, 0, -1)
			let seg = deepcopy(get(segments, j))
			if get(seg, 'name') ==# 'TRUNCATE'
				" Skip truncate segments
				continue
			endif

			" Look ahead for another segment that's visible in this mode
			if index(get(seg, 'modes'), mode) != -1
				" Use this segment
				let seg_prev = seg

				break
			endif
		endfor

		"" Find next segment
		let seg_next = s:EMPTY_SEGMENT

		" If we're currently at i = len(segments) - 1 we have to start on i or else we will get an error because the index doesn't exist
		let range_start = (i == len(segments) - 1 ? i : i + 1)
		for j in range(range_start, len(segments) - 1, 1)
			let seg = deepcopy(get(segments, j))
			if get(seg, 'name') ==# 'TRUNCATE'
				" Skip truncate segments
				continue
			endif

			" Look ahead for another segment that's visible in this mode
			if index(get(seg, 'modes'), mode) != -1
				" Use this segment
				let seg_next = seg

				break
			endif
		endfor

		if index(get(seg_curr, 'modes', []), mode) == -1
			" The segment is invisible in this mode, skip it
			" FIXME When two segments after each other are hidden, a gap appears where the segments would be, this is probably due to segment padding
			continue
		endif

		" Handle the different segment types
		if seg_curr.type == 'segment'
			if seg_curr.name ==# 'TRUNCATE'
				" Truncate statusline
				call add(ret, '%<')
			elseif seg_curr.name ==# 'SPLIT'
				" Split statusline

				" Switch sides
				let side = s:RIGHT_SIDE

				" Handle highlighting
				let mode_colors = get(seg_curr.colors, mode, seg_curr.colors['n'])
				let hl_group = s:HlCreate(mode_colors)

				" Add segment text
				call add(ret, '%#'. hl_group .'#%=')
			else
				" Add segment text
				let text = seg_curr.text

				" Decide on whether to use the group's colors or the segment's colors
				let colors = get(seg_curr, 'colors', base_color)

				" Fallback to normal (current) highlighting if we don't have mode-specific highlighting
				let mode_colors = get(colors, mode, get(colors, 'n', {}))

				if empty(mode_colors)
					echom 'Segment doesn''t have any colors! NS: "'. seg_curr.ns .'" SEG: "'. seg_curr.name .'"'

					continue
				endif

				" Check if we're in a group (level > 0)
				if level > 0
					" If we're in a group we don't have dividers between
					" segments, so we should only pad one side, but only pad
					" if the segment doesn't have Pl#Segment#NoPadding() set
					let padding_right = (seg_curr.padding && side == s:LEFT_SIDE  ? repeat(' ', s:PADDING) : '')
					let padding_left  = (seg_curr.padding && side == s:RIGHT_SIDE ? repeat(' ', s:PADDING) : '')

					" Check if we lack a bg/fg color for this segment
					" If we do, use the bg/fg color from base_color
					let base_color_mode = ! has_key(base_color, mode) ? base_color['n'] : base_color[mode]

					for col in ['ctermbg', 'ctermfg', 'guibg', 'guifg']
						if empty(mode_colors[col])
							let mode_colors[col] = base_color_mode[col]
						endif
					endfor
				else
					"" If we're outside a group we have dividers and must pad both sides
					let padding_left  = repeat(' ', s:PADDING)
					let padding_right = repeat(' ', s:PADDING)
				endif

				" Get main hl group for segment
				let hl_group = s:HlCreate(mode_colors)

				" Prepare segment text
				let text = '%(%#'. hl_group .'#'. padding_left . text . padding_right . '%)'

				if level == 0
					" Add divider to single segments
					let text = s:AddDivider(text, side, mode, colors, seg_prev, seg_curr, seg_next)
				endif

				call add(ret, text)
			endif
		elseif seg_curr.type == 'segment_group'
			" Recursively parse segment group
			let func_params = [mode, side, seg_curr.segments, level + 1]

			if has_key(seg_curr, 'colors')
				" Pass the base colors on to the child segments
				call add(func_params, seg_curr.colors)
			endif

			" Get segment group string
			let text = join(call('s:ParseSegments', func_params), '')

			" Pad on the opposite side of the divider
			let padding_right = (side == s:RIGHT_SIDE ? repeat(' ', s:PADDING) : '')
			let padding_left  = (side == s:LEFT_SIDE  ? repeat(' ', s:PADDING) : '')

			let text = s:AddDivider(padding_left . text . padding_right, side, mode, seg_curr.colors, seg_prev, seg_curr, seg_next)

			call add(ret, text)
		endif
	endfor

	return ret
endfunction " }}}
function! s:HlCreate(hl) " {{{
	" Create a short and unique highlighting group name
	" It uses the hex values of all the color properties and an attribute flag at the end
	" NONE colors are translated to NN for cterm and NNNNNN for gui colors
	let hi_group = printf('Pl%s%s%s%s%s'
		\ , (a:hl['ctermfg'] == 'NONE' ? 'NN'     : printf('%02x', a:hl['ctermfg']))
		\ , (a:hl['guifg']   == 'NONE' ? 'NNNNNN' : printf('%06x', a:hl['guifg']  ))
		\ , (a:hl['ctermbg'] == 'NONE' ? 'NN'     : printf('%02x', a:hl['ctermbg']))
		\ , (a:hl['guibg']   == 'NONE' ? 'NNNNNN' : printf('%06x', a:hl['guibg']  ))
		\ , substitute(a:hl['attr'], '\v([a-zA-Z])[a-zA-Z]*,?', '\1', 'g')
		\ )

	if ! s:HlExists(hi_group)
		let ctermbg = a:hl['ctermbg'] == 'NONE' ? 'NONE' : printf('%d', a:hl['ctermbg'])
		if (has('win32') || has('win64')) && !has('gui_running') && ctermbg != 'NONE' && ctermbg > 128
			let ctermbg -= 128
		endif
		let hi_cmd = printf('hi %s ctermfg=%s ctermbg=%s cterm=%s guifg=%s guibg=%s gui=%s'
			\ , hi_group
			\ , a:hl['ctermfg'] == 'NONE' ? 'NONE' : printf('%d', a:hl['ctermfg'])
			\ , ctermbg
			\ , a:hl['attr']
			\ , (a:hl['guifg'] == 'NONE' ? 'NONE' : printf('#%06x', a:hl['guifg']))
			\ , (a:hl['guibg'] == 'NONE' ? 'NONE' : printf('#%06x', a:hl['guibg']))
			\ , a:hl['attr']
			\ )

		exec hi_cmd

		" Add command to Pl#HL list for caching
		call add(g:Pl#HL, hi_cmd)
	endif

	" Return only the highlighting group name
	return hi_group
endfunction " }}}
function! s:HlExists(hl) " {{{
	if ! hlexists(a:hl)
		return 0
	endif

	redir => hlstatus
	silent exec 'hi' a:hl
	redir END

	return (hlstatus !~ 'cleared')
endfunction " }}}
function! s:AddDivider(text, side, mode, colors, prev, curr, next) " {{{
	let seg_prev = a:prev
	let seg_curr = a:curr
	let seg_next = a:next

	" Set default color/type for the divider
	let div_colors = get(a:colors, a:mode, a:colors['n'])
	let div_type = s:SOFT_DIVIDER

	" Define segment to compare
	let cmp_seg = a:side == s:LEFT_SIDE ? seg_next : seg_prev

	let cmp_all_colors = get(cmp_seg, 'colors', {})
	let cmp_colors = get(cmp_all_colors, a:mode, get(cmp_all_colors, 'n', {}))

	if ! empty(cmp_colors)
		" Compare the highlighting groups
		"
		" If the background color for cterm is equal, use soft divider with the current segment's highlighting
		" If not, use hard divider with a new highlighting group
		"
		" Note that if the previous/next segment is the split, a hard divider is always used
		if get(div_colors, 'ctermbg') != get(cmp_colors, 'ctermbg') || get(seg_next, 'name') ==# 'SPLIT' || get(seg_prev, 'name') ==# 'SPLIT'
			let div_type = s:HARD_DIVIDER

			" Create new highlighting group
			if div_colors['attr'] =~ 'reverse' && cmp_colors['attr'] =~ 'reverse'
				" Use FG = CURRENT FG, BG = CMP FG
				let div_colors['ctermbg'] = get(cmp_colors, 'ctermfg')
				let div_colors['guibg']   = get(cmp_colors, 'guifg')

				let div_colors['attr']    = div_colors['attr'] =~ 'bold' ? 'bold' : 'NONE'
			elseif div_colors['attr'] =~ 'reverse'
				" Use FG = CURRENT FG, BG = CMP BG
				let div_colors['ctermbg'] = get(cmp_colors, 'ctermbg')
				let div_colors['guibg']   = get(cmp_colors, 'guibg')

				let div_colors['attr']    = div_colors['attr'] =~ 'bold' ? 'bold' : 'NONE'
			elseif cmp_colors['attr'] =~ 'reverse'
				" Use FG = CMP FG, BG = CURRENT BG : reversed
				let div_colors['ctermfg'] = get(cmp_colors, 'ctermfg')
				let div_colors['guifg']   = get(cmp_colors, 'guifg')

				let div_colors['attr']    = 'reverse'

			else
				" Use FG = CURRENT BG, BG = CMP BG
				let div_colors['ctermfg'] = get(div_colors, 'ctermbg')
				let div_colors['guifg']   = get(div_colors, 'guibg')

				let div_colors['ctermbg'] = get(cmp_colors, 'ctermbg')
				let div_colors['guibg']   = get(cmp_colors, 'guibg')

				let div_colors['attr']    = 'NONE'
			endif
		endif
	endif

	" Prepare divider
	let divider_raw = deepcopy(g:Pl#Parser#Symbols[g:Powerline_symbols].dividers[a:side + div_type])
	let divider = Pl#Parser#ParseChars(divider_raw)

	" Don't add dividers for segments adjacent to split (unless it's a hard divider)
	if ((get(seg_next, 'name') ==# 'SPLIT' || get(seg_prev, 'name') ==# 'SPLIT') && div_type != s:HARD_DIVIDER)
		return ''
	endif

	if a:side == s:LEFT_SIDE
		" Left side
		" Divider to the right
		return printf('%%(%s%%#%s#%s%%)', a:text, s:HlCreate(div_colors), divider)
	else
		" Right side
		" Divider to the left
		return printf('%%(%%#%s#%s%s%%)', s:HlCreate(div_colors), divider, a:text)
	endif
endfunction " }}}
