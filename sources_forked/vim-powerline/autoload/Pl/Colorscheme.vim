function! Pl#Colorscheme#Init(hi) " {{{
	let colorscheme = {}

	for hi in a:hi
		" Ensure that the segments are a list
		let segments = type(hi[0]) == type('') ? [ hi[0] ] : hi[0]
		let mode_hi_dict = hi[1]

		for segment in segments
			let colorscheme[segment] = mode_hi_dict
		endfor
	endfor

	return colorscheme
endfunction " }}}
function! Pl#Colorscheme#Apply(colorscheme, buffer_segments) " {{{
	" Set color parameters for all segments in a:buffer_segments

	" TODO This function should be recursive and work on both segments and groups
	" TODO We could probably handle the NS stuff here...

	try
		let colorscheme = g:Powerline#Colorschemes#{a:colorscheme}#colorscheme
	catch
		echom 'Color scheme "'. a:colorscheme .'" doesn''t exist!'

		return
	endtry

	let buffer_segments = a:buffer_segments

	" This is a bit complex, I'll walk you through exactly what happens here...
	"
	" First of all we loop through the buffer_segments, which are the segments that
	" this specific buffer will have.
	for buffer_segment in buffer_segments
		" The buffer_segment consists of a 'matches' list and a 'segments' list.
		" The 'matches' list has conditions to limit this statusline to specific buffers/windows.
		" The 'segments' list has each segment and segment group for this buffer
		for segment in buffer_segment.segments
			let type = get(segment, 'type', '')

			if type == 'segment_group'
				" We're going to handle segment groups different from single segments. Segment groups
				" have child segments which may have their own highlighting (e.g. fileinfo.flags),
				" and these child segments may be grouped (e.g. fileinfo.flags.ro) to provide very
				" specific highlighting. So here we'll handle all that:

				" Set the default/fallback colors for this group
				for i in range(len(segment.variants), 0, -1)
					" Check for available highlighting for the main group segment
					"
					" This works like the segment highlighting below
					" TODO Create a function for this
					let seg_variants = join(segment.variants[0:i], '.')

					let seg_name = i > 0 ? segment.name .'.'. seg_variants : segment.name
					let seg_ns_name = len(segment.ns) > 0 ? segment.ns .':'. seg_name : seg_name

					if has_key(colorscheme, seg_ns_name)
						" We have a namespaced highlight group
						let segment.colors = colorscheme[seg_ns_name]
						break
					elseif has_key(colorscheme, seg_name)
						" We have a non-namespaced group
						let segment.colors = colorscheme[seg_name]
						break
					endif
				endfor

				" The reason why we need to deepcopy the group's segments is that the child segments
				" all point to the same base segments and that screws up highlighting if we highlight
				" some child segments with different namespaced colors
				let segment.segments = deepcopy(segment.segments)

				" Apply colors to each child segment
				for child_segment in segment.segments
					" Check if this child segment is grouped (e.g. fileinfo.flags.group.subgroup)
					" We're going to prioritize the most specific grouping and then work back to the
					" most common group (e.g. fileinfo.flags)

					" FIXME We don't have the variants from before because group children aren't run through Pl#Segment#Get
					let child_segment.variants = [seg_name] + split(child_segment.name, '\.')

					" Use the parent group's namespace
					let child_segment.ns = segment.ns

					for i in range(len(child_segment.variants), 0, -1)
						" Check for available highlighting for the main group segment
						let child_seg_name = join(child_segment.variants[0:i], '.')

						let child_seg_ns_name = len(child_segment.ns) > 0 ? child_segment.ns .':'. child_seg_name : child_seg_name

						if has_key(colorscheme, child_seg_ns_name)
							" We have a namespaced highlight group
							let child_segment.colors = colorscheme[child_seg_ns_name]
							break
						elseif has_key(colorscheme, child_seg_name)
							" We have a non-namespaced group
							let child_segment.colors = colorscheme[child_seg_name]
							break
						endif
					endfor
				endfor
			elseif type == 'segment'
				for i in range(len(segment.variants), 0, -1)
					" Check for available highlighting
					"
					" This is done in the following manner, using the segment gundo:static_filename.text.buffer as an example:
					"
					" * Look for the hl group: gundo:static_filename.text.buffer
					" * Look for the hl group:       static_filename.text.buffer
					" * Look for the hl group: gundo:static_filename.text
					" * Look for the hl group:       static_filename.text
					" * Look for the hl group: gundo:static_filename
					" * Look for the hl group:       static_filename
					" * Return the segment without highlighting, causing an error in the parser
					let seg_variants = join(segment.variants[0:i], '.')

					let seg_name = i > 0 ? segment.name .'.'. seg_variants : segment.name
					let seg_ns_name = len(segment.ns) > 0 ? segment.ns .':'. seg_name : seg_name

					if has_key(colorscheme, seg_ns_name)
						" We have a namespaced highlight group
						let segment.colors = colorscheme[seg_ns_name]
						break
					elseif has_key(colorscheme, seg_name)
						" We have a non-namespaced group
						let segment.colors = colorscheme[seg_name]
						break
					endif
				endfor
			endif

			unlet! segment
		endfor
	endfor

	" Good luck parsing this return value
	"
	" It's a huge dict with all segments for all buffers with their respective syntax highlighting.
	" It will be parsed by the main Powerline code, where all the data will be shortened to a simple
	" array consiting of a statusline for each mode, with generated highlighting groups and dividers.
	return buffer_segments
endfunction " }}}
