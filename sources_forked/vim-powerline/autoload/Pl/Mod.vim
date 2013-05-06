let s:segment_mods = []

function! Pl#Mod#AddSegmentMod(action, properties) " {{{
	call add(s:segment_mods, [a:action, a:properties])
endfunction " }}}
function! Pl#Mod#ApplySegmentMods(theme) " {{{
	let theme = deepcopy(a:theme)

	for mod in s:segment_mods
		let [action, properties] = mod

		" We have to loop through the segments instead of using index() because some
		" segments are lists!
		let target_seg_idx = -1

		for i in range(0, len(theme) - 1)
			unlet! segment
			let segment = theme[i]

			if type(segment) == type(properties.target_segment) && segment == properties.target_segment
				let target_seg_idx = i
				break
			endif
		endfor

		if action == 'insert_segment'
			" Insert segment
			if target_seg_idx != -1
				call insert(theme, properties.new_segment, (properties.where == 'before' ? target_seg_idx : target_seg_idx + 1))
			endif
		elseif action == 'remove_segment'
			" Remove segment
			if target_seg_idx != -1
				call remove(theme, target_seg_idx)
			endif
		endif
	endfor

	return theme
endfunction " }}}
