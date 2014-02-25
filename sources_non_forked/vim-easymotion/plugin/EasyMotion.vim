" EasyMotion - Vim motions on speed!
"
" Author: Kim Silkebækken <kim.silkebaekken+vim@gmail.com>
" Source repository: https://github.com/Lokaltog/vim-easymotion

" Script initialization {{{
	if exists('g:EasyMotion_loaded') || &compatible || version < 702
		finish
	endif

	let g:EasyMotion_loaded = 1
" }}}
" Default configuration {{{
	" Default options {{{
		call EasyMotion#InitOptions({
		\   'leader_key'      : '<Leader><Leader>'
		\ , 'keys'            : 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
		\ , 'do_shade'        : 1
		\ , 'do_mapping'      : 1
		\ , 'grouping'        : 1
		\
		\ , 'hl_group_target' : 'EasyMotionTarget'
		\ , 'hl_group_shade'  : 'EasyMotionShade'
		\ })
	" }}}
	" Default highlighting {{{
		let s:target_hl_defaults = {
		\   'gui'     : ['NONE', '#ff0000' , 'bold']
		\ , 'cterm256': ['NONE', '196'     , 'bold']
		\ , 'cterm'   : ['NONE', 'red'     , 'bold']
		\ }

		let s:shade_hl_defaults = {
		\   'gui'     : ['NONE', '#777777' , 'NONE']
		\ , 'cterm256': ['NONE', '242'     , 'NONE']
		\ , 'cterm'   : ['NONE', 'grey'    , 'NONE']
		\ }

		call EasyMotion#InitHL(g:EasyMotion_hl_group_target, s:target_hl_defaults)
		call EasyMotion#InitHL(g:EasyMotion_hl_group_shade,  s:shade_hl_defaults)

		" Reset highlighting after loading a new color scheme {{{
			augroup EasyMotionInitHL
				autocmd!

				autocmd ColorScheme * call EasyMotion#InitHL(g:EasyMotion_hl_group_target, s:target_hl_defaults)
				autocmd ColorScheme * call EasyMotion#InitHL(g:EasyMotion_hl_group_shade,  s:shade_hl_defaults)
			augroup end
		" }}}
	" }}}
	" Default key mapping {{{
		call EasyMotion#InitMappings({
		\   'f' : { 'name': 'F'  , 'dir': 0 }
		\ , 'F' : { 'name': 'F'  , 'dir': 1 }
		\ , 't' : { 'name': 'T'  , 'dir': 0 }
		\ , 'T' : { 'name': 'T'  , 'dir': 1 }
		\ , 'w' : { 'name': 'WB' , 'dir': 0 }
		\ , 'W' : { 'name': 'WBW', 'dir': 0 }
		\ , 'b' : { 'name': 'WB' , 'dir': 1 }
		\ , 'B' : { 'name': 'WBW', 'dir': 1 }
		\ , 'e' : { 'name': 'E'  , 'dir': 0 }
		\ , 'E' : { 'name': 'EW' , 'dir': 0 }
		\ , 'ge': { 'name': 'E'  , 'dir': 1 }
		\ , 'gE': { 'name': 'EW' , 'dir': 1 }
		\ , 'j' : { 'name': 'JK' , 'dir': 0 }
		\ , 'k' : { 'name': 'JK' , 'dir': 1 }
		\ , 'n' : { 'name': 'Search' , 'dir': 0 }
		\ , 'N' : { 'name': 'Search' , 'dir': 1 }
		\ })
	" }}}
" }}}

" vim: fdm=marker:noet:ts=4:sw=4:sts=4
