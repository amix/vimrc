" Powerline - The ultimate statusline utility
"
" Author: Kim Silkebækken <kim.silkebaekken+vim@gmail.com>
" Source repository: https://github.com/Lokaltog/vim-powerline

" Script variables {{{
	let g:Pl#OLD_STL = ''
	let g:Pl#THEME = []
	let g:Pl#THEME_CALLBACKS = []
	let g:Pl#HL = []

	" Cache revision, this must be incremented whenever the cache format is changed
	let s:CACHE_REVISION = 7
" }}}
" Script initialization {{{
	function! Pl#LoadCache() " {{{
		if filereadable(g:Powerline_cache_file) && g:Powerline_cache_enabled
			exec 'source' escape(g:Powerline_cache_file, ' \')

			if ! exists('g:Powerline_cache_revision') || g:Powerline_cache_revision != s:CACHE_REVISION
				" Cache revision differs, cache is invalid
				unlet! g:Powerline_cache_revision

				return 0
			endif

			" Create highlighting groups
			for hi_cmd in g:Pl#HL
				exec hi_cmd
			endfor

			" Run theme callbacks
			for callback in g:Pl#THEME_CALLBACKS
				" Substitute {{NEWLINE}} with newlines (strings must be
				" stored without newlines characters to avoid vim errors)
				exec substitute(callback[0], "{{NEWLINE}}", "\n", 'g')
				exec substitute(callback[1], "{{NEWLINE}}", "\n", 'g')
			endfor

			return 1
		endif

		return 0
	endfunction " }}}
	function! Pl#ClearCache() " {{{
		if filereadable(g:Powerline_cache_file)
			" Delete the cache file
			call delete(g:Powerline_cache_file)
		endif

		echo 'Powerline cache cleared. Please restart vim for the changes to take effect.'
	endfunction " }}}
	function! Pl#ReloadColorscheme() " {{{
		call Pl#ClearCache()

		" The colorscheme and theme files must be manually sourced because
		" vim won't reload previously autoloaded files
		"
		" This is a bit hackish, but it works
		unlet! g:Powerline#Colorschemes#{g:Powerline_colorscheme}#colorscheme
		exec "source" split(globpath(&rtp, 'autoload/Powerline/Colorschemes/'. g:Powerline_colorscheme .'.vim', 1), '\n')[0]

		unlet! g:Powerline#Themes#{g:Powerline_theme}#theme
		exec "source" split(globpath(&rtp, 'autoload/Powerline/Themes/'. g:Powerline_theme .'.vim', 1), '\n')[0]

		let g:Pl#THEME = []

		call Pl#Load()
	endfunction " }}}
	function! Pl#Load() " {{{
		if empty(g:Pl#OLD_STL)
			" Store old statusline
			let g:Pl#OLD_STL = &statusline
		endif

		if ! Pl#LoadCache()
			try
				" Autoload the theme dict first
				let raw_theme = g:Powerline#Themes#{g:Powerline_theme}#theme
			catch
				echom 'Invalid Powerline theme! Please check your theme and colorscheme settings.'

				return
			endtry

			" Create list with parsed statuslines
			for buffer_statusline in raw_theme
				unlet! mode_statuslines
				let mode_statuslines = Pl#Parser#GetStatusline(buffer_statusline.segments)

				if ! empty(buffer_statusline.callback)
					" The callback function passes its arguments on to
					" Pl#StatuslineCallback along with the normal/current mode
					" statusline.
					let s:cb_func  = "function! PowerlineStatuslineCallback_". buffer_statusline.callback[1] ."(...)\n"
					let s:cb_func .= "return Pl#StatuslineCallback(". string(mode_statuslines['n']) .", a:000)\n"
					let s:cb_func .= "endfunction"

					" The callback expression should be used to initialize any
					" variables that will use the callback function. The
					" expression requires a %s which will be replaced by the
					" callback function name.
					let s:cb_expr  = printf(buffer_statusline.callback[2], 'PowerlineStatuslineCallback_'. buffer_statusline.callback[1])

					exec s:cb_func
					exec s:cb_expr

					" Newlines must be substituted with another character
					" because vim doesn't like newlines in strings
					call add(g:Pl#THEME_CALLBACKS, [substitute(s:cb_func, "\n", "{{NEWLINE}}", 'g'), substitute(s:cb_expr, "\n", "{{NEWLINE}}", 'g')])

					unlet! s:cb_func s:cb_expr

					continue
				endif

				" Store the statuslines for matching specific buffers
				call add(g:Pl#THEME, {
					\ 'matches': buffer_statusline.matches,
					\ 'mode_statuslines': mode_statuslines
					\ })
			endfor

			if ! g:Powerline_cache_enabled
				" Don't cache anything if caching is disabled or cache file isn't writeable
				return
			endif

			" Prepare commands and statuslines for caching
			let cache = [
				\ 'let g:Powerline_cache_revision = '. string(s:CACHE_REVISION),
				\ 'let g:Pl#HL = '. string(g:Pl#HL),
				\ 'let g:Pl#THEME  = '. string(g:Pl#THEME),
				\ 'let g:Pl#THEME_CALLBACKS  = '. string(g:Pl#THEME_CALLBACKS),
			\ ]

			call writefile(cache, g:Powerline_cache_file)
		endif
	endfunction " }}}
" }}}
" Statusline updater {{{
	function! Pl#Statusline(statusline, current) " {{{
		let mode = mode()

		if ! a:current
			let mode = 'N' " Normal (non-current)
		elseif mode =~# '\v(v|V|)'
			let mode = 'v' " Visual mode
		elseif mode =~# '\v(s|S|)'
			let mode = 's' " Select mode
		elseif mode =~# '\vi'
			let mode = 'i' " Insert mode
		elseif mode =~# '\v(R|Rv)'
			let mode = 'r' " Replace mode
		else
			" Fallback to normal mode
			let mode = 'n' " Normal (current)
		endif

		return g:Pl#THEME[a:statusline].mode_statuslines[mode]
	endfunction " }}}
	function! Pl#StatuslineCallback(statusline, args) " {{{
		" Replace %1, %2, etc. in the statusline with the callback args
		return substitute(
			\ a:statusline,
			\ '\v\%(\d+)',
			\ '\=a:args[submatch(1)]',
			\ 'g')
	endfunction " }}}
	function! Pl#UpdateStatusline(current) " {{{
		if empty(g:Pl#THEME)
			" Load statuslines if they aren't loaded yet
			call Pl#Load()
		endif

		for i in range(0, len(g:Pl#THEME) - 1)
			if Pl#Match#Validate(g:Pl#THEME[i])
				" Update window-local statusline
				let &l:statusline = '%!Pl#Statusline('. i .','. a:current .')'
			endif
		endfor
	endfunction " }}}
" }}}
