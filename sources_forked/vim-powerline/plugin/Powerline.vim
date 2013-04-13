" Powerline - The ultimate statusline utility
"
" Author: Kim Silkebækken <kim.silkebaekken+vim@gmail.com>
" Source repository: https://github.com/Lokaltog/vim-powerline

" Script initialization {{{
	if exists('g:Powerline_loaded') || &compatible || version < 702
		finish
	endif

	let g:Powerline_loaded = 1
" }}}
" Commands {{{
	command! PowerlineClearCache call Pl#ClearCache()
	command! PowerlineReloadColorscheme call Pl#ReloadColorscheme()
" }}}
" Set default options {{{
	for [s:key, s:value] in items({
		\   'theme'            : 'default'
		\ , 'colorscheme'      : 'default'
		\ , 'symbols'          : 'compatible'
		\ , 'symbols_override' : {}
		\ , 'dividers_override': []
		\ , 'stl_path_style'   : 'relative'
		\ , 'cache_enabled'    : 1
		\ })

		if ! exists('g:Powerline_' . s:key)
			exec printf('let g:Powerline_%s = %s', s:key, string(s:value))
		endif

		unlet! s:key s:value
	endfor

	if ! exists('g:Powerline_cache_file')
		exec 'let g:Powerline_cache_file = '. string(printf('%s/Powerline_%s_%s_%s.cache'
			\ , simplify(expand('<sfile>:p:h') .'/..')
			\ , g:Powerline_theme
			\ , g:Powerline_colorscheme
			\ , g:Powerline_symbols
			\ ))
	endif
" }}}
" Autocommands {{{
	function! s:CreateAutocmds()
		augroup PowerlineMain
			autocmd!

			" Reload statuslines when changing color scheme
			autocmd ColorScheme *
				\ call Pl#Load()

			autocmd BufEnter,WinEnter,FileType,BufUnload *
				\ call Pl#UpdateStatusline(1)

			autocmd BufLeave,WinLeave *
				\ call Pl#UpdateStatusline(0)

			autocmd BufWritePost */autoload/Powerline/Colorschemes/*.vim
				\ :PowerlineReloadColorscheme
		augroup END
	endfunction

	augroup PowerlineStartup
		autocmd!

		autocmd VimEnter * call s:CreateAutocmds() | call Pl#UpdateStatusline(1)
	augroup END
" }}}
