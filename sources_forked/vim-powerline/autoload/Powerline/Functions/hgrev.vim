function! Powerline#Functions#hgrev#Status(symbol) " {{{
	if ! exists('*HGRev')
		" HGRev hasn't been loaded yet
		return ''
	endif
	if !exists("b:statusline_hg_status")
		silent execute "RefreshMercurialRev"
	endif
	let b:statusline_hg_status=HGRev()
	if b:statusline_hg_status != '-'
		let ret = "\u26A1". '' . substitute(b:statusline_hg_status, '^[^ ]*', '\1', 'g')
		let ret=substitute(ret,' M$','+','g')
	else
		let ret=''
		endif
	return ret
endfunction " }}}
