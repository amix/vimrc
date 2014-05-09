" Helper function for (x)html snippets
if exists('s:did_snip_helper') || &cp || !exists('loaded_snips')
	finish
endif
let s:did_snip_helper = 1

" Automatically closes tag if in xhtml
fun! Close()
	return stridx(&ft, 'xhtml') == -1 ? '' : ' /'
endf
