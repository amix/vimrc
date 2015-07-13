if exists("g:go_loaded_gosnippets")
  finish
endif
let g:go_loaded_gosnippets = 1

" by default UltiSnips
if !exists("g:go_snippet_engine")
	let g:go_snippet_engine = "ultisnips"
endif

function! s:GoUltiSnips()
	if globpath(&rtp, 'plugin/UltiSnips.vim') == ""
		return
	endif

	if !exists("g:UltiSnipsSnippetDirectories")
			let g:UltiSnipsSnippetDirectories = ["gosnippets/UltiSnips"]
	else
			let g:UltiSnipsSnippetDirectories += ["gosnippets/UltiSnips"]
	endif
endfunction

function! s:GoNeosnippet()
	if globpath(&rtp, 'plugin/neosnippet.vim') == ""
		return
	endif

	let g:neosnippet#enable_snipmate_compatibility = 1

	let gosnippets_dir = globpath(&rtp, 'gosnippets/snippets')
	if type(g:neosnippet#snippets_directory) == type([])
		let g:neosnippet#snippets_directory += [gosnippets_dir]
	elseif type(g:neosnippet#snippets_directory) == type("")
		if strlen(g:neosnippet#snippets_directory) > 0
			let g:neosnippet#snippets_directory = g:neosnippet#snippets_directory . "," . gosnippets_dir
		else
			let g:neosnippet#snippets_directory = gosnippets_dir
		endif
	endif
endfunction

if g:go_snippet_engine == "ultisnips"
	call s:GoUltiSnips()
elseif g:go_snippet_engine == "neosnippet"
	call s:GoNeosnippet()
endif
