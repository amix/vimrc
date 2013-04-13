let g:Powerline#Themes#default#theme = Pl#Theme#Create(
	\ Pl#Theme#Buffer(''
		\ , 'paste_indicator'
		\ , 'mode_indicator'
		\ , 'fugitive:branch'
		\ , 'hgrev:branch'
		\ , 'fileinfo'
		\ , 'syntastic:errors'
		\ , Pl#Segment#Truncate()
		\ , 'tagbar:currenttag'
		\ , Pl#Segment#Split()
		\ , 'rvm:string'
		\ , 'virtualenv:statusline'
		\ , 'fileformat'
		\ , 'fileencoding'
		\ , 'filetype'
		\ , 'scrollpercent'
		\ , 'lineinfo'
	\ ),
	\
	\ Pl#Theme#Buffer('command_t'
		\ , ['static_str.name', 'Command-T']
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
		\ , ['raw.line', '%10(Match #%l%)']
	\ ),
	\
	\ Pl#Theme#Buffer('gundo', Pl#Match#Any('gundo_tree')
		\ , ['static_str.name', 'Gundo']
		\ , ['static_str.buffer', 'Undo tree']
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
	\ ),
	\
	\ Pl#Theme#Buffer('gundo', Pl#Match#Any('gundo_preview')
		\ , ['static_str.name', 'Gundo']
		\ , ['static_str.buffer', 'Diff preview']
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
	\ ),
	\
	\ Pl#Theme#Buffer('bt_help'
		\ , ['static_str.name', 'Help']
		\ , 'filename'
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
		\ , 'scrollpercent'
	\ ),
	\
	\ Pl#Theme#Buffer('ft_vimpager'
		\ , ['static_str.name', 'Pager']
		\ , 'filename'
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
		\ , 'scrollpercent'
	\ ),
	\
	\ Pl#Theme#Buffer('lustyexplorer'
		\ , ['static_str.name', 'LustyExplorer']
		\ , ['static_str.buffer', 'Buffer list']
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
	\ ),
	\
	\ Pl#Theme#Buffer('ft_man'
		\ , ['static_str.name', 'Man page']
		\ , 'filename'
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
		\ , 'scrollpercent'
	\ ),
	\
	\ Pl#Theme#Buffer('minibufexplorer'
		\ , ['static_str.name', 'MiniBufExplorer']
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
	\ ),
	\
	\ Pl#Theme#Buffer('ft_qf'
		\ , ['static_str.name', 'Quickfix']
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
	\ ),
	\
	\ Pl#Theme#Buffer('tagbar'
		\ , ['static_str.name', 'Tagbar']
		\ , ['static_str.buffer', 'Tree']
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
	\ ),
	\
	\ Pl#Theme#Buffer('ctrlp', Pl#Theme#Callback('ctrlp_main', 'if ! exists("g:ctrlp_status_func") | let g:ctrlp_status_func = {} | endif | let g:ctrlp_status_func.main = "%s"')
		\ , 'ctrlp:prev'
		\ , 'ctrlp:item'
		\ , 'ctrlp:next'
		\ , 'ctrlp:marked'
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
		\ , 'ctrlp:focus'
		\ , 'ctrlp:byfname'
		\ , 'pwd'
	\ ),
	\
	\ Pl#Theme#Buffer('ctrlp', Pl#Theme#Callback('ctrlp_prog', 'if ! exists("g:ctrlp_status_func") | let g:ctrlp_status_func = {} | endif | let g:ctrlp_status_func.prog = "%s"')
		\ , 'ctrlp:count'
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
		\ , 'pwd'
	\ ),
	\
	\ Pl#Theme#Buffer('nerdtree'
		\ , ['raw.name', '%{Powerline#Functions#GetShortPath(4)}']
		\ , Pl#Segment#Truncate()
		\ , Pl#Segment#Split()
	\ )
\ )
