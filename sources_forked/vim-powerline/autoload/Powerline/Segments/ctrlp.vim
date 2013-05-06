if !exists("g:Powerline#Segments#ctrlp#segments#focus ")
	let g:Powerline#Segments#ctrlp#segments#focus = '%{"%0"}'
endif
if !exists("g:Powerline#Segments#ctrlp#segments#prev ")
	let g:Powerline#Segments#ctrlp#segments#prev = '%-3{"%3"}'
endif
if !exists("g:Powerline#Segments#ctrlp#segments#next ")
	let g:Powerline#Segments#ctrlp#segments#next = '%-3{"%5"}'
endif

let g:Powerline#Segments#ctrlp#segments = Pl#Segment#Init(['ctrlp'
	\ , Pl#Segment#Create('focus', g:Powerline#Segments#ctrlp#segments#focus)
	\ , Pl#Segment#Create('byfname', '%{"%1"}')
	\ , Pl#Segment#Create('prev', g:Powerline#Segments#ctrlp#segments#prev)
	\ , Pl#Segment#Create('item', '%-9{"%4"}')
	\ , Pl#Segment#Create('next', g:Powerline#Segments#ctrlp#segments#next)
	\ , Pl#Segment#Create('marked', '%{"%6" == " <+>" ? "" : strpart("%6", 2, len("%6") - 3)}')
	\
	\ , Pl#Segment#Create('count', '%-6{"%0"}')
\ ])
