let g:Powerline#Segments#virtualenv#segments = Pl#Segment#Init(['virtualenv',
	\ has('python') && (exists('g:virtualenv_loaded') && g:virtualenv_loaded == 1),
	\
	\ Pl#Segment#Create('statusline', '%{virtualenv#statusline()}')
\ ])
