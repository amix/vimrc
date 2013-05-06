let g:Powerline#Segments#rvm#segments = Pl#Segment#Init(['rvm',
	\ (exists('g:loaded_rvm') && g:loaded_rvm == 1),
	\
	\ Pl#Segment#Create('string', '%{rvm#string()}'),
	\ Pl#Segment#Create('statusline', '%{rvm#statusline()}')
\ ])
