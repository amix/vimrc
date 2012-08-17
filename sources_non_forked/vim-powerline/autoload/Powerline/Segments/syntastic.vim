let g:Powerline#Segments#syntastic#segments = Pl#Segment#Init(['syntastic',
	\ (exists('g:loaded_syntastic_plugin') && g:loaded_syntastic_plugin == 1),
	\
	\ Pl#Segment#Create('errors', '%{Powerline#Functions#syntastic#GetErrors("$LINE")}', Pl#Segment#Modes('!N'))
\ ])
