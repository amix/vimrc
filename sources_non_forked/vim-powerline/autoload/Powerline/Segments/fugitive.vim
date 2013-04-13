let g:Powerline#Segments#fugitive#segments = Pl#Segment#Init(['fugitive',
	\ (exists('g:loaded_fugitive') && g:loaded_fugitive == 1),
	\
	\ Pl#Segment#Create('branch', '%{Powerline#Functions#fugitive#GetBranch("$BRANCH")}')
\ ])
