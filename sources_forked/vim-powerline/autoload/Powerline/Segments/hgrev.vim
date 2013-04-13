let g:Powerline#Segments#hgrev#segments = Pl#Segment#Init(['hgrev',
	\ (exists('hgrev_loaded')),
	\ Pl#Segment#Create('branch', '%{Powerline#Functions#hgrev#Status("$BRANCH")}')
  \ ])
