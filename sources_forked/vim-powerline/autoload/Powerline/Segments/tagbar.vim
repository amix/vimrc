let g:Powerline#Segments#tagbar#segments = Pl#Segment#Init(['tagbar',
	\ (exists(':Tagbar') > 0),
	\
	\ Pl#Segment#Create('currenttag', '%{tagbar#currenttag("%s", "")}', Pl#Segment#Modes('!N')),
	\ Pl#Segment#Create('fullcurrenttag', '%{tagbar#currenttag("%s", "", "f")}', Pl#Segment#Modes('!N'))
\ ])
