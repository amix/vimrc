let s:tree_up_dir_line = '.. (up a dir)'
syn match NERDTreeIgnore #\~#
syn match NERDTreeIgnore #\[RO\]#

"highlighting for the .. (up dir) line at the top of the tree
execute "syn match NERDTreeUp #\\V". s:tree_up_dir_line ."#"

"quickhelp syntax elements
syn match NERDTreeHelpKey #" \{1,2\}[^ ]*:#ms=s+2,me=e-1
syn match NERDTreeHelpKey #" \{1,2\}[^ ]*,#ms=s+2,me=e-1
syn match NERDTreeHelpTitle #" .*\~#ms=s+2,me=e-1
syn match NERDTreeToggleOn #(on)#ms=s+1,he=e-1
syn match NERDTreeToggleOff #(off)#ms=e-3,me=e-1
syn match NERDTreeHelpCommand #" :.\{-}\>#hs=s+3
syn match NERDTreeHelp  #^".*# contains=NERDTreeHelpKey,NERDTreeHelpTitle,NERDTreeIgnore,NERDTreeToggleOff,NERDTreeToggleOn,NERDTreeHelpCommand

"highlighting for sym links
syn match NERDTreeLinkTarget #->.*# containedin=NERDTreeDir,NERDTreeFile
syn match NERDTreeLinkFile #.* ->#me=e-3 containedin=NERDTreeFile
syn match NERDTreeLinkDir #.*/ ->#me=e-3 containedin=NERDTreeDir

"highlighing for directory nodes and file nodes
syn match NERDTreeDirSlash #/# containedin=NERDTreeDir

if g:NERDTreeDirArrows
    syn match NERDTreeClosable #▾# containedin=NERDTreeDir,NERDTreeFile
    syn match NERDTreeOpenable #▸# containedin=NERDTreeDir,NERDTreeFile

    syn match NERDTreeDir #[^▾▸ ].*/#
    syn match NERDTreeExecFile  #^ .*\*\($\| \)# contains=NERDTreeRO,NERDTreeBookmark
    syn match NERDTreeFile  #^[^"\.▾▸] *[^▾▸]*# contains=NERDTreeLink,NERDTreeRO,NERDTreeBookmark,NERDTreeExecFile

    "highlighting for readonly files
    syn match NERDTreeRO # *\zs.*\ze \[RO\]# contains=NERDTreeIgnore,NERDTreeBookmark,NERDTreeFile

    syn match NERDTreeFlags #^ *\zs\[.\]# containedin=NERDTreeFile
    syn match NERDTreeFlags #\[.\]# containedin=NERDTreeDir
else
    "highlighting for the ~/+ symbols for the directory nodes
    syn match NERDTreeClosable #\~\<#
    syn match NERDTreeClosable #\~\.#
    syn match NERDTreeOpenable #+\<#
    syn match NERDTreeOpenable #+\.#he=e-1

    "highlighting for the tree structural parts
    syn match NERDTreePart #|#
    syn match NERDTreePart #`#
    syn match NERDTreePartFile #[|`]-#hs=s+1 contains=NERDTreePart

    syn match NERDTreeDir #[^-| `].*/# contains=NERDTreeLink,NERDTreeOpenable,NERDTreeClosable
    syn match NERDTreeExecFile  #[|` ].*\*\($\| \)# contains=NERDTreeLink,NERDTreePart,NERDTreePartFile,NERDTreeBookmark
    syn match NERDTreeFile  #|-.*# contains=NERDTreeLink,NERDTreePart,NERDTreePartFile,NERDTreeBookmark,NERDTreeExecFile
    syn match NERDTreeFile  #`-.*# contains=NERDTreeLink,NERDTreePart,NERDTreePartFile,NERDTreeBookmark,NERDTreeExecFile

    "highlighting for readonly files
    syn match NERDTreeRO #|-.*\[RO\]#he=e-5 contains=NERDTreeIgnore,NERDTreeBookmark,NERDTreePart,NERDTreePartFile

    syn match NERDTreeFlags #-\[.\]# containedin=NERDTreeFile,NERDTreePartFile
    syn match NERDTreeFlags #[+~]\zs\[.\]# containedin=NERDTreeDir
endif

syn match NERDTreeCWD #^[</].*$#

"highlighting for bookmarks
syn match NERDTreeBookmark # {.*}#hs=s+1

"highlighting for the bookmarks table
syn match NERDTreeBookmarksLeader #^>#
syn match NERDTreeBookmarksHeader #^>-\+Bookmarks-\+$# contains=NERDTreeBookmarksLeader
syn match NERDTreeBookmarkName #^>.\{-} #he=e-1 contains=NERDTreeBookmarksLeader
syn match NERDTreeBookmark #^>.*$# contains=NERDTreeBookmarksLeader,NERDTreeBookmarkName,NERDTreeBookmarksHeader

hi def link NERDTreePart Special
hi def link NERDTreePartFile Type
hi def link NERDTreeExecFile Title
hi def link NERDTreeDirSlash Identifier

hi def link NERDTreeBookmarksHeader statement
hi def link NERDTreeBookmarksLeader ignore
hi def link NERDTreeBookmarkName Identifier
hi def link NERDTreeBookmark normal

hi def link NERDTreeHelp String
hi def link NERDTreeHelpKey Identifier
hi def link NERDTreeHelpCommand Identifier
hi def link NERDTreeHelpTitle Macro
hi def link NERDTreeToggleOn Question
hi def link NERDTreeToggleOff WarningMsg

hi def link NERDTreeLinkTarget Type
hi def link NERDTreeLinkFile Macro
hi def link NERDTreeLinkDir Macro

hi def link NERDTreeDir Directory
hi def link NERDTreeUp Directory
hi def link NERDTreeFile Normal
hi def link NERDTreeCWD Statement
hi def link NERDTreeOpenable Title
hi def link NERDTreeClosable Title
hi def link NERDTreeIgnore ignore
hi def link NERDTreeRO WarningMsg
hi def link NERDTreeBookmark Statement
hi def link NERDTreeFlags Number

hi def link NERDTreeCurrentNode Search
