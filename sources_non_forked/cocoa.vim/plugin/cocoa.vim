" File: cocoa.vim
" Author: Michael Sanders (msanders42 [at] gmail [dot] com)
if exists('s:did_cocoa') || &cp || version < 700
	finish
endif
let s:did_cocoa = 1

" These have to load after the normal ftplugins to override the defaults; I'd
" like to put this in ftplugin/objc_cocoa_mappings.vim, but that doesn't seem
" to work..
au FileType objc ru after/syntax/objc_enhanced.vim
			\| let b:match_words = '@\(implementation\|interface\):@end'
			\| setl inc=^\s*#\s*import omnifunc=objc#cocoacomplete#Complete
			\| if globpath(expand('<afile>:p:h'), '*.xcodeproj') != '' |
			\  setl makeprg=open\ -a\ xcode\ &&\ osascript\ -e\ 'tell\ app\ \"Xcode\"\ to\ build'
			\| endif
