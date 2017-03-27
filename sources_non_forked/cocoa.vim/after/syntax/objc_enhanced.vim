" Author:       Michael Sanders (msanders42 [at] gmail [dot] com)
" Description:  Better syntax highlighting for Objective-C files (part of the
"               cocoa.vim plugin).
" Last Updated: June 23, 2009

" NOTE:    This next file (cocoa_keywords.vim) is rather large and may slow
"          things down. Loading it seems to take less than 0.5 microseconds
"          on my machine, but I'm not sure of the consequences; if it is slow
"          for you, just comment out the next line.
ru after/syntax/cocoa_keywords.vim

syn match objcDirective '@synthesize\|@property\|@optional\|@required' display
syn keyword objcType IBOutlet IBAction Method
syn keyword objcConstant YES NO TRUE FALSE

syn region objcImp start='@implementation' end='@end' transparent
syn region objcHeader start='@interface' end='@end' transparent

" I make this typo sometimes so it's nice to have it highlighted.
syn match objcError '\v(NSLogv=\(\s*)@<=[^@]=["'].*'me=e-1

syn match objcSubclass '\(@implementation\|@interface\)\@<=\s*\k\+' display contained containedin=objcImp,objcHeader
syn match objcSuperclass '\(@\(implementation\|interface\)\s*\k\+\s*:\)\@<=\s*\k*' display contained containedin=objcImp,objcHeader

" Matches "- (void) foo: (int) bar and: (float) foobar"
syn match objcMethod '^\s*[-+]\s*\_.\{-}[\{;]'me=e-1 transparent contains=cParen,objcInstMethod,objcFactMethod
" Matches "bar & foobar" in above
syn match objcMethodArg ')\@<=\s*\k\+' contained containedin=objcMethod
" Matches "foo:" & "and:" in above
syn match objcMethodName '\(^\s*[-+]\s*(\_[^)]*)\)\@<=\_\s*\_\k\+' contained containedin=objcMethod
syn match objcMethodColon '\k\+\s*:' contained containedin=objcMethod
" Don't match these groups in cParen "(...)"
syn cluster cParenGroup add=objcMethodName,objcMethodArg,objcMethodColon
" This fixes a bug with completion inside parens (e.g. if ([NSString ]))
syn cluster cParenGroup remove=objcMethodCall

" Matches "bar" in "[NSObject bar]" or "bar" in "[[NSObject foo: baz] bar]",
" but NOT "bar" in "[NSObject foo: bar]".
syn match objcMessageName '\(\[\s*\k\+\s\+\|\]\s*\)\@<=\k*\s*\]'me=e-1 display contained containedin=objcMethodCall
" Matches "foo:" in "[NSObject foo: bar]" or "[[NSObject new] foo: bar]"
syn match objcMessageColon '\(\_\S\+\_\s\+\)\@<=\k\+\s*:' display contained containedin=objcMethodCall

" Don't match these in this strange group for edge cases...
syn cluster cMultiGroup add=objcMessageColon,objcMessageName,objcMethodName,objcMethodArg,objcMethodColon

" You may want to customize this one. I couldn't find a default group to suit
" it, but you can modify your colorscheme to make this a different color.
hi link objcMethodName Special
hi link objcMethodColon objcMethodName

hi link objcMethodArg Identifier

hi link objcMessageName objcMethodArg
hi link objcMessageColon objcMessageName

hi link objcSubclass objcMethodName
hi link objcSuperclass String

hi link objcError Error
