" vim:tabstop=2:shiftwidth=2:expandtab:foldmethod=marker:textwidth=79
" Vimwiki syntax file
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/

" Quit if syntax file is already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Links highlighting is controlled by vimwiki#base#highlight_links() function.
" It is called from setup_buffer_enter() function in the BufEnter autocommand.

" Load concrete Wiki syntax
execute 'runtime! syntax/vimwiki_'.VimwikiGet('syntax').'.vim'

" Concealed chars
if exists("+conceallevel")
  syntax conceal on
endif

syntax spell toplevel

syn match VimwikiLinkChar contained /\[\[/
syn match VimwikiLinkChar contained /\]\]/
syn match VimwikiLinkChar contained /\[\[[^\[\]\|]\{-}|\ze.\{-}]]/
syn match VimwikiLinkChar contained /\[\[[^\[\]\|]\{-}]\[\ze.\{-}]]/

syn match VimwikiNoLinkChar contained /\[\[/
syn match VimwikiNoLinkChar contained /\]\]/
syn match VimwikiNoLinkChar contained /\[\[[^\[\]\|]\{-}|\ze.*]]/
syn match VimwikiNoLinkChar contained /\[\[[^\[\]\|]\{-}]\[\ze.*]]/

execute 'syn match VimwikiBoldChar contained /'.g:vimwiki_char_bold.'/'
execute 'syn match VimwikiItalicChar contained /'.g:vimwiki_char_italic.'/'
execute 'syn match VimwikiBoldItalicChar contained /'.g:vimwiki_char_bolditalic.'/'
execute 'syn match VimwikiItalicBoldChar contained /'.g:vimwiki_char_italicbold.'/'
execute 'syn match VimwikiCodeChar contained /'.g:vimwiki_char_code.'/'
execute 'syn match VimwikiDelTextChar contained /'.g:vimwiki_char_deltext.'/'
execute 'syn match VimwikiSuperScript contained /'.g:vimwiki_char_superscript.'/'
execute 'syn match VimwikiSubScript contained /'.g:vimwiki_char_subscript.'/'
if exists("+conceallevel")
  syntax conceal off
endif

" Non concealed chars
syn match VimwikiHeaderChar contained /\%(^\s*=\+\)\|\%(=\+\s*$\)/
execute 'syn match VimwikiBoldCharT contained /'.g:vimwiki_char_bold.'/'
execute 'syn match VimwikiItalicCharT contained /'.g:vimwiki_char_italic.'/'
execute 'syn match VimwikiBoldItalicCharT contained /'.g:vimwiki_char_bolditalic.'/'
execute 'syn match VimwikiItalicBoldCharT contained /'.g:vimwiki_char_italicbold.'/'
execute 'syn match VimwikiCodeCharT contained /'.g:vimwiki_char_code.'/'
execute 'syn match VimwikiDelTextCharT contained /'.g:vimwiki_char_deltext.'/'
execute 'syn match VimwikiSuperScriptT contained /'.g:vimwiki_char_superscript.'/'
execute 'syn match VimwikiSubScriptT contained /'.g:vimwiki_char_subscript.'/'


" Emoticons
syntax match VimwikiEmoticons /\%((.)\|:[()|$@]\|:-[DOPS()\]|$@]\|;)\|:'(\)/

let g:vimwiki_rxTodo = '\C\%(TODO:\|DONE:\|STARTED:\|FIXME:\|FIXED:\|XXX:\)'
execute 'syntax match VimwikiTodo /'. g:vimwiki_rxTodo .'/'


" Tables
syntax match VimwikiTableRow /^\s*|.\+|\s*$/ 
      \ transparent contains=VimwikiCellSeparator,VimwikiLinkT,
      \ VimwikiNoExistsLinkT,VimwikiEmoticons,VimwikiTodo,
      \ VimwikiBoldT,VimwikiItalicT,VimwikiBoldItalicT,VimwikiItalicBoldT,
      \ VimwikiDelTextT,VimwikiSuperScriptT,VimwikiSubScriptT,VimwikiCodeT,
      \ @Spell
syntax match VimwikiCellSeparator 
      \ /\%(|\)\|\%(-\@<=+\-\@=\)\|\%([|+]\@<=-\+\)/ contained

" List items
execute 'syntax match VimwikiList /'.g:vimwiki_rxListBullet.'/'
execute 'syntax match VimwikiList /'.g:vimwiki_rxListNumber.'/'
execute 'syntax match VimwikiList /'.g:vimwiki_rxListDefine.'/'

execute 'syntax match VimwikiBold /'.g:vimwiki_rxBold.'/ contains=VimwikiBoldChar,@Spell'
execute 'syntax match VimwikiBoldT /'.g:vimwiki_rxBold.'/ contained contains=VimwikiBoldCharT,@Spell'

execute 'syntax match VimwikiItalic /'.g:vimwiki_rxItalic.'/ contains=VimwikiItalicChar,@Spell'
execute 'syntax match VimwikiItalicT /'.g:vimwiki_rxItalic.'/ contained contains=VimwikiItalicCharT,@Spell'

execute 'syntax match VimwikiBoldItalic /'.g:vimwiki_rxBoldItalic.'/ contains=VimwikiBoldItalicChar,VimwikiItalicBoldChar,@Spell'
execute 'syntax match VimwikiBoldItalicT /'.g:vimwiki_rxBoldItalic.'/ contained contains=VimwikiBoldItalicChatT,VimwikiItalicBoldCharT,@Spell'

execute 'syntax match VimwikiItalicBold /'.g:vimwiki_rxItalicBold.'/ contains=VimwikiBoldItalicChar,VimwikiItalicBoldChar,@Spell'
execute 'syntax match VimwikiItalicBoldT /'.g:vimwiki_rxItalicBold.'/ contained contains=VimwikiBoldItalicCharT,VimsikiItalicBoldCharT,@Spell'

execute 'syntax match VimwikiDelText /'.g:vimwiki_rxDelText.'/ contains=VimwikiDelTextChar,@Spell'
execute 'syntax match VimwikiDelTextT /'.g:vimwiki_rxDelText.'/ contained contains=VimwikiDelTextChar,@Spell'

execute 'syntax match VimwikiSuperScript /'.g:vimwiki_rxSuperScript.'/ contains=VimwikiSuperScriptChar,@Spell'
execute 'syntax match VimwikiSuperScriptT /'.g:vimwiki_rxSuperScript.'/ contained contains=VimwikiSuperScriptCharT,@Spell'

execute 'syntax match VimwikiSubScript /'.g:vimwiki_rxSubScript.'/ contains=VimwikiSubScriptChar,@Spell'
execute 'syntax match VimwikiSubScriptT /'.g:vimwiki_rxSubScript.'/ contained contains=VimwikiSubScriptCharT,@Spell'

execute 'syntax match VimwikiCode /'.g:vimwiki_rxCode.'/ contains=VimwikiCodeChar'
execute 'syntax match VimwikiCodeT /'.g:vimwiki_rxCode.'/ contained contains=VimwikiCodeCharT'


" <hr> horizontal rule
execute 'syntax match VimwikiHR /'.g:vimwiki_rxHR.'/'

execute 'syntax region VimwikiPre start=/^\s*'.g:vimwiki_rxPreStart.
      \ '/ end=/^\s*'.g:vimwiki_rxPreEnd.'\s*$/ contains=@Spell'

" List item checkbox
syntax match VimwikiCheckBox /\[.\?\]/
if g:vimwiki_hl_cb_checked
  execute 'syntax match VimwikiCheckBoxDone /'.
        \ g:vimwiki_rxListBullet.'\s*\['.g:vimwiki_listsyms[4].'\].*$/'.
        \ ' contains=VimwikiNoExistsLink,VimwikiLink'
  execute 'syntax match VimwikiCheckBoxDone /'.
        \ g:vimwiki_rxListNumber.'\s*\['.g:vimwiki_listsyms[4].'\].*$/'.
        \ ' contains=VimwikiNoExistsLink,VimwikiLink'
endif

" placeholders
syntax match VimwikiPlaceholder /^\s*%toc\%(\s.*\)\?$/ contains=VimwikiPlaceholderParam
syntax match VimwikiPlaceholder /^\s*%nohtml\s*$/
syntax match VimwikiPlaceholder /^\s*%title\%(\s.*\)\?$/ contains=VimwikiPlaceholderParam
syntax match VimwikiPlaceholder /^\s*%template\%(\s.*\)\?$/ contains=VimwikiPlaceholderParam
syntax match VimwikiPlaceholderParam /\s.*/ contained

" html tags
let html_tags = join(split(g:vimwiki_valid_html_tags, '\s*,\s*'), '\|')
exe 'syntax match VimwikiHTMLtag #\c</\?\%('.html_tags.'\)\%(\s\{-1}\S\{-}\)\{-}\s*/\?>#'
execute 'syntax match VimwikiBold #\c<b>.\{-}</b># contains=VimwikiHTMLTag'
execute 'syntax match VimwikiItalic #\c<i>.\{-}</i># contains=VimwikiHTMLTag'
execute 'syntax match VimwikiUnderline #\c<u>.\{-}</u># contains=VimwikiHTMLTag'

execute 'syntax match VimwikiComment /'.g:vimwiki_rxComment.'/ contains=@Spell'

" Header levels, 1-6
execute 'syntax match VimwikiHeader1 /'.g:vimwiki_rxH1.'/ contains=VimwikiTodo,VimwikiHeaderChar,VimwikiNoExistsLink,VimwikiLink,@Spell'
execute 'syntax match VimwikiHeader2 /'.g:vimwiki_rxH2.'/ contains=VimwikiTodo,VimwikiHeaderChar,VimwikiNoExistsLink,VimwikiLink,@Spell'
execute 'syntax match VimwikiHeader3 /'.g:vimwiki_rxH3.'/ contains=VimwikiTodo,VimwikiHeaderChar,VimwikiNoExistsLink,VimwikiLink,@Spell'
execute 'syntax match VimwikiHeader4 /'.g:vimwiki_rxH4.'/ contains=VimwikiTodo,VimwikiHeaderChar,VimwikiNoExistsLink,VimwikiLink,@Spell'
execute 'syntax match VimwikiHeader5 /'.g:vimwiki_rxH5.'/ contains=VimwikiTodo,VimwikiHeaderChar,VimwikiNoExistsLink,VimwikiLink,@Spell'
execute 'syntax match VimwikiHeader6 /'.g:vimwiki_rxH6.'/ contains=VimwikiTodo,VimwikiHeaderChar,VimwikiNoExistsLink,VimwikiLink,@Spell'

" group names "{{{

if g:vimwiki_hl_headers == 0
  hi link VimwikiHeader1 Title
  hi link VimwikiHeader2 Title
  hi link VimwikiHeader3 Title
  hi link VimwikiHeader4 Title
  hi link VimwikiHeader5 Title
  hi link VimwikiHeader6 Title
else
  if &background == 'light'
    hi def VimwikiHeader1 guibg=bg guifg=#aa5858 gui=bold ctermfg=DarkRed   term=bold cterm=bold 
    hi def VimwikiHeader2 guibg=bg guifg=#507030 gui=bold ctermfg=DarkGreen term=bold cterm=bold 
    hi def VimwikiHeader3 guibg=bg guifg=#1030a0 gui=bold ctermfg=DarkBlue  term=bold cterm=bold 
    hi def VimwikiHeader4 guibg=bg guifg=#103040 gui=bold ctermfg=Black     term=bold cterm=bold 
    hi def VimwikiHeader5 guibg=bg guifg=#505050 gui=bold ctermfg=Black     term=bold cterm=bold 
    hi def VimwikiHeader6 guibg=bg guifg=#636363 gui=bold ctermfg=Black     term=bold cterm=bold 
  else                                                                                           
    hi def VimwikiHeader1 guibg=bg guifg=#e08090 gui=bold ctermfg=Red       term=bold cterm=bold 
    hi def VimwikiHeader2 guibg=bg guifg=#80e090 gui=bold ctermfg=Green     term=bold cterm=bold 
    hi def VimwikiHeader3 guibg=bg guifg=#6090e0 gui=bold ctermfg=Blue      term=bold cterm=bold 
    hi def VimwikiHeader4 guibg=bg guifg=#c0c0f0 gui=bold ctermfg=White     term=bold cterm=bold 
    hi def VimwikiHeader5 guibg=bg guifg=#e0e0f0 gui=bold ctermfg=White     term=bold cterm=bold 
    hi def VimwikiHeader6 guibg=bg guifg=#f0f0f0 gui=bold ctermfg=White     term=bold cterm=bold 
  endif
endif

hi def link VimwikiMarkers Normal

hi def VimwikiBold term=bold cterm=bold gui=bold
hi def link VimwikiBoldT VimwikiBold

hi def VimwikiItalic term=italic cterm=italic gui=italic
hi def link VimwikiItalicT VimwikiItalic

hi def VimwikiBoldItalic term=bold cterm=bold gui=bold,italic
hi def link VimwikiItalicBold VimwikiBoldItalic
hi def link VimwikiBoldItalicT VimwikiBoldItalic
hi def link VimwikiItalicBoldT VimwikiBoldItalic

hi def VimwikiUnderline gui=underline

hi def link VimwikiCode PreProc
hi def link VimwikiCodeT VimwikiCode

hi def link VimwikiPre PreProc
hi def link VimwikiPreT VimwikiPre

hi def link VimwikiNoExistsLink SpellBad
hi def link VimwikiNoExistsLinkT VimwikiNoExistsLink

hi def link VimwikiLink Underlined
hi def link VimwikiLinkT VimwikiLink

hi def link VimwikiList Identifier
hi def link VimwikiCheckBox VimwikiList
hi def link VimwikiCheckBoxDone Comment
hi def link VimwikiEmoticons Character

hi def link VimwikiDelText Constant
hi def link VimwikiDelTextT VimwikiDelText

hi def link VimwikiSuperScript Number
hi def link VimwikiSuperScriptT VimwikiSuperScript

hi def link VimwikiSubScript Number
hi def link VimwikiSubScriptT VimwikiSubScript

hi def link VimwikiTodo Todo
hi def link VimwikiComment Comment

hi def link VimwikiPlaceholder SpecialKey
hi def link VimwikiPlaceholderParam String
hi def link VimwikiHTMLtag SpecialKey

hi def link VimwikiCellSeparator VimwikiMarkers
hi def link VimwikiBoldChar VimwikiMarkers
hi def link VimwikiItalicChar VimwikiMarkers
hi def link VimwikiBoldItalicChar VimwikiMarkers
hi def link VimwikiItalicBoldChar VimwikiMarkers
hi def link VimwikiDelTextChar VimwikiMarkers
hi def link VimwikiSuperScriptChar VimwikiMarkers
hi def link VimwikiSubScriptChar VimwikiMarkers
hi def link VimwikiCodeChar VimwikiMarkers
hi def link VimwikiHeaderChar VimwikiMarkers
hi def link VimwikiLinkChar VimwikiLink
hi def link VimwikiNoLinkChar VimwikiNoExistsLink

hi def link VimwikiBoldCharT VimwikiMarkers
hi def link VimwikiItalicCharT VimwikiMarkers
hi def link VimwikiBoldItalicCharT VimwikiMarkers
hi def link VimwikiItalicBoldCharT VimwikiMarkers
hi def link VimwikiDelTextCharT VimwikiMarkers
hi def link VimwikiSuperScriptCharT VimwikiMarkers
hi def link VimwikiSubScriptCharT VimwikiMarkers
hi def link VimwikiCodeCharT VimwikiMarkers
hi def link VimwikiHeaderCharT VimwikiMarkers
hi def link VimwikiLinkCharT VimwikiLinkT
hi def link VimwikiNoLinkCharT VimwikiNoExistsLinkT
"}}}

let b:current_syntax="vimwiki"

" EMBEDDED syntax setup "{{{
let nested = VimwikiGet('nested_syntaxes')
if !empty(nested)
  for [hl_syntax, vim_syntax] in items(nested)
    call vimwiki#base#nested_syntax(vim_syntax, 
          \ '^\s*{{{\%(.*[[:blank:][:punct:]]\)\?'.
          \ hl_syntax.'\%([[:blank:][:punct:]].*\)\?',
          \ '^\s*}}}', 'VimwikiPre')
  endfor
endif
"}}}
