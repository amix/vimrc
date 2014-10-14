"==============================================================================
"File:        zenroom2.vim
"Description: Emulates iA Writer environment when editing Markdown, reStructuredText
"             or text files.
"Maintainer:  Amir Salihefendic <amix@doist.io>
"Version:     0.1
"Last Change: 2013-12-29
"License:     BSD
"==============================================================================

if exists( "g:loaded_zenroom2_plugin" )
    finish
endif
let g:loaded_zenroom2_plugin = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin Configuration
"
" Save the current `background` value for reset later
let s:save_background = ""
if exists( "&background" )
    let s:save_background = &background
endif

function! s:markdown_room()
    set background=light
    set linespace=8

    hi Normal guibg=gray95
    hi NonText guifg=gray95
    hi FoldColumn guibg=gray95
    hi CursorLine guibg=gray90
    hi Title gui=bold guifg=gray25
    hi MarkdownHeadingDelimiter gui=bold guifg=gray25
    hi htmlSpecialChar guifg=black
    hi markdownError guifg=black
    hi markdownBold gui=bold guifg=gray25
    hi markdownItalic guifg=gray25 gui=underline
    hi markdownUrl guifg=#2fb3a6
    hi markdownAutomaticLink guifg=#2fb3a6
    hi markdownLinkText guifg=#317849
    hi markdownUrlTitle guifg=#317849
    hi markdownBlockquote guifg=#317849 gui=bold
    hi markdownId guifg=#2fb3a6
    hi markdownIdDeclaration guifg=#317849 gui=bold
    hi markdownListMarker guifg=#317849
    hi Cursor guibg=#15abdd

    if has('gui_running')
        let l:highlightbgcolor = "guibg=#f2f2f2" 
        let l:highlightfgbgcolor = "guifg=#f2f2f2" . " " . l:highlightbgcolor
    else
        let l:highlightbgcolor = "ctermbg=bg" 
        let l:highlightfgbgcolor = "ctermfg=bg" . " " . l:highlightbgcolor
    endif

    exec( "hi Normal " . l:highlightbgcolor )
    exec( "hi VertSplit " . l:highlightfgbgcolor )
    exec( "hi NonText " . l:highlightfgbgcolor )
    exec( "hi StatusLine " . l:highlightfgbgcolor )
    exec( "hi StatusLineNC " . l:highlightfgbgcolor )
endfunction

function! s:zenroom_goyo_before()
    if !has("gui_running")
        return
    endif
    let is_mark_or_rst = &filetype == "markdown" || &filetype == "rst" || &filetype == "text"

    if is_mark_or_rst
        call s:markdown_room()
    endif
endfunction

function! s:zenroom_goyo_after()
    if !has("gui_running")
        return
    endif
    let is_mark_or_rst = &filetype == "markdown" || &filetype == "rst" || &filetype == "text"
    if is_mark_or_rst
        set linespace=0

        if s:save_background != ""
            exec( "set background=" . s:save_background )
        endif
    endif
endfunction

let g:goyo_callbacks = [ function('s:zenroom_goyo_before'), function('s:zenroom_goyo_after') ]
