"==============================================================================
"File:        vimroom.vim
"Description: Vaguely emulates a writeroom-like environment in Vim by
"             splitting the current window in such a way as to center a column
"             of user-specified width, wrap the text, and break lines.
"Maintainer:  Mike West <mike@mikewest.org>
"Version:     0.7
"Last Change: 2010-10-31
"License:     BSD <../LICENSE.markdown>
"==============================================================================

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin Configuration
"

" The typical start to any vim plugin: If the plugin has already been loaded,
" exit as quickly as possible.
if exists( "g:loaded_vimroom_plugin" )
    finish
endif
let g:loaded_vimroom_plugin = 1

" The desired column width.  Defaults to 100:
if !exists( "g:vimroom_width" )
    let g:vimroom_width = 100
endif

" The minimum sidebar size.  Defaults to 5:
if !exists( "g:vimroom_min_sidebar_width" )
    let g:vimroom_min_sidebar_width = 5
endif

" The sidebar height.  Defaults to 3:
if !exists( "g:vimroom_sidebar_height" )
    let g:vimroom_sidebar_height = 2
endif

" The GUI background color.  Defaults to "black"
if !exists( "g:vimroom_guibackground" )
    let g:vimroom_guibackground = "#202020"
endif

" The cterm background color.  Defaults to "bg"
if !exists( "g:vimroom_ctermbackground" )
    let g:vimroom_ctermbackground = "bg"
endif

" The "scrolloff" value: how many lines should be kept visible above and below
" the cursor at all times?  Defaults to 999 (which centers your cursor in the 
" active window).
if !exists( "g:vimroom_scrolloff" )
    let g:vimroom_scrolloff = 0
endif

" Should Vimroom map navigational keys (`<Up>`, `<Down>`, `j`, `k`) to navigate
" "display" lines instead of "logical" lines (which makes it much simpler to deal
" with wrapped lines). Defaults to `1` (on). Set to `0` if you'd prefer not to
" run the mappings.
if !exists( "g:vimroom_navigation_keys" )
    let g:vimroom_navigation_keys = 1
endif

" Should Vimroom clear line numbers from the Vimroomed buffer?  Defaults to `1`
" (on). Set to `0` if you'd prefer Vimroom to leave line numbers untouched.
" (Note that setting this to `0` will not turn line numbers on if they aren't
" on already).
if !exists( "g:vimroom_clear_line_numbers" )
    let g:vimroom_clear_line_numbers = 1
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin Code
"

" Given the desired column width, and minimum sidebar width, determine
" the minimum window width necessary for splitting to make sense
let s:minwidth = g:vimroom_width + ( g:vimroom_min_sidebar_width * 2 )

" Save the current color scheme for reset later
let s:scheme = ""
if exists( "g:colors_name" )
    let s:scheme = g:colors_name
endif
if exists( "&t_mr" )
    let s:save_t_mr = &t_mr
end

" Save the current scrolloff value for reset later
let s:save_scrolloff = ""
if exists( "&scrolloff" )
    let s:save_scrolloff = &scrolloff
end

" Save the current `laststatus` value for reset later
let s:save_laststatus = ""
if exists( "&laststatus" )
    let s:save_laststatus = &laststatus
endif

" Save the current `background` value for reset later
let s:save_background = ""
if exists( "&background" )
    let s:save_background = &background
endif

" Save the current `textwidth'` value for reset later
let s:save_textwidth = ""
if exists( "&textwidth'" )
    let s:save_textwidth' = &textwidth'
endif

" Save the current `showtabline` value for reset later
let s:save_showtabline = ""
if exists( "&showtabline" )
    let s:save_showtabline = &showtabline
endif

" Save the current `textwidth` value for reset later
let s:save_textwidth = ""
if exists( "&textwidth" )
    let s:save_textwidth = &textwidth
endif

" Save the current `number` and `relativenumber` values for reset later
let s:save_number = 0
let s:save_relativenumber = 0
if exists( "&number" )
    let s:save_number = &number
endif
if exists ( "&relativenumber" )
    let s:save_relativenumber = &relativenumber
endif

" We're currently in nonvimroomized state
let s:active  = 0

function! s:is_the_screen_wide_enough()
    return winwidth( winnr() ) >= s:minwidth
endfunction

function! s:sidebar_size()
    return ( winwidth( winnr() ) - g:vimroom_width - 2 ) / 2
endfunction

function! s:markdown_room()
    set background=light
    set linespace=8
    set textwidth=80
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
        let l:highlightbgcolor = "ctermbg=" . g:vimroom_ctermbackground
        let l:highlightfgbgcolor = "ctermfg=" . g:vimroom_ctermbackground . " " . l:highlightbgcolor
    endif

    exec( "hi Normal " . l:highlightbgcolor )
    exec( "hi VertSplit " . l:highlightfgbgcolor )
    exec( "hi NonText " . l:highlightfgbgcolor )
    exec( "hi StatusLine " . l:highlightfgbgcolor )
    exec( "hi StatusLineNC " . l:highlightfgbgcolor )
endfunction

function! <SID>VimroomToggle()
    if s:active == 1
        let s:active = 0
        " Close all other split windows
        if g:vimroom_sidebar_height
            wincmd j
            close
            wincmd k
            close
        endif
        if g:vimroom_min_sidebar_width
            wincmd l
            close
            wincmd h
            close
        endif
        " Reset color scheme (or clear new colors, if no scheme is set)
        if s:scheme != ""
            exec( "colorscheme " . s:scheme ) 
        else
            hi clear
        endif
        if s:save_t_mr != ""
            exec( "set t_mr=" .s:save_t_mr )
        endif
        " Reset `scrolloff` and `laststatus`
        if s:save_scrolloff != ""
            exec( "set scrolloff=" . s:save_scrolloff )
        endif
        if s:save_laststatus != ""
            exec( "set laststatus=" . s:save_laststatus )
        endif
        if s:save_showtabline != ""
            exec( "set showtabline=" . s:save_showtabline )
        endif

        exec( "set linespace=0" )

        if s:save_background != ""
            exec( "set background=" . s:save_background )
        endif
        if s:save_textwidth != ""
            exec( "set textwidth=" . s:save_textwidth )
        endif
        if s:save_number != 0
            set number
        endif
        if s:save_relativenumber != 0
            set relativenumber
        endif
        " Remove wrapping and linebreaks
        set nowrap
        set nolinebreak

        " Enable AirLine
        exec("silent AirlineToggle")
        exec("silent AirlineRefresh")
    else
        if s:is_the_screen_wide_enough()
            let is_mark_or_rst = &filetype == "markdown" || &filetype == "rst" || &filetype == "text"

            if is_mark_or_rst
                call s:markdown_room()
            endif
            
            " Disable AirLine
            exec("silent AirlineToggle")

            let s:active = 1
            let s:sidebar = s:sidebar_size()

            " Turn off status bar
            if s:save_laststatus != ""
                setlocal laststatus=0
            endif
            " Turn off tabline
            if s:save_showtabline != ""
                setlocal showtabline=0
            endif
            if g:vimroom_min_sidebar_width
                " Create the left sidebar
                exec( "silent leftabove " . s:sidebar . "vsplit new" )
                setlocal noma
                setlocal nocursorline
                setlocal nonumber
                silent! setlocal norelativenumber
                wincmd l
                " Create the right sidebar
                exec( "silent rightbelow " . s:sidebar . "vsplit new" )
                setlocal noma
                setlocal nocursorline
                setlocal nonumber
                silent! setlocal norelativenumber
                wincmd h
                exec( "silent vertical resize " . g:vimroom_width )
            endif
            if g:vimroom_sidebar_height
                " Create the top sidebar
                exec( "silent leftabove " . g:vimroom_sidebar_height . "split new" )
                setlocal noma
                setlocal nocursorline
                setlocal nonumber
                silent! setlocal norelativenumber
                wincmd j
                " Create the bottom sidebar
                exec( "silent rightbelow " . g:vimroom_sidebar_height . "split new" )
                setlocal noma
                setlocal nocursorline
                setlocal nonumber
                silent! setlocal norelativenumber
                wincmd k
            endif
            " Setup wrapping, line breaking, and push the cursor down
            set wrap
            set linebreak
            if g:vimroom_clear_line_numbers
                set nonumber
                silent! set norelativenumber
            endif
            if s:save_textwidth != ""
                exec( "set textwidth=".g:vimroom_width )
            endif
            if s:save_scrolloff != ""
                exec( "set scrolloff=".g:vimroom_scrolloff )
            endif

            " Setup navigation over "display lines", not "logical lines" if
            " mappings for the navigation keys don't already exist.
            if g:vimroom_navigation_keys
                try
                    noremap     <unique> <silent> <Up> g<Up>
                    noremap     <unique> <silent> <Down> g<Down>
                    noremap     <unique> <silent> k gk
                    noremap     <unique> <silent> j gj
                    inoremap    <unique> <silent> <Up> <C-o>g<Up>
                    inoremap    <unique> <silent> <Down> <C-o>g<Down>
                catch /E227:/
                    echo "Navigational key mappings already exist."
                endtry
            endif

            " Hide distracting visual elements
            if !is_mark_or_rst
                if has('gui_running')
                    let l:highlightbgcolor = "guibg=" . g:vimroom_guibackground
                    let l:highlightfgbgcolor = "guifg=" . g:vimroom_guibackground . " " . l:highlightbgcolor
                else
                    let l:highlightbgcolor = "ctermbg=" . g:vimroom_ctermbackground
                    let l:highlightfgbgcolor = "ctermfg=" . g:vimroom_ctermbackground . " " . l:highlightbgcolor
                endif
                exec( "hi Normal " . l:highlightbgcolor )
                exec( "hi VertSplit " . l:highlightfgbgcolor )
                exec( "hi NonText " . l:highlightfgbgcolor )
                exec( "hi StatusLine " . l:highlightfgbgcolor )
                exec( "hi StatusLineNC " . l:highlightfgbgcolor )
            endif
            set t_mr=""
            set fillchars+=vert:\ 
        endif
    endif
endfunction

" Create a mapping for the `VimroomToggle` function
noremap <silent> <Plug>VimroomToggle    :call <SID>VimroomToggle()<CR>

" Create a `VimroomToggle` command:
command -nargs=0 VimroomToggle call <SID>VimroomToggle()

" If no mapping exists, map it to `<Leader>V`.
if !hasmapto( '<Plug>VimroomToggle' )
    nmap <silent> <Leader>V <Plug>VimroomToggle
endif
