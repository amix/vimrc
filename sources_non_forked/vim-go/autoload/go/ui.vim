let s:buf_nr = -1

"OpenWindow opens a new scratch window and put's the content into the window
function! go#ui#OpenWindow(content)
    " reuse existing buffer window if it exists otherwise create a new one
    if !bufexists(s:buf_nr)
        execute 'botright new'
        file `="[Implements]"`
        let s:buf_nr = bufnr('%')
    elseif bufwinnr(s:buf_nr) == -1
        execute 'botright new'
        execute s:buf_nr . 'buffer'
    elseif bufwinnr(s:buf_nr) != bufwinnr('%')
        execute bufwinnr(s:buf_nr) . 'wincmd w'
    endif


    " Keep minimum height to 10, if there is more just increase it that it
    " occupies all results
    let implements_height = 10
    if len(a:content) < implements_height
        exe 'resize ' . implements_height
    else
        exe 'resize ' . len(a:content)
    endif
	
		" some sane default values for a readonly buffer
    setlocal filetype=vimgo
    setlocal bufhidden=delete
    setlocal buftype=nofile
    setlocal noswapfile
    setlocal nobuflisted
    setlocal winfixheight
    setlocal cursorline " make it easy to distinguish

    " we need this to purge the buffer content
    setlocal modifiable

    "delete everything first from the buffer
    %delete _  

    " add the content
    call append(0, a:content)

    " delete last line that comes from the append call
    $delete _  

    " set it back to non modifiable
    setlocal nomodifiable
endfunction


" CloseWindow closes the current window
function! go#ui#CloseWindow()
    close
    echo ""
endfunction

" OpenDefinition parses the current line and jumps to it by openening a new
" tab
function! go#ui#OpenDefinition()
    let curline = getline('.')

    " don't touch our first line and any blank line
    if curline =~ "implements" || curline =~ "^$"
        " supress information about calling this function
        echo "" 
        return
    endif

    " format: 'interface file:lnum:coln'
    let mx = '^\(^\S*\)\s*\(.\{-}\):\(\d\+\):\(\d\+\)'

    " parse it now into the list
    let tokens = matchlist(curline, mx)

    " convert to: 'file:lnum:coln'
    let expr = tokens[2] . ":" . tokens[3] . ":" .  tokens[4]

    " jump to it in a new tab, we use explicit lgetexpr so we can later change
    " the behaviour via settings (like opening in vsplit instead of tab)
    lgetexpr expr
    tab split
    ll 1

    " center the word 
    norm! zz 
endfunction

