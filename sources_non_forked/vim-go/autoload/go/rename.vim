if !exists("g:go_gorename_bin")
    let g:go_gorename_bin = "gorename"
endif

function! go#rename#Rename(bang, ...)
    let to = ""
    if a:0 == 0
        let from = expand("<cword>")
        let ask = printf("vim-go: rename '%s' to: ", from)
        let to = input(ask, from)
        redraw
    else
        let to = a:1
    endif

    "return with a warning if the bin doesn't exist
    let bin_path = go#path#CheckBinPath(g:go_gorename_bin) 
    if empty(bin_path) 
        return 
    endif

    let fname = expand('%:p')
    let pos = s:getpos(line('.'), col('.'))
    let cmd = printf('%s -offset %s -to %s', shellescape(bin_path), shellescape(printf('%s:#%d', fname, pos)), shellescape(to))

    let out = go#tool#ExecuteInDir(cmd)

    " strip out newline on the end that gorename puts. If we don't remove, it
    " will trigger the 'Hit ENTER to continue' prompt
    let clean = split(out, '\n')

    if v:shell_error
        let errors = go#tool#ParseErrors(split(out, '\n'))
        call go#list#Populate(errors)
        call go#list#Window(len(errors))
        if !empty(errors) && !a:bang
            call go#list#JumpToFirst()
        elseif empty(errors)
            " failed to parse errors, output the original content
            call go#util#EchoError(out)
        endif
        return
    else
        call go#list#Clean()
        call go#list#Window()
        redraw | echon "vim-go: " | echohl Function | echon clean[0] | echohl None
    endif

    " refresh the buffer so we can see the new content
    " TODO(arslan): also find all other buffers and refresh them too. For this
    " we need a way to get the list of changes from gorename upon an success
    " change.
    silent execute ":e"
endfunction

func! s:getpos(l, c)
    if &encoding != 'utf-8'
        let buf = a:l == 1 ? '' : (join(getline(1, a:l-1), "\n") . "\n")
        let buf .= a:c == 1 ? '' : getline('.')[:a:c-2]
        return len(iconv(buf, &encoding, 'utf-8'))
    endif
    return line2byte(a:l) + (a:c-2)
endfun

" vim:ts=4:sw=4:et
"
