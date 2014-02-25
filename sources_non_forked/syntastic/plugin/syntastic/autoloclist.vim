if exists("g:loaded_syntastic_notifier_autoloclist")
    finish
endif
let g:loaded_syntastic_notifier_autoloclist = 1

if !exists("g:syntastic_auto_loc_list")
    let g:syntastic_auto_loc_list = 2
endif

let g:SyntasticAutoloclistNotifier = {}

" Public methods {{{1
"
function! g:SyntasticAutoloclistNotifier.New()
    let newObj = copy(self)
    return newObj
endfunction

function! g:SyntasticAutoloclistNotifier.refresh(loclist)
    call g:SyntasticAutoloclistNotifier.AutoToggle(a:loclist)
endfunction

function! g:SyntasticAutoloclistNotifier.AutoToggle(loclist)
    if a:loclist.hasErrorsOrWarningsToDisplay()
        if g:syntastic_auto_loc_list == 1
            call a:loclist.show()
        endif
    else
        if g:syntastic_auto_loc_list > 0

            "TODO: this will close the loc list window if one was opened by
            "something other than syntastic
            lclose
        endif
    endif
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
