if exists('g:loaded_syntastic_notifier_autoloclist') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_notifier_autoloclist = 1

let g:SyntasticAutoloclistNotifier = {}

" Public methods {{{1
"
function! g:SyntasticAutoloclistNotifier.New() abort " {{{2
    let newObj = copy(self)
    return newObj
endfunction " }}}2

function! g:SyntasticAutoloclistNotifier.refresh(loclist) abort " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'autoloclist: refresh')
    call g:SyntasticAutoloclistNotifier.AutoToggle(a:loclist)
endfunction " }}}2

function! g:SyntasticAutoloclistNotifier.AutoToggle(loclist) abort " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'autoloclist: toggle')
    if !a:loclist.isEmpty()
        if syntastic#util#var('auto_loc_list') == 1
            call a:loclist.show()
        endif
    else
        if syntastic#util#var('auto_loc_list') > 0

            "TODO: this will close the loc list window if one was opened by
            "something other than syntastic
            lclose
        endif
    endif
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
