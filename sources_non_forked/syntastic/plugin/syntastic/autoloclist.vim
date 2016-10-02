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
    let auto_loc_list = syntastic#util#var('auto_loc_list')
    if !a:loclist.isEmpty()
        if auto_loc_list == 1 || auto_loc_list == 3
            call a:loclist.show()
        endif
    else
        if (auto_loc_list == 1 || auto_loc_list == 2) && !empty(get(w:, 'syntastic_loclist_set', []))
            try
                " Vim 7.4.2200 or later
                let title = get(getloclist(0, { 'title': 1 }), 'title', ':SyntasticCheck ')
            catch /\m^Vim\%((\a\+)\)\=:E\%(118\|731\)/
                let title = ':SyntasticCheck '
            endtry

            if strpart(title, 0, 16) ==# ':SyntasticCheck '
                " TODO: this will close the loc list window if one was opened
                " by something other than syntastic
                call SyntasticLoclistHide()

                try
                    " Vim 7.4.2200 or later
                    call setloclist(0, [], 'r', { 'title': '' })
                catch /\m^Vim\%((\a\+)\)\=:E\%(118\|731\)/
                    " do nothing
                endtry
                let w:syntastic_loclist_set = []
            endif
        endif
    endif
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
