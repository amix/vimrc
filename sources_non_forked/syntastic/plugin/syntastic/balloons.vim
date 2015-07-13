if exists('g:loaded_syntastic_notifier_balloons') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_notifier_balloons = 1

if !has('balloon_eval')
    let g:syntastic_enable_balloons = 0
endif

let g:SyntasticBalloonsNotifier = {}

" Public methods {{{1

function! g:SyntasticBalloonsNotifier.New() abort " {{{2
    let newObj = copy(self)
    return newObj
endfunction " }}}2

function! g:SyntasticBalloonsNotifier.enabled() abort " {{{2
    return has('balloon_eval') && syntastic#util#var('enable_balloons')
endfunction " }}}2

" Update the error balloons
function! g:SyntasticBalloonsNotifier.refresh(loclist) abort " {{{2
    unlet! b:syntastic_private_balloons
    if self.enabled() && !a:loclist.isEmpty()
        let b:syntastic_private_balloons = a:loclist.balloons()
        if !empty(b:syntastic_private_balloons)
            set ballooneval balloonexpr=SyntasticBalloonsExprNotifier()
        endif
    endif
endfunction " }}}2

" Reset the error balloons
" @vimlint(EVL103, 1, a:loclist)
function! g:SyntasticBalloonsNotifier.reset(loclist) abort " {{{2
    let b:syntastic_private_balloons = {}
    if has('balloon_eval')
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'balloons: reset')
        unlet! b:syntastic_private_balloons
        set noballooneval
    endif
endfunction " }}}2
" @vimlint(EVL103, 0, a:loclist)

" }}}1

" Private functions {{{1

function! SyntasticBalloonsExprNotifier() abort " {{{2
    if !exists('b:syntastic_private_balloons')
        return ''
    endif
    return get(b:syntastic_private_balloons, v:beval_lnum, '')
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
