if exists("g:loaded_syntastic_notifier_highlighting") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_notifier_highlighting = 1

" Highlighting requires getmatches introduced in 7.1.040
let s:has_highlighting = v:version > 701 || (v:version == 701 && has('patch040'))
lockvar s:has_highlighting

let g:SyntasticHighlightingNotifier = {}

let s:setup_done = 0

" Public methods {{{1

function! g:SyntasticHighlightingNotifier.New() abort " {{{2
    let newObj = copy(self)

    if !s:setup_done
        call self._setup()
        let s:setup_done = 1
        lockvar s:setup_done
    endif

    return newObj
endfunction " }}}2

function! g:SyntasticHighlightingNotifier.enabled() abort " {{{2
    return s:has_highlighting && syntastic#util#var('enable_highlighting')
endfunction " }}}2

" Sets error highlights in the cuirrent window
function! g:SyntasticHighlightingNotifier.refresh(loclist) abort " {{{2
    if self.enabled()
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'highlighting: refresh')
        call self._reset()
        let buf = bufnr('')
        let issues = filter(a:loclist.copyRaw(), 'v:val["bufnr"] == buf')
        for item in issues
            let group = 'Syntastic' . get(item, 'subtype', '') . ( item['type'] ==? 'E' ? 'Error' : 'Warning' )

            " The function `Syntastic_{filetype}_{checker}_GetHighlightRegex` is
            " used to override default highlighting.
            if has_key(item, 'hl')
                call matchadd(group, '\%' . item['lnum'] . 'l' . item['hl'])
            elseif get(item, 'col', 0)
                if get(item, 'vcol', 0)
                    let lastcol = virtcol([item['lnum'], '$'])
                    let coltype = 'v'
                else
                    let lastcol = col([item['lnum'], '$'])
                    let coltype = 'c'
                endif
                let lcol = min([lastcol, item['col']])

                call matchadd(group, '\%' . item['lnum'] . 'l\%' . lcol . coltype)
            endif
        endfor
    endif
endfunction " }}}2

" Remove all error highlights from the window
" @vimlint(EVL103, 1, a:loclist)
function! g:SyntasticHighlightingNotifier.reset(loclist) abort " {{{2
    if s:has_highlighting
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'highlighting: reset')
        call self._reset()
    endif
endfunction " }}}2
" @vimlint(EVL103, 0, a:loclist)

" }}}1

" Private methods {{{1

" One time setup: define our own highlighting
function! g:SyntasticHighlightingNotifier._setup() abort " {{{2
    if s:has_highlighting
        if !hlexists('SyntasticError')
            highlight link SyntasticError SpellBad
        endif
        if !hlexists('SyntasticWarning')
            highlight link SyntasticWarning SpellCap
        endif
        if !hlexists('SyntasticStyleError')
            highlight link SyntasticStyleError SyntasticError
        endif
        if !hlexists('SyntasticStyleWarning')
            highlight link SyntasticStyleWarning SyntasticWarning
        endif
    endif
endfunction " }}}2

function! g:SyntasticHighlightingNotifier._reset() abort " {{{2
    for match in getmatches()
        if stridx(match['group'], 'Syntastic') == 0
            call matchdelete(match['id'])
        endif
    endfor
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
