if exists("g:loaded_syntastic_notifier_highlighting")
    finish
endif
let g:loaded_syntastic_notifier_highlighting = 1

" Highlighting requires getmatches introduced in 7.1.040
let s:has_highlighting = v:version > 701 || (v:version == 701 && has('patch040'))

if !exists("g:syntastic_enable_highlighting")
    let g:syntastic_enable_highlighting = 1
endif

let g:SyntasticHighlightingNotifier = {}

let s:setup_done = 0

" Public methods {{{1

function! g:SyntasticHighlightingNotifier.New()
    let newObj = copy(self)

    if !s:setup_done
        call self._setup()
        let s:setup_done = 1
    endif

    return newObj
endfunction

function! g:SyntasticHighlightingNotifier.enabled()
    return
        \ s:has_highlighting &&
        \ (exists('b:syntastic_enable_highlighting') ? b:syntastic_enable_highlighting : g:syntastic_enable_highlighting)
endfunction

" Sets error highlights in the cuirrent window
function! g:SyntasticHighlightingNotifier.refresh(loclist)
    if self.enabled()
        call self.reset(a:loclist)
        call syntastic#log#debug(g:SyntasticDebugNotifications, 'highlighting: refresh')
        let buf = bufnr('')
        let issues = filter(a:loclist.copyRaw(), 'v:val["bufnr"] == buf')
        for item in issues
            let group = item['type'] ==? 'E' ? 'SyntasticError' : 'SyntasticWarning'

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
endfunction

" Remove all error highlights from the window
" @vimlint(EVL103, 1, a:loclist)
function! g:SyntasticHighlightingNotifier.reset(loclist)
    if s:has_highlighting
        call syntastic#log#debug(g:SyntasticDebugNotifications, 'highlighting: reset')
        for match in getmatches()
            if stridx(match['group'], 'Syntastic') == 0
                call matchdelete(match['id'])
            endif
        endfor
    endif
endfunction
" @vimlint(EVL103, 0, a:loclist)

" Private methods {{{1

" One time setup: define our own highlighting
function! g:SyntasticHighlightingNotifier._setup()
    if s:has_highlighting
        if !hlexists('SyntasticError')
            highlight link SyntasticError SpellBad

        endif
        if !hlexists('SyntasticWarning')
            highlight link SyntasticWarning SpellCap
        endif
    endif
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
