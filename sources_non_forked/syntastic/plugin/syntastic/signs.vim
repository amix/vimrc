if exists("g:loaded_syntastic_notifier_signs")
    finish
endif
let g:loaded_syntastic_notifier_signs = 1

if !exists("g:syntastic_enable_signs")
    let g:syntastic_enable_signs = 1
endif

if !exists("g:syntastic_error_symbol")
    let g:syntastic_error_symbol = '>>'
endif

if !exists("g:syntastic_warning_symbol")
    let g:syntastic_warning_symbol = '>>'
endif

if !exists("g:syntastic_style_error_symbol")
    let g:syntastic_style_error_symbol = 'S>'
endif

if !exists("g:syntastic_style_warning_symbol")
    let g:syntastic_style_warning_symbol = 'S>'
endif


" start counting sign ids at 5000, start here to hopefully avoid conflicting
" with any other code that places signs (not sure if this precaution is
" actually needed)
let s:first_sign_id = 5000
let s:next_sign_id = s:first_sign_id

let g:SyntasticSignsNotifier = {}

let s:setup_done = 0

" Public methods {{{1

function! g:SyntasticSignsNotifier.New()
    let newObj = copy(self)

    if !s:setup_done
        call self._setup()
        let s:setup_done = 1
    endif

    return newObj
endfunction

function! g:SyntasticSignsNotifier.enabled()
    return
        \ has('signs') &&
        \ exists('b:syntastic_enable_signs') ? b:syntastic_enable_signs : g:syntastic_enable_signs
endfunction

function! g:SyntasticSignsNotifier.refresh(loclist)
    let old_signs = copy(self._bufSignIds())
    if self.enabled()
        call self._signErrors(a:loclist)
    endif
    call self._removeSigns(old_signs)
    let s:first_sign_id = s:next_sign_id
endfunction

" Private methods {{{1

" One time setup: define our own sign types and highlighting
function! g:SyntasticSignsNotifier._setup()
    if has('signs')
        if !hlexists('SyntasticErrorSign')
            highlight link SyntasticErrorSign error
        endif
        if !hlexists('SyntasticWarningSign')
            highlight link SyntasticWarningSign todo
        endif
        if !hlexists('SyntasticStyleErrorSign')
            highlight link SyntasticStyleErrorSign SyntasticErrorSign
        endif
        if !hlexists('SyntasticStyleWarningSign')
            highlight link SyntasticStyleWarningSign SyntasticWarningSign
        endif
        if !hlexists('SyntasticStyleErrorLine')
            highlight link SyntasticStyleErrorLine SyntasticErrorLine
        endif
        if !hlexists('SyntasticStyleWarningLine')
            highlight link SyntasticStyleWarningLine SyntasticWarningLine
        endif

        " define the signs used to display syntax and style errors/warns
        exe 'sign define SyntasticError text=' . g:syntastic_error_symbol .
            \ ' texthl=SyntasticErrorSign linehl=SyntasticErrorLine'
        exe 'sign define SyntasticWarning text=' . g:syntastic_warning_symbol .
            \ ' texthl=SyntasticWarningSign linehl=SyntasticWarningLine'
        exe 'sign define SyntasticStyleError text=' . g:syntastic_style_error_symbol .
            \ ' texthl=SyntasticStyleErrorSign linehl=SyntasticStyleErrorLine'
        exe 'sign define SyntasticStyleWarning text=' . g:syntastic_style_warning_symbol .
            \ ' texthl=SyntasticStyleWarningSign linehl=SyntasticStyleWarningLine'
    endif
endfunction

" Place signs by all syntax errors in the buffer
function! g:SyntasticSignsNotifier._signErrors(loclist)
    let loclist = a:loclist
    if loclist.hasErrorsOrWarningsToDisplay()

        " errors some first, so that they are not masked by warnings
        let buf = bufnr('')
        let issues = copy(loclist.errors())
        if !loclist.quietWarnings()
            call extend(issues, loclist.warnings())
        endif
        call filter(issues, 'v:val["bufnr"] == buf')
        let seen = {}

        for i in issues
            if !has_key(seen, i['lnum'])
                let seen[i['lnum']] = 1

                let sign_severity = i['type'] ==? 'W' ? 'Warning' : 'Error'
                let sign_subtype = get(i, 'subtype', '')
                let sign_type = 'Syntastic' . sign_subtype . sign_severity

                exec "sign place " . s:next_sign_id . " line=" . i['lnum'] . " name=" . sign_type . " buffer=" . i['bufnr']
                call add(self._bufSignIds(), s:next_sign_id)
                let s:next_sign_id += 1
            endif
        endfor
    endif
endfunction

" Remove the signs with the given ids from this buffer
function! g:SyntasticSignsNotifier._removeSigns(ids)
    if has('signs')
        for i in a:ids
            exec "sign unplace " . i
            call remove(self._bufSignIds(), index(self._bufSignIds(), i))
        endfor
    endif
endfunction

" Get all the ids of the SyntaxError signs in the buffer
function! g:SyntasticSignsNotifier._bufSignIds()
    if !exists("b:syntastic_sign_ids")
        let b:syntastic_sign_ids = []
    endif
    return b:syntastic_sign_ids
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
