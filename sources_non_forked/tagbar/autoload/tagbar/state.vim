function! tagbar#state#get_current_file(force_current) abort
    return s:get().getCurrent(a:force_current)
endfunction

function! tagbar#state#set_current_file(fileinfo) abort
    call s:get().setCurrentFile(a:fileinfo)
endfunction

function! tagbar#state#set_paused() abort
    call s:get().setPaused()
endfunction

function! s:get() abort
    if !exists('t:tagbar_state')
        let t:tagbar_state = s:State.New()
    endif

    return t:tagbar_state
endfunction

let s:State = {
    \ '_current' : {},
    \ '_paused'  : {},
\ }

" s:state.New() {{{1
function! s:State.New() abort dict
    return deepcopy(self)
endfunction

" s:state.getCurrent() {{{1
function! s:State.getCurrent(force_current) abort dict
    if !tagbar#is_paused() || a:force_current
        return self._current
    else
        return self._paused
    endif
endfunction

" s:state.setCurrentFile() {{{1
function! s:State.setCurrentFile(fileinfo) abort dict
    let self._current = a:fileinfo
endfunction

" s:state.setPaused() {{{1
function! s:State.setPaused() abort dict
    let self._paused = self._current
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
