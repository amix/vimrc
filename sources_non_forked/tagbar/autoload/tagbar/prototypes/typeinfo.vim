function! tagbar#prototypes#typeinfo#new(...) abort
    let newobj = {}

    let newobj.kinddict = {}

    if a:0 > 0
        call extend(newobj, a:1)
    endif

    let newobj.getKind = function(s:add_snr('s:getKind'))
    let newobj.createKinddict = function(s:add_snr('s:createKinddict'))

    return newobj
endfunction

" s:getKind() {{{1
function! s:getKind(kind) abort dict
    let idx = self.kinddict[a:kind]
    return self.kinds[idx]
endfunction

" s:createKinddict() {{{1
" Create a dictionary of the kind order for fast access in sorting functions
function! s:createKinddict() abort dict
    let i = 0
    for kind in self.kinds
        let self.kinddict[kind.short] = i
        let i += 1
    endfor
    let self.kinddict['?'] = i
endfunction

" s:add_snr() {{{1
function! s:add_snr(funcname) abort
    if !exists("s:snr")
        let s:snr = matchstr(expand('<sfile>'), '<SNR>\d\+_\zeget_snr$')
    endif
    return s:snr . a:funcname
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
