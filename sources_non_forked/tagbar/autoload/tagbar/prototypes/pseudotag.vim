function! tagbar#prototypes#pseudotag#new(name) abort
    let newobj = tagbar#prototypes#basetag#new(a:name)

    let newobj.isPseudoTag = function(s:add_snr('s:isPseudoTag'))
    let newobj.strfmt = function(s:add_snr('s:strfmt'))

    return newobj
endfunction

" s:isPseudoTag() {{{1
function! s:isPseudoTag() abort dict
    return 1
endfunction

" s:strfmt() {{{1
function! s:strfmt() abort dict
    let typeinfo = self.typeinfo

    let suffix = get(self.fields, 'signature', '')
    if has_key(typeinfo.kind2scope, self.fields.kind)
        let suffix .= ' : ' . typeinfo.kind2scope[self.fields.kind]
    endif

    return self._getPrefix() . self.name . '*' . suffix
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
