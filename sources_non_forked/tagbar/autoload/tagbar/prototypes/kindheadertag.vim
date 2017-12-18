function! tagbar#prototypes#kindheadertag#new(name) abort
    let newobj = tagbar#prototypes#basetag#new(a:name)

    let newobj.isKindheader = function(s:add_snr('s:isKindheader'))
    let newobj.getPrototype = function(s:add_snr('s:getPrototype'))
    let newobj.isFoldable = function(s:add_snr('s:isFoldable'))
    let newobj.isFolded = function(s:add_snr('s:isFolded'))
    let newobj.openFold = function(s:add_snr('s:openFold'))
    let newobj.closeFold = function(s:add_snr('s:closeFold'))
    let newobj.toggleFold = function(s:add_snr('s:toggleFold'))

    return newobj
endfunction

" s:isKindheader() {{{1
function! s:isKindheader() abort dict
    return 1
endfunction

" s:getPrototype() {{{1
function! s:getPrototype(short) abort dict
    return self.name . ': ' .
         \ self.numtags . ' ' . (self.numtags > 1 ? 'tags' : 'tag')
endfunction

" s:isFoldable() {{{1
function! s:isFoldable() abort dict
    return 1
endfunction

" s:isFolded() {{{1
function! s:isFolded() abort dict
    return self.fileinfo.kindfolds[self.short]
endfunction

" s:openFold() {{{1
function! s:openFold() abort dict
    let self.fileinfo.kindfolds[self.short] = 0
endfunction

" s:closeFold() {{{1
function! s:closeFold() abort dict
    let self.fileinfo.kindfolds[self.short] = 1
    return line('.')
endfunction

" s:toggleFold() {{{1
function! s:toggleFold(fileinfo) abort dict
    let a:fileinfo.kindfolds[self.short] = !a:fileinfo.kindfolds[self.short]
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
