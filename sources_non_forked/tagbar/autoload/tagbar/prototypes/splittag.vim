" A tag that was created because of a tag name that covers multiple scopes
" Inherits the fields of the "main" tag it was split from.
" May be replaced during tag processing if it appears as a normal tag later,
" just like a pseudo tag.

function! tagbar#prototypes#splittag#new(name) abort
    let newobj = tagbar#prototypes#normaltag#new(a:name)

    let newobj.isSplitTag = function(s:add_snr('s:isSplitTag'))

    return newobj
endfunction

function! s:isSplitTag() abort dict
    return 1
endfunction

function! s:add_snr(funcname) abort
    if !exists("s:snr")
        let s:snr = matchstr(expand('<sfile>'), '<SNR>\d\+_\zeget_snr$')
    endif
    return s:snr . a:funcname
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
