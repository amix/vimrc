" Script-local variable needed since compare functions can't
" take additional arguments
let s:compare_typeinfo = {}

function! tagbar#sorting#sort(tags, compareby, compare_typeinfo) abort
    let s:compare_typeinfo = a:compare_typeinfo

    let comparemethod =
            \ a:compareby == 'kind' ? 's:compare_by_kind' : 's:compare_by_line'

    call sort(a:tags, comparemethod)

    for tag in a:tags
        if !empty(tag.getChildren())
            call tagbar#sorting#sort(tag.getChildren(), a:compareby,
                                   \ a:compare_typeinfo)
        endif
    endfor
endfunction

function! s:compare_by_kind(tag1, tag2) abort
    let typeinfo = s:compare_typeinfo

    if typeinfo.kinddict[a:tag1.fields.kind] <#
     \ typeinfo.kinddict[a:tag2.fields.kind]
        return -1
    elseif typeinfo.kinddict[a:tag1.fields.kind] >#
         \ typeinfo.kinddict[a:tag2.fields.kind]
        return 1
    else
        " Ignore '~' prefix for C++ destructors to sort them directly under
        " the constructors
        if a:tag1.name[0] ==# '~'
            let name1 = a:tag1.name[1:]
        else
            let name1 = a:tag1.name
        endif
        if a:tag2.name[0] ==# '~'
            let name2 = a:tag2.name[1:]
        else
            let name2 = a:tag2.name
        endif

        let ci = g:tagbar_case_insensitive
        if (((!ci) && (name1 <=# name2)) || (ci && (name1 <=? name2)))
            return -1
        else
            return 1
        endif
    endif
endfunction

function! s:compare_by_line(tag1, tag2) abort
    return a:tag1.fields.line - a:tag2.fields.line
endfunction

" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
