let s:delimitMate_extra_excluded_regions = ',rustLifetimeCandidate,rustGenericLifetimeCandidate'

" For this buffer, when delimitMate issues the `User delimitMate_map`
" event in the autocommand system, add the above-defined extra excluded
" regions to delimitMate's state, if they have not already been added.
function! rust#delimitmate#onMap() abort
    if &filetype !=# 'rust'
        return
    endif

    if get(b:, "delimitMate_quotes")
        let b:rust_prev_delimitMate_quotes = b:delimitMate_quotes
    endif
    let b:delimitMate_quotes = "\" `"

    if match(delimitMate#Get("excluded_regions"),
                \ s:delimitMate_extra_excluded_regions) == -1
        call delimitMate#Set("excluded_regions",
                    \delimitMate#Get("excluded_regions").s:delimitMate_extra_excluded_regions)
    endif
endfunction

" For this buffer, when delimitMate issues the `User delimitMate_unmap`
" event in the autocommand system, delete the above-defined extra excluded
" regions from delimitMate's state (the deletion being idempotent and
" having no effect if the extra excluded regions are not present in the
" targeted part of delimitMate's state).
function! rust#delimitmate#onUnmap() abort
    if &filetype !=# 'rust'
        return
    endif

    if get(b:, "rust_prev_delimitMate_quotes")
        let b:delimitMate_quotes = b:rust_prev_delimitMate_quotes
    endif

    call delimitMate#Set("excluded_regions", substitute(
               \ delimitMate#Get("excluded_regions"),
               \ '\C\V' . s:delimitMate_extra_excluded_regions,
               \ '', 'g'))
endfunction

" vim: set et sw=4 sts=4 ts=8:

