" Author: w0rp <devw0rp@gmail.com>
" Description: Logic for handling mappings between files

" Invert filesystem mappings so they can be mapped in reverse.
function! ale#filename_mapping#Invert(filename_mappings) abort
    return map(copy(a:filename_mappings), '[v:val[1], v:val[0]]')
endfunction

" Given a filename and some filename_mappings, map a filename.
function! ale#filename_mapping#Map(filename, filename_mappings) abort
    let l:simplified_filename = ale#path#Simplify(a:filename)

    for [l:mapping_from, l:mapping_to] in a:filename_mappings
        let l:mapping_from = ale#path#Simplify(l:mapping_from)

        if l:simplified_filename[:len(l:mapping_from) - 1] is# l:mapping_from
            return l:mapping_to . l:simplified_filename[len(l:mapping_from):]
        endif
    endfor

    return a:filename
endfunction
