" Author: w0rp <devw0rp@gmail.com>
" Description: Set options in files based on regex patterns.

" A dictionary mapping regular expression patterns to arbitrary buffer
" variables to be set. Useful for configuring ALE based on filename patterns.
let g:ale_pattern_options = get(g:, 'ale_pattern_options', {})
let g:ale_pattern_options_enabled = get(g:, 'ale_pattern_options_enabled', !empty(g:ale_pattern_options))

" These variables are used to cache the sorting of patterns below.
let s:last_pattern_options = {}
let s:sorted_items = []

function! s:CmpPatterns(left_item, right_item) abort
    if a:left_item[0] < a:right_item[0]
        return -1
    endif

    if a:left_item[0] > a:right_item[0]
        return 1
    endif

    return 0
endfunction

function! ale#pattern_options#SetOptions(buffer) abort
    if !get(g:, 'ale_pattern_options_enabled', 0)
    \|| empty(get(g:, 'ale_pattern_options', 0))
        return
    endif

    " The items will only be sorted whenever the patterns change.
    if g:ale_pattern_options != s:last_pattern_options
        let s:last_pattern_options = deepcopy(g:ale_pattern_options)
        " The patterns are sorted, so they are applied consistently.
        let s:sorted_items = sort(
        \   items(g:ale_pattern_options),
        \   function('s:CmpPatterns')
        \)
    endif

    let l:filename = expand('#' . a:buffer . ':p')

    for [l:pattern, l:options] in s:sorted_items
        if match(l:filename, l:pattern) >= 0
            for [l:key, l:value] in items(l:options)
                call setbufvar(a:buffer, l:key, l:value)
            endfor
        endif
    endfor
endfunction
