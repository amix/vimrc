" Author: w0rp <devw0rp@gmail.com>
" Description: Code for ignoring linters. Only loaded and if configured.

" Given a filetype and a configuration for ignoring linters, return a List of
" Strings for linter names to ignore.
function! ale#engine#ignore#GetList(filetype, config) abort
    if type(a:config) is v:t_list
        return a:config
    endif

    if type(a:config) is v:t_dict
        let l:names_to_remove = []

        for l:part in split(a:filetype , '\.')
            call extend(l:names_to_remove, get(a:config, l:part, []))
        endfor

        return l:names_to_remove
    endif

    return []
endfunction

" Given a List of linter descriptions, exclude the linters to be ignored.
function! ale#engine#ignore#Exclude(filetype, all_linters, config) abort
    let l:names_to_remove = ale#engine#ignore#GetList(a:filetype, a:config)
    let l:filtered_linters = []

    for l:linter in a:all_linters
        let l:name_list = [l:linter.name] + l:linter.aliases
        let l:should_include = 1

        for l:name in l:name_list
            if index(l:names_to_remove, l:name) >= 0
                let l:should_include = 0
                break
            endif
        endfor

        if l:should_include
            call add(l:filtered_linters, l:linter)
        endif
    endfor

    return l:filtered_linters
endfunction
