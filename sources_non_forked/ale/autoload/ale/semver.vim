let s:version_cache = {}

" Reset the version cache used for parsing the version.
function! ale#semver#ResetVersionCache() abort
    let s:version_cache = {}
endfunction

" Given an executable name and some lines of output, which can be empty,
" parse the version from the lines of output, or return the cached version
" triple [major, minor, patch]
"
" If the version cannot be found, an empty List will be returned instead.
function! ale#semver#GetVersion(executable, version_lines) abort
    let l:version = get(s:version_cache, a:executable, [])

    for l:line in a:version_lines
        let l:match = matchlist(l:line, '\v(\d+)\.(\d+)\.(\d+)')

        if !empty(l:match)
            let l:version = [l:match[1] + 0, l:match[2] + 0, l:match[3] + 0]
            let s:version_cache[a:executable] = l:version

            break
        endif
    endfor

    return l:version
endfunction

" Return 1 if the semver version has been cached for a given executable.
function! ale#semver#HasVersion(executable) abort
    return has_key(s:version_cache, a:executable)
endfunction

" Given two triples of integers [major, minor, patch], compare the triples
" and return 1 if the LHS is greater than or equal to the RHS.
"
" Pairs of [major, minor] can also be used for either argument.
"
" 0 will be returned if the LHS is an empty List.
function! ale#semver#GTE(lhs, rhs) abort
    if empty(a:lhs)
        return 0
    endif

    if a:lhs[0] > a:rhs[0]
        return 1
    elseif a:lhs[0] == a:rhs[0]
        if a:lhs[1] > a:rhs[1]
            return 1
        elseif a:lhs[1] == a:rhs[1]
            return get(a:lhs, 2) >= get(a:rhs, 2)
        endif
    endif

    return 0
endfunction
