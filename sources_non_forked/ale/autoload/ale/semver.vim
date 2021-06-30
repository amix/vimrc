let s:version_cache = {}

" Reset the version cache used for parsing the version.
function! ale#semver#ResetVersionCache() abort
    let s:version_cache = {}
endfunction

function! ale#semver#ParseVersion(version_lines) abort
    for l:line in a:version_lines
        let l:match = matchlist(l:line, '\v(\d+)\.(\d+)(\.(\d+))?')

        if !empty(l:match)
            return [l:match[1] + 0, l:match[2] + 0, l:match[4] + 0]
        endif
    endfor

    return []
endfunction

" Given an executable name and some lines of output, which can be empty,
" parse the version from the lines of output, or return the cached version
" triple [major, minor, patch]
"
" If the version cannot be found, an empty List will be returned instead.
function! s:GetVersion(executable, version_lines) abort
    let l:version = get(s:version_cache, a:executable, [])
    let l:parsed_version = ale#semver#ParseVersion(a:version_lines)

    if !empty(l:parsed_version)
        let l:version = l:parsed_version
        let s:version_cache[a:executable] = l:version
    endif

    return l:version
endfunction

function! ale#semver#RunWithVersionCheck(buffer, executable, command, Callback) abort
    if empty(a:executable)
        return ''
    endif

    let l:cache = s:version_cache

    if has_key(s:version_cache, a:executable)
        return a:Callback(a:buffer, s:version_cache[a:executable])
    endif

    return ale#command#Run(
    \   a:buffer,
    \   a:command,
    \   {_, output -> a:Callback(a:buffer, s:GetVersion(a:executable, output))},
    \   {'output_stream': 'both', 'executable': a:executable}
    \)
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
