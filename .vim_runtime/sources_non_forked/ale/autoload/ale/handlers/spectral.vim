" Author: t2h5 <https://github.com/t2h5>
" Description: Integration of Stoplight Spectral CLI with ALE.

function! ale#handlers#spectral#HandleSpectralOutput(buffer, lines) abort
    " Matches patterns like the following:
    " openapi.yml:1:1 error oas3-schema "Object should have required property `info`."
    " openapi.yml:1:1 warning oas3-api-servers "OpenAPI `servers` must be present and non-empty array."
    let l:pattern = '\v^.*:(\d+):(\d+) (error|warning) (.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:obj = {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:match[3] is# 'error' ? 'E' : 'W',
        \   'text': l:match[4],
        \}

        let l:code_match = matchlist(l:obj.text, '\v^(.+) "(.+)"$')

        if !empty(l:code_match)
            let l:obj.code = l:code_match[1]
            let l:obj.text = l:code_match[2]
        endif

        call add(l:output, l:obj)
    endfor

    return l:output
endfunction

