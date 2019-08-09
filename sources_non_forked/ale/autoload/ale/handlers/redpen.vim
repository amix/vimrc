" Author: rhysd https://rhysd.github.io
" Description: Redpen, a proofreading tool (http://redpen.cc)

function! ale#handlers#redpen#HandleRedpenOutput(buffer, lines) abort
    " Only one file was passed to redpen. So response array has only one
    " element.
    let l:res = get(ale#util#FuzzyJSONDecode(a:lines, []), 0, {})
    let l:output = []

    for l:err in get(l:res, 'errors', [])
        let l:item = {
        \   'text': l:err.message,
        \   'type': 'W',
        \   'code': l:err.validator,
        \}

        if has_key(l:err, 'startPosition')
            let l:item.lnum = l:err.startPosition.lineNum
            let l:item.col = l:err.startPosition.offset + 1

            if has_key(l:err, 'endPosition')
                let l:item.end_lnum = l:err.endPosition.lineNum
                let l:item.end_col = l:err.endPosition.offset
            endif
        else
            " Fallback to a whole sentence region when a region is not
            " specified by the error.
            let l:item.lnum = l:err.lineNum
            let l:item.col = l:err.sentenceStartColumnNum + 1
        endif

        " Adjust column number for multibyte string
        let l:line = getline(l:item.lnum)

        if l:line is# ''
            let l:line = l:err.sentence
        endif

        let l:line = split(l:line, '\zs')

        if l:item.col >= 2
            let l:col = 0

            for l:strlen in map(l:line[0:(l:item.col - 2)], 'strlen(v:val)')
                let l:col = l:col + l:strlen
            endfor

            let l:item.col = l:col + 1
        endif

        if has_key(l:item, 'end_col')
            let l:col = 0

            for l:strlen in map(l:line[0:(l:item.end_col - 1)], 'strlen(v:val)')
                let l:col = l:col + l:strlen
            endfor

            let l:item.end_col = l:col
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction
