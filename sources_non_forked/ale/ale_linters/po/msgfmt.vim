" Author: Cian Butler https://github.com/butlerx
" Description: msgfmt for PO files

function! ale_linters#po#msgfmt#Handle(buffer, lines) abort
    let l:results = ale#handlers#unix#HandleAsWarning(a:buffer, a:lines)
    let l:index = 0

    for l:item in l:results
        if l:index > 0 && l:item.text =~? 'this is the location of the first definition'
            let l:last_item = l:results[l:index - 1]

            if l:last_item.text =~? 'duplicate message definition'
                let l:last_item.text = 'duplicate of message at line ' . l:item.lnum
                let l:item.text = 'first location of duplicate of message at line ' . l:last_item.lnum
            endif
        endif

        let l:index += 1
    endfor

    return l:results
endfunction

call ale#linter#Define('po', {
\   'name': 'msgfmt',
\   'executable': 'msgfmt',
\   'output_stream': 'stderr',
\   'command': 'msgfmt --statistics --output-file=- %t',
\   'callback': 'ale_linters#po#msgfmt#Handle',
\})
