function! ale#handlers#deadnix#Handle(buffer, lines) abort
    let l:output = []

    for l:line in a:lines
        try
            let l:file = ale#util#FuzzyJSONDecode(l:line, v:null)
        catch
            continue
        endtry

        if type(l:file) isnot v:t_dict
            continue
        endif

        for l:error in l:file['results']
            try
                let l:ale_error = {
                \   'lnum': l:error['line'],
                \   'col': l:error['column'],
                \   'end_col': l:error['endColumn'],
                \   'text': l:error['message'],
                \   'type': 'W',
                \}
            catch
                continue
            endtry

            call add(l:output, l:ale_error)
        endfor
    endfor

    return l:output
endfunction
