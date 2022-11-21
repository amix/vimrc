" Author: w0rp <devw0rp@gmail.com>
" Description: Error handling for the format GHC outputs.
"
function! ale#handlers#haskell#GetStackExecutable(bufnr) abort
    if ale#path#FindNearestFile(a:bufnr, 'stack.yaml') isnot# ''
        return 'stack'
    endif

    " if there is no stack.yaml file, we don't use stack even if it exists,
    " so we return '', because executable('') apparently always fails
    return ''
endfunction

" Remember the directory used for temporary files for Vim.
let s:temp_dir = fnamemodify(ale#util#Tempname(), ':h')
" Build part of a regular expression for matching ALE temporary filenames.
let s:temp_regex_prefix =
\   '\M'
\   . substitute(s:temp_dir, '\\', '\\\\', 'g')
\   . '\.\{-}'

function! s:PanicOutput(lines) abort
    return [{
    \   'lnum': 1,
    \   'col': 1,
    \   'text': 'ghc panic!',
    \   'type': 'E',
    \   'detail' : join(a:lines, "\n"),
    \}]
endfunction

function! ale#handlers#haskell#HandleGHCFormat(buffer, lines) abort
    " Look for lines like the following.
    "
    "Appoint/Lib.hs:8:1: warning:
    "Appoint/Lib.hs:8:1:
    let l:basename = expand('#' . a:buffer . ':t')
    " Build a complete regular expression for replacing temporary filenames
    " in Haskell error messages with the basename for this file.
    let l:temp_filename_regex = s:temp_regex_prefix . l:basename

    let l:pattern = '\v^\s*([a-zA-Z]?:?[^:]+):(\d+):(\d+):(.*)?$'
    let l:output = []

    let l:corrected_lines = []

    " If ghc panic error, put the whole message in details and exit.
    let l:panic_position = match(a:lines,'ghc: panic!')
    let l:panic_end = match(a:lines,'Please report this as a GHC bug:')

    if l:panic_position >= 0
        return s:PanicOutput(a:lines[l:panic_position : l:panic_end])
    endif

    " Group the lines into smaller lists.
    for l:line in a:lines
        if len(matchlist(l:line, l:pattern)) > 0
            call add(l:corrected_lines, [l:line])
        elseif l:line is# ''
            call add(l:corrected_lines, [l:line])
        elseif len(l:corrected_lines) > 0
            call add(l:corrected_lines[-1], l:line)
        endif
    endfor

    for l:line_list in l:corrected_lines
        " Join the smaller lists into one large line to parse.
        let l:line = l:line_list[0]

        for l:extra_line in l:line_list[1:]
            let l:line .= substitute(l:extra_line, '\v^\s+', ' ', '')
        endfor

        let l:match = matchlist(l:line, l:pattern)

        if len(l:match) == 0
            continue
        endif

        if !ale#path#IsBufferPath(a:buffer, l:match[1])
            continue
        endif

        let l:errors = matchlist(l:match[4], '\v([wW]arning|[eE]rror): ?(.*)')

        if len(l:errors) > 0
            let l:ghc_type = l:errors[1]
            let l:text = l:errors[2]
        else
            let l:ghc_type = ''
            let l:text = l:match[4][:0] is# ' ' ? l:match[4][1:] : l:match[4]
        endif

        if l:ghc_type is? 'Warning'
            let l:type = 'W'
        else
            let l:type = 'E'
        endif

        " Replace temporary filenames in problem messages with the basename
        let l:text = substitute(l:text, l:temp_filename_regex, l:basename, 'g')

        let l:item = {
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:text,
        \   'type': l:type,
        \}

        " Include extra lines as details if they are there.
        if len(l:line_list) > 1
            let l:item.detail = join(l:line_list[1:], "\n")
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction
