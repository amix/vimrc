" Authors: Trevor Whitney <trevorjwhitney@gmail.com> and Takuya Kosugiyama <re@itkq.jp>
" Description: jsonnetfmt for jsonnet files

call ale#Set('jsonnet_jsonnetfmt_executable', 'jsonnetfmt')
call ale#Set('jsonnet_jsonnetfmt_options', '')

function! ale_linters#jsonnet#jsonnetfmt#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'jsonnet_jsonnetfmt_options')

    return '%e'
    \   . ale#Pad(l:options)
    \   . ' %t'
endfunction


function! ale_linters#jsonnet#jsonnetfmt#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " STATIC ERROR: foo.jsonnet:22:3-12: expected token OPERATOR but got (IDENTIFIER, "bar")
    " STATIC ERROR: hoge.jsonnet:20:3: unexpected: "}" while parsing terminal
    let l:pattern = '^STATIC ERROR:[^:]*:\(\d\+\):\(\d\+\):*\(-\d\+\)* \(.*\)'
    let l:output = []

    for l:line in a:lines
        let l:match = matchlist(l:line, l:pattern)

        if len(l:match) == 0
            continue
        endif

        " vcol is Needed to indicate that the column is a character.
        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': l:match[1] + 0,
        \   'vcol': 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[4],
        \   'type': 'E',
        \   'nr': -1,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('jsonnet', {
\   'name': 'jsonnetfmt',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'jsonnet_jsonnetfmt_executable')},
\   'command': function('ale_linters#jsonnet#jsonnetfmt#GetCommand'),
\   'callback': 'ale_linters#jsonnet#jsonnetfmt#Handle',
\})
