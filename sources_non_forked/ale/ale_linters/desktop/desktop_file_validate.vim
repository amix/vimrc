call ale#Set('desktop_desktop_file_validate_options', '')

" Example matches for pattern:
"
" foo.desktop: warning: key "TerminalOptions" in group ...
" foo.desktop: error: action "new-private-window" is defined, ...
let s:pattern = '\v^(.+): ([a-z]+): (.+)$'

function! ale_linters#desktop#desktop_file_validate#Handle(buffer, lines) abort
    " The error format doesn't specify lines, so we can just put all of the
    " errors on line 1.
    return ale#util#MapMatches(a:lines, s:pattern, {match -> {
    \   'lnum': 1,
    \   'col': 1,
    \   'type': match[2] is? 'error' ? 'E' : 'W',
    \   'text': match[3],
    \}})
endfunction

call ale#linter#Define('desktop', {
\   'name': 'desktop_file_validate',
\   'aliases': ['desktop-file-validate'],
\   'executable': 'desktop-file-validate',
\   'command': {b ->
\       '%e'
\       . ale#Pad(ale#Var(b, 'desktop_desktop_file_validate_options'))
\       . ' %t'
\   },
\   'callback': 'ale_linters#desktop#desktop_file_validate#Handle',
\   'output_stream': 'both',
\})
