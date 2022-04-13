function! ale_linters#systemd#systemd_analyze#Handle(buffer, lines) abort
    return ale#util#MapMatches(a:lines, '\v(.+):([0-9]+): (.+)', {match -> {
    \   'lnum': str2nr(match[2]),
    \   'col': 1,
    \   'type': 'W',
    \   'text': match[3],
    \}})
endfunction

call ale#linter#Define('systemd', {
\   'name': 'systemd_analyze',
\   'aliases': ['systemd-analyze'],
\   'executable': 'systemd-analyze',
\   'command': 'SYSTEMD_LOG_COLOR=0 %e --user verify %s',
\   'callback': 'ale_linters#systemd#systemd_analyze#Handle',
\   'output_stream': 'both',
\   'lint_file': 1,
\})
