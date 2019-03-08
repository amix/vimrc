" Author: Eddie Lebow https://github.com/elebow
" Description: Reek, a code smell detector for Ruby files

call ale#Set('ruby_reek_show_context', 0)
call ale#Set('ruby_reek_show_wiki_link', 0)
call ale#Set('ruby_reek_options', '')
call ale#Set('ruby_reek_executable', 'reek')

function! ale_linters#ruby#reek#VersionCheck(buffer) abort
    " If we have previously stored the version number in a cache, then
    " don't look it up again.
    if ale#semver#HasVersion('reek')
        " Returning an empty string skips this command.
        return ''
    endif

    let l:executable = ale#Var(a:buffer, 'ruby_reek_executable')

    return ale#handlers#ruby#EscapeExecutable(l:executable, 'reek')
    \   . ' --version'
endfunction

function! ale_linters#ruby#reek#GetCommand(buffer, version_output) abort
    let l:version = ale#semver#GetVersion('reek', a:version_output)
    let l:executable = ale#Var(a:buffer, 'ruby_reek_executable')

    " Tell reek what the filename is if the version of reek is new enough.
    let l:display_name_args = ale#semver#GTE(l:version, [5, 0, 0])
    \   ? ' --stdin-filename %s'
    \   : ''

    return ale#handlers#ruby#EscapeExecutable(l:executable, 'reek')
    \   . ' -f json --no-progress --no-color --force-exclusion'
    \   . l:display_name_args
endfunction

function! s:BuildText(buffer, error) abort
    let l:parts = []

    if ale#Var(a:buffer, 'ruby_reek_show_context')
        call add(l:parts, a:error.context)
    endif

    call add(l:parts, a:error.message)

    if ale#Var(a:buffer, 'ruby_reek_show_wiki_link')
        call add(l:parts, '[' . a:error.wiki_link . ']')
    endif

    return join(l:parts, ' ')
endfunction

function! ale_linters#ruby#reek#Handle(buffer, lines) abort
    let l:output = []

    for l:error in ale#util#FuzzyJSONDecode(a:lines, [])
        for l:location in l:error.lines
            call add(l:output, {
            \    'lnum': l:location,
            \    'type': 'W',
            \    'text': s:BuildText(a:buffer, l:error),
            \    'code': l:error.smell_type,
            \})
        endfor
    endfor

    return l:output
endfunction

call ale#linter#Define('ruby', {
\   'name': 'reek',
\   'executable': {b -> ale#Var(b, 'ruby_reek_executable')},
\   'command_chain': [
\       {'callback': 'ale_linters#ruby#reek#VersionCheck'},
\       {'callback': 'ale_linters#ruby#reek#GetCommand'},
\   ],
\    'callback': 'ale_linters#ruby#reek#Handle',
\})
