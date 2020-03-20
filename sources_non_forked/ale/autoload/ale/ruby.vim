" Author: Eddie Lebow https://github.com/elebow
" Description: Functions for integrating with Ruby tools

" Find the nearest dir containing "app", "db", and "config", and assume it is
" the root of a Rails app.
function! ale#ruby#FindRailsRoot(buffer) abort
    for l:name in ['app', 'config', 'db']
        let l:dir = fnamemodify(
        \   ale#path#FindNearestDirectory(a:buffer, l:name),
        \   ':h:h'
        \)

        if l:dir isnot# '.'
        \&& isdirectory(l:dir . '/app')
        \&& isdirectory(l:dir . '/config')
        \&& isdirectory(l:dir . '/db')
            return l:dir
        endif
    endfor

    return ''
endfunction

" Find the nearest dir containing a potential ruby project.
function! ale#ruby#FindProjectRoot(buffer) abort
    let l:dir = ale#ruby#FindRailsRoot(a:buffer)

    if isdirectory(l:dir)
        return l:dir
    endif

    for l:name in ['.solargraph.yml', 'Rakefile', 'Gemfile']
        let l:dir = fnamemodify(
        \   ale#path#FindNearestFile(a:buffer, l:name),
        \   ':h'
        \)

        if l:dir isnot# '.' && isdirectory(l:dir)
            return l:dir
        endif
    endfor

    return ''
endfunction

" Handle output from rubocop and linters that depend on it (e.b. standardrb)
function! ale#ruby#HandleRubocopOutput(buffer, lines) abort
    try
        let l:errors = json_decode(a:lines[0])
    catch
        return []
    endtry

    if !has_key(l:errors, 'summary')
    \|| l:errors['summary']['offense_count'] == 0
    \|| empty(l:errors['files'])
        return []
    endif

    let l:output = []

    for l:error in l:errors['files'][0]['offenses']
        let l:start_col = l:error['location']['column'] + 0
        call add(l:output, {
        \   'lnum': l:error['location']['line'] + 0,
        \   'col': l:start_col,
        \   'end_col': l:start_col + l:error['location']['length'] - 1,
        \   'code': l:error['cop_name'],
        \   'text': l:error['message'],
        \   'type': ale_linters#ruby#rubocop#GetType(l:error['severity']),
        \})
    endfor

    return l:output
endfunction

function! ale#ruby#EscapeExecutable(executable, bundle_exec) abort
    let l:exec_args = a:executable =~? 'bundle'
    \   ? ' exec ' . a:bundle_exec
    \   : ''

    return ale#Escape(a:executable) . l:exec_args
endfunction
