" Author: Adrian Zalewski <aazalewski@hotmail.com>
" Description: Ember-template-lint for checking Handlebars files

call ale#Set('handlebars_embertemplatelint_executable', 'ember-template-lint')
call ale#Set('handlebars_embertemplatelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#handlebars#embertemplatelint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'handlebars_embertemplatelint', [
    \   'node_modules/.bin/ember-template-lint',
    \])
endfunction

function! ale_linters#handlebars#embertemplatelint#GetCommand(buffer, version) abort
    " Reading from stdin was introduced in ember-template-lint@1.6.0
    return ale#semver#GTE(a:version, [1, 6, 0])
    \   ? '%e --json --filename %s'
    \   : '%e --json %t'
endfunction

function! ale_linters#handlebars#embertemplatelint#GetCommandWithVersionCheck(buffer) abort
    return ale#semver#RunWithVersionCheck(
    \   a:buffer,
    \   ale_linters#handlebars#embertemplatelint#GetExecutable(a:buffer),
    \   '%e --version',
    \   function('ale_linters#handlebars#embertemplatelint#GetCommand'),
    \)
endfunction

function! ale_linters#handlebars#embertemplatelint#Handle(buffer, lines) abort
    let l:output = []
    let l:json = ale#util#FuzzyJSONDecode(a:lines, {})

    for l:error in get(values(l:json), 0, [])
        if has_key(l:error, 'fatal')
            call add(l:output, {
            \   'lnum': get(l:error, 'line', 1),
            \   'col': get(l:error, 'column', 1),
            \   'text': l:error.message,
            \   'type': l:error.severity == 1 ? 'W' : 'E',
            \})
        else
            call add(l:output, {
            \   'lnum': l:error.line,
            \   'col': l:error.column,
            \   'text': l:error.rule . ': ' . l:error.message,
            \   'type': l:error.severity == 1 ? 'W' : 'E',
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('handlebars', {
\   'name': 'embertemplatelint',
\   'aliases': ['ember-template-lint'],
\   'executable': function('ale_linters#handlebars#embertemplatelint#GetExecutable'),
\   'command': function('ale_linters#handlebars#embertemplatelint#GetCommandWithVersionCheck'),
\   'callback': 'ale_linters#handlebars#embertemplatelint#Handle',
\})
