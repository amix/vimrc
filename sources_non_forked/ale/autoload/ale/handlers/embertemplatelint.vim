" Author: Adrian Zalewski <aazalewski@hotmail.com>
" Description: Ember-template-lint for checking Handlebars files

function! ale#handlers#embertemplatelint#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'handlebars_embertemplatelint', [
    \   'node_modules/.bin/ember-template-lint',
    \])
endfunction

function! ale#handlers#embertemplatelint#GetCommand(buffer, version) abort
    if ale#semver#GTE(a:version, [4, 0, 0])
        " --json was removed in favor of --format=json in ember-template-lint@4.0.0
        return '%e --format=json --filename %s'
    endif

    return '%e --json --filename %s'
endfunction

function! ale#handlers#embertemplatelint#GetCommandWithVersionCheck(buffer) abort
    return ale#semver#RunWithVersionCheck(
    \   a:buffer,
    \   ale#handlers#embertemplatelint#GetExecutable(a:buffer),
    \   '%e --version',
    \   function('ale#handlers#embertemplatelint#GetCommand'),
    \)
endfunction

function! ale#handlers#embertemplatelint#Handle(buffer, lines) abort
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

function! ale#handlers#embertemplatelint#DefineLinter(filetype) abort
    call ale#Set('handlebars_embertemplatelint_executable', 'ember-template-lint')
    call ale#Set('handlebars_embertemplatelint_use_global', get(g:, 'ale_use_global_executables', 0))

    call ale#linter#Define(a:filetype, {
    \   'name': 'embertemplatelint',
    \   'aliases': ['ember-template-lint'],
    \   'executable': function('ale#handlers#embertemplatelint#GetExecutable'),
    \   'command': function('ale#handlers#embertemplatelint#GetCommandWithVersionCheck'),
    \   'callback': 'ale#handlers#embertemplatelint#Handle',
    \})
endfunction


