" Author: w0rp <devw0rp@gmail.com>
" Description: Functions for working with eslint, for checking or fixing files.

let s:sep = has('win32') ? '\' : '/'

call ale#Set('javascript_eslint_options', '')
call ale#Set('javascript_eslint_executable', 'eslint')
call ale#Set('javascript_eslint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_eslint_suppress_eslintignore', 0)
call ale#Set('javascript_eslint_suppress_missing_config', 0)

function! ale#handlers#eslint#FindConfig(buffer) abort
    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        for l:basename in [
        \   '.eslintrc.js',
        \   '.eslintrc.yaml',
        \   '.eslintrc.yml',
        \   '.eslintrc.json',
        \   '.eslintrc',
        \]
            let l:config = ale#path#Simplify(join([l:path, l:basename], s:sep))

            if filereadable(l:config)
                return l:config
            endif
        endfor
    endfor

    return ale#path#FindNearestFile(a:buffer, 'package.json')
endfunction

function! ale#handlers#eslint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_eslint', [
    \   'node_modules/.bin/eslint_d',
    \   'node_modules/eslint/bin/eslint.js',
    \   'node_modules/.bin/eslint',
    \])
endfunction

function! ale#handlers#eslint#GetCommand(buffer) abort
    let l:executable = ale#handlers#eslint#GetExecutable(a:buffer)

    let l:options = ale#Var(a:buffer, 'javascript_eslint_options')

    " ESLint 6 loads plugins/configs/parsers from the project root
    " By default, the project root is simply the CWD of the running process.
    " https://github.com/eslint/rfcs/blob/master/designs/2018-simplified-package-loading/README.md
    " https://github.com/dense-analysis/ale/issues/2787
    " Identify project root from presence of node_modules dir.
    " Note: If node_modules not present yet, can't load local deps anyway.
    let l:modules_dir = ale#path#FindNearestDirectory(a:buffer, 'node_modules')
    let l:project_dir = !empty(l:modules_dir) ? fnamemodify(l:modules_dir, ':h:h') : ''
    let l:cd_command = !empty(l:project_dir) ? ale#path#CdString(l:project_dir) : ''

    return l:cd_command
    \   . ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' -f json --stdin --stdin-filename %s'
endfunction

function! s:AddHintsForTypeScriptParsingErrors(output) abort
    for l:item in a:output
        let l:item.text = substitute(
        \   l:item.text,
        \   '^\(Parsing error\)',
        \   '\1 (You may need configure typescript-eslint-parser)',
        \   '',
        \)
    endfor
endfunction

function! s:CheckForBadConfig(buffer, lines) abort
    let l:config_error_pattern = '\v^ESLint couldn''t find a configuration file'
    \   . '|^Cannot read config file'
    \   . '|^.*Configuration for rule .* is invalid'
    \   . '|^ImportDeclaration should appear'

    " Look for a message in the first few lines which indicates that
    " a configuration file couldn't be found.
    for l:line in a:lines[:10]
        let l:match = matchlist(l:line, l:config_error_pattern)

        if len(l:match) > 0
            " Don't show the missing config error if we've disabled it.
            if ale#Var(a:buffer, 'javascript_eslint_suppress_missing_config')
            \&& l:match[0] is# 'ESLint couldn''t find a configuration file'
                return 0
            endif

            return 1
        endif
    endfor

    return 0
endfunction

function! s:parseJSON(buffer, lines) abort
    let l:parsed = []

    for l:line in a:lines
        try
            let l:parsed = extend(l:parsed, json_decode(l:line))
        catch
        endtry
    endfor

    if type(l:parsed) != v:t_list || empty(l:parsed)
        return []
    endif

    let l:errors = l:parsed[0]['messages']

    if empty(l:errors)
        return []
    endif

    let l:output = []

    for l:error in l:errors
        let l:obj = ({
        \   'lnum': get(l:error, 'line', 0),
        \   'text': get(l:error, 'message', ''),
        \   'type': 'E',
        \})

        if get(l:error, 'severity', 0) is# 1
            let l:obj.type = 'W'
        endif

        if has_key(l:error, 'ruleId')
            let l:code = l:error['ruleId']

            " Sometimes ESLint returns null here
            if !empty(l:code)
                let l:obj.code = l:code
            endif
        endif

        if has_key(l:error, 'column')
            let l:obj.col = l:error['column']
        endif

        if has_key(l:error, 'endColumn')
            let l:obj.end_col = l:error['endColumn'] - 1
        endif

        if has_key(l:error, 'endLine')
            let l:obj.end_lnum = l:error['endLine']
        endif

        call add(l:output, l:obj)
    endfor

    return l:output
endfunction

let s:col_end_patterns = [
\   '\vParsing error: Unexpected token (.+) ?',
\   '\v''(.+)'' is not defined.',
\   '\v%(Unexpected|Redundant use of) [''`](.+)[''`]',
\   '\vUnexpected (console) statement',
\]

function! s:parseLines(buffer, lines) abort
    " Matches patterns line the following:
    "
    " /path/to/some-filename.js:47:14: Missing trailing comma. [Warning/comma-dangle]
    " /path/to/some-filename.js:56:41: Missing semicolon. [Error/semi]
    let l:pattern = '^.*:\(\d\+\):\(\d\+\): \(.\+\) \[\(.\+\)\]$'
    " This second pattern matches lines like the following:
    "
    " /path/to/some-filename.js:13:3: Parsing error: Unexpected token
    let l:parsing_pattern = '^.*:\(\d\+\):\(\d\+\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, [l:pattern, l:parsing_pattern])
        let l:text = l:match[3]

        let l:obj = {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:text,
        \   'type': 'E',
        \}

        " Take the error type from the output if available.
        let l:split_code = split(l:match[4], '/')

        if get(l:split_code, 0, '') is# 'Warning'
            let l:obj.type = 'W'
        endif

        " The code can be something like 'Error/foo/bar', or just 'Error'
        if !empty(get(l:split_code, 1))
            let l:obj.code = join(l:split_code[1:], '/')
        endif

        for l:col_match in ale#util#GetMatches(l:text, s:col_end_patterns)
            let l:obj.end_col = l:obj.col + len(l:col_match[1]) - 1
        endfor

        call add(l:output, l:obj)
    endfor

    return l:output
endfunction

function! s:FilterResult(buffer, obj) abort
    if ale#Var(a:buffer, 'javascript_eslint_suppress_eslintignore')
        if a:obj.text =~# '^File ignored'
            return 0
        endif
    endif

    if has_key(a:obj, 'code') && a:obj.code is# 'no-trailing-spaces'
    \&& !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
        return 0
    endif

    return 1
endfunction

function! s:HandleESLintOutput(buffer, lines, type) abort
    if s:CheckForBadConfig(a:buffer, a:lines)
        return [{
        \   'lnum': 1,
        \   'text': 'eslint configuration error (type :ALEDetail for more information)',
        \   'detail': join(a:lines, "\n"),
        \}]
    endif

    if a:lines == ['Could not connect']
        return [{
        \   'lnum': 1,
        \   'text': 'Could not connect to eslint_d. Try updating eslint_d or killing it.',
        \}]
    endif

    if a:type is# 'json'
        let l:output = s:parseJSON(a:buffer, a:lines)
    else
        let l:output = s:parseLines(a:buffer, a:lines)
    endif

    call filter(l:output, {idx, obj -> s:FilterResult(a:buffer, obj)})

    if expand('#' . a:buffer . ':t') =~? '\.tsx\?$'
        call s:AddHintsForTypeScriptParsingErrors(l:output)
    endif

    return l:output
endfunction

function! ale#handlers#eslint#HandleJSON(buffer, lines) abort
    return s:HandleESLintOutput(a:buffer, a:lines, 'json')
endfunction

function! ale#handlers#eslint#Handle(buffer, lines) abort
    return s:HandleESLintOutput(a:buffer, a:lines, 'lines')
endfunction
