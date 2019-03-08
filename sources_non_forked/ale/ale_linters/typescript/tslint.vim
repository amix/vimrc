" Author: Prashanth Chandra <https://github.com/prashcr>, Jonathan Clem <https://jclem.net>
" Description: tslint for TypeScript files

call ale#handlers#tslint#InitVariables()

function! ale_linters#typescript#tslint#Handle(buffer, lines) abort
    " Do not output any errors for empty files if the option is on.
    if ale#Var(a:buffer, 'typescript_tslint_ignore_empty_files')
    \&& getbufline(a:buffer, 1, '$') == ['']
        return []
    endif

    let l:dir = expand('#' . a:buffer . ':p:h')
    let l:output = []

    for l:error in ale#util#FuzzyJSONDecode(a:lines, [])
        if get(l:error, 'ruleName', '') is# 'no-implicit-dependencies'
            continue
        endif

        let l:item = {
        \   'type': (get(l:error, 'ruleSeverity', '') is# 'WARNING' ? 'W' : 'E'),
        \   'text': l:error.failure,
        \   'lnum': l:error.startPosition.line + 1,
        \   'col': l:error.startPosition.character + 1,
        \   'end_lnum': l:error.endPosition.line + 1,
        \   'end_col': l:error.endPosition.character + 1,
        \}

        let l:filename = ale#path#GetAbsPath(l:dir, l:error.name)

        " Assume temporary files are this file.
        if !ale#path#IsTempName(l:filename)
            let l:item.filename = l:filename
        endif

        if has_key(l:error, 'ruleName')
            let l:item.code = l:error.ruleName
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

function! ale_linters#typescript#tslint#GetCommand(buffer) abort
    let l:tslint_config_path = ale#path#ResolveLocalPath(
    \   a:buffer,
    \   'tslint.json',
    \   ale#Var(a:buffer, 'typescript_tslint_config_path')
    \)
    let l:tslint_config_option = !empty(l:tslint_config_path)
    \   ? ' -c ' . ale#Escape(l:tslint_config_path)
    \   : ''

    let l:tslint_rules_dir = ale#Var(a:buffer, 'typescript_tslint_rules_dir')
    let l:tslint_rules_option = !empty(l:tslint_rules_dir)
    \  ? ' -r ' . ale#Escape(l:tslint_rules_dir)
    \  : ''

    return ale#path#BufferCdString(a:buffer)
    \   . ale#Escape(ale#handlers#tslint#GetExecutable(a:buffer))
    \   . ' --format json'
    \   . l:tslint_config_option
    \   . l:tslint_rules_option
    \   . ' %t'
endfunction

call ale#linter#Define('typescript', {
\   'name': 'tslint',
\   'executable': function('ale#handlers#tslint#GetExecutable'),
\   'command': function('ale_linters#typescript#tslint#GetCommand'),
\   'callback': 'ale_linters#typescript#tslint#Handle',
\})
