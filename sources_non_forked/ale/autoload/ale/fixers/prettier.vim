" Author: tunnckoCore (Charlike Mike Reagent) <mameto2011@gmail.com>,
"         w0rp <devw0rp@gmail.com>, morhetz (Pavel Pertsev) <morhetz@gmail.com>
" Description: Integration of Prettier with ALE.

call ale#Set('javascript_prettier_executable', 'prettier')
call ale#Set('javascript_prettier_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_prettier_options', '')

function! ale#fixers#prettier#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_prettier', [
    \   'node_modules/.bin/prettier_d',
    \   'node_modules/prettier-cli/index.js',
    \   'node_modules/.bin/prettier',
    \])
endfunction

function! ale#fixers#prettier#Fix(buffer) abort
    return ale#semver#RunWithVersionCheck(
    \   a:buffer,
    \   ale#fixers#prettier#GetExecutable(a:buffer),
    \   '%e --version',
    \   function('ale#fixers#prettier#ApplyFixForVersion'),
    \)
endfunction

function! ale#fixers#prettier#ProcessPrettierDOutput(buffer, output) abort
    " If the output is an error message, don't use it.
    for l:line in a:output[:10]
        if l:line =~# '^\w*Error:'
            return []
        endif
    endfor

    return a:output
endfunction

function! ale#fixers#prettier#ApplyFixForVersion(buffer, version) abort
    let l:executable = ale#fixers#prettier#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'javascript_prettier_options')
    let l:parser = ''

    " Append the --parser flag depending on the current filetype (unless it's
    " already set in g:javascript_prettier_options).
    if empty(expand('#' . a:buffer . ':e')) && match(l:options, '--parser') == -1
        " Mimic Prettier's defaults. In cases without a file extension or
        " filetype (scratch buffer), Prettier needs `parser` set to know how
        " to process the buffer.
        if ale#semver#GTE(a:version, [1, 16, 0])
            let l:parser = 'babel'
        else
            let l:parser = 'babylon'
        endif

        let l:prettier_parsers = {
        \    'typescript': 'typescript',
        \    'css': 'css',
        \    'less': 'less',
        \    'scss': 'scss',
        \    'json': 'json',
        \    'json5': 'json5',
        \    'graphql': 'graphql',
        \    'markdown': 'markdown',
        \    'vue': 'vue',
        \    'yaml': 'yaml',
        \    'html': 'html',
        \}

        for l:filetype in split(getbufvar(a:buffer, '&filetype'), '\.')
            if has_key(l:prettier_parsers, l:filetype)
                let l:parser = l:prettier_parsers[l:filetype]
                break
            endif
        endfor
    endif

    if !empty(l:parser)
        let l:options = (!empty(l:options) ? l:options . ' ' : '') . '--parser ' . l:parser
    endif

    " Special error handling needed for prettier_d
    if l:executable =~# 'prettier_d$'
        return {
        \   'command': ale#path#BufferCdString(a:buffer)
        \       . ale#Escape(l:executable)
        \       . (!empty(l:options) ? ' ' . l:options : '')
        \       . ' --stdin-filepath %s --stdin',
        \   'process_with': 'ale#fixers#prettier#ProcessPrettierDOutput',
        \}
    endif

    " 1.4.0 is the first version with --stdin-filepath
    if ale#semver#GTE(a:version, [1, 4, 0])
        return {
        \   'command': ale#path#BufferCdString(a:buffer)
        \       . ale#Escape(l:executable)
        \       . (!empty(l:options) ? ' ' . l:options : '')
        \       . ' --stdin-filepath %s --stdin',
        \}
    endif

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' %t'
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --write',
    \   'read_temporary_file': 1,
    \}
endfunction
