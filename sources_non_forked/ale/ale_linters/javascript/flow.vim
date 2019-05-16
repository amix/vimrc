" Author: Zach Perrault -- @zperrault
" Author: Florian Beeres <yuuki@protonmail.com>
" Description: FlowType checking for JavaScript files

call ale#Set('javascript_flow_executable', 'flow')
call ale#Set('javascript_flow_use_home_config', 0)
call ale#Set('javascript_flow_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_flow_use_respect_pragma', 1)

function! ale_linters#javascript#flow#GetExecutable(buffer) abort
    let l:flow_config = ale#path#FindNearestFile(a:buffer, '.flowconfig')

    if empty(l:flow_config)
        " Don't run Flow if we can't find a .flowconfig file.
        return ''
    endif

    " Don't run Flow with a configuration file from the home directory by
    " default, which can eat all of your RAM.
    if fnamemodify(l:flow_config, ':h') is? $HOME
    \&& !ale#Var(a:buffer, 'javascript_flow_use_home_config')
        return ''
    endif

    return ale#node#FindExecutable(a:buffer, 'javascript_flow', [
    \   'node_modules/.bin/flow',
    \])
endfunction

function! ale_linters#javascript#flow#VersionCheck(buffer) abort
    let l:executable = ale_linters#javascript#flow#GetExecutable(a:buffer)

    if empty(l:executable)
        return ''
    endif

    return ale#Escape(l:executable) . ' --version'
endfunction

function! ale_linters#javascript#flow#GetCommand(buffer, version_lines) abort
    let l:executable = ale_linters#javascript#flow#GetExecutable(a:buffer)

    if empty(l:executable)
        return ''
    endif

    let l:version = ale#semver#GetVersion(l:executable, a:version_lines)

    " If we can parse the version number, then only use --respect-pragma
    " if the version is >= 0.36.0, which added the argument.
    let l:use_respect_pragma = ale#Var(a:buffer, 'javascript_flow_use_respect_pragma')
    \   && (empty(l:version) || ale#semver#GTE(l:version, [0, 36]))

    return ale#Escape(l:executable)
    \   . ' check-contents'
    \   . (l:use_respect_pragma ? ' --respect-pragma': '')
    \   . ' --json --from ale %s < %t'
    \   . (!has('win32') ? '; echo' : '')
endfunction

" Filter lines of flow output until we find the first line where the JSON
" output starts.
function! s:GetJSONLines(lines) abort
    let l:start_index = 0

    for l:line in a:lines
        if l:line[:0] is# '{'
            break
        endif

        let l:start_index += 1
    endfor

    return a:lines[l:start_index :]
endfunction

function! s:ExtraErrorMsg(current, new) abort
    let l:newMsg = ''

    if a:current is# ''
        " extra messages appear to already have a :
        let l:newMsg = a:new
    else
        let l:newMsg = a:current . ' ' . a:new
    endif

    return l:newMsg
endfunction


function! s:GetDetails(error) abort
    let l:detail = ''

    for l:extra_error in a:error.extra
        if has_key(l:extra_error, 'message')
            for l:extra_message in l:extra_error.message
                let l:detail = s:ExtraErrorMsg(l:detail, l:extra_message.descr)
            endfor
        endif

        if has_key(l:extra_error, 'children')
            for l:child in l:extra_error.children
                for l:child_message in l:child.message
                    let l:detail = l:detail . ' ' . l:child_message.descr
                endfor
            endfor
        endif
    endfor

    return l:detail
endfunction

function! ale_linters#javascript#flow#Handle(buffer, lines) abort
    let l:str = join(s:GetJSONLines(a:lines), '')

    if empty(l:str)
        return []
    endif

    let l:flow_output = json_decode(l:str)
    let l:output = []

    for l:error in get(l:flow_output, 'errors', [])
        " Each error is broken up into parts
        let l:text = ''
        let l:line = 0
        let l:col = 0

        for l:message in l:error.message
            " Comments have no line of column information, so we skip them.
            " In certain cases, `l:message.loc.source` points to a different path
            " than the buffer one, thus we skip this loc information too.
            if has_key(l:message, 'loc')
            \&& l:line is# 0
            \&& ale#path#IsBufferPath(a:buffer, l:message.loc.source)
                let l:line = l:message.loc.start.line + 0
                let l:col = l:message.loc.start.column + 0
            endif

            if l:text is# ''
                let l:text = l:message.descr . ':'
            else
                let l:text = l:text . ' ' . l:message.descr
            endif
        endfor

        if has_key(l:error, 'operation')
            let l:text = l:text . ' See also: ' . l:error.operation.descr
        endif

        let l:errorToAdd = {
        \   'lnum': l:line,
        \   'col': l:col,
        \   'text': l:text,
        \   'type': has_key(l:error, 'level') && l:error.level is# 'error' ? 'E' : 'W',
        \}

        if has_key(l:error, 'extra')
            let l:errorToAdd.detail = l:errorToAdd.text
            \   . "\n" . s:GetDetails(l:error)
        endif

        call add(l:output, l:errorToAdd)
    endfor

    return l:output
endfunction

call ale#linter#Define('javascript', {
\   'name': 'flow',
\   'executable': function('ale_linters#javascript#flow#GetExecutable'),
\   'command_chain': [
\       {'callback': 'ale_linters#javascript#flow#VersionCheck'},
\       {'callback': 'ale_linters#javascript#flow#GetCommand'},
\   ],
\   'callback': 'ale_linters#javascript#flow#Handle',
\   'read_buffer': 0,
\})
