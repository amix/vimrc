call ale#Set('ruby_steep_executable', 'steep')
call ale#Set('ruby_steep_options', '')

" Find the nearest dir containing a Steepfile
function! ale_linters#ruby#steep#FindRoot(buffer) abort
    for l:name in ['Steepfile']
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

" Rename path relative to root
function! ale_linters#ruby#steep#RelativeToRoot(buffer, path) abort
    let l:separator = has('win32') ? '\' : '/'
    let l:steep_root = ale_linters#ruby#steep#FindRoot(a:buffer)

    " path isn't under root
    if l:steep_root is# ''
        return ''
    endif

    let l:steep_root_prefix = l:steep_root . l:separator

    " win32 path separators get interpreted by substitute, escape them
    if has('win32')
        let l:steep_root_pat = substitute(l:steep_root_prefix, '\\', '\\\\', 'g')
    else
        let l:steep_root_pat = l:steep_root_prefix
    endif

    return substitute(a:path, l:steep_root_pat, '', '')
endfunction

function! ale_linters#ruby#steep#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_steep_executable')

    " steep check needs to apply some config from the file path so:
    " - steep check can't use stdin (no path)
    " - steep check can't use %t (path outside of project)
    " => we can only use %s

    " somehow :ALEInfo shows that ALE still appends '< %t' to the command
    " => luckily steep check ignores stdin

    " somehow steep has a problem with absolute path to file but a path
    " relative to Steepfile directory works:
    " see https://github.com/soutaro/steep/pull/975
    " => change to Steepfile directory and remove leading path

    let l:buffer_filename = fnamemodify(bufname(a:buffer), ':p')
    let l:buffer_filename = fnameescape(l:buffer_filename)

    let l:relative = ale_linters#ruby#steep#RelativeToRoot(a:buffer, l:buffer_filename)

    " if file is not under steep root, steep can't type check
    if l:relative is# ''
        " don't execute
        return ''
    endif

    return ale#ruby#EscapeExecutable(l:executable, 'steep')
    \   . ' check '
    \   . ale#Var(a:buffer, 'ruby_steep_options')
    \   . ' ' . fnameescape(l:relative)
endfunction

function! ale_linters#ruby#steep#GetType(severity) abort
    if a:severity is? 'information'
    \|| a:severity is? 'hint'
        return 'I'
    endif

    if a:severity is? 'warning'
        return 'W'
    endif

    return 'E'
endfunction

" Handle output from steep
function! ale_linters#ruby#steep#HandleOutput(buffer, lines) abort
    let l:output = []

    let l:in = 0
    let l:item = {}

    for l:line in a:lines
        " Look for first line of a message block
        " If not in-message (l:in == 0) that's expected
        " If in-message (l:in > 0) that's less expected but let's recover
        let l:match = matchlist(l:line, '^\([^:]*\):\([0-9]*\):\([0-9]*\): \[\([^]]*\)\] \(.*\)')

        if len(l:match) > 0
            " Something is lingering: recover by pushing what is there
            if len(l:item) > 0
                call add(l:output, l:item)
                let l:item = {}
            endif

            let l:filename = l:match[1]

            " Steep's reported column is offset by 1 (zero-indexed?)
            let l:item = {
            \   'lnum': l:match[2] + 0,
            \   'col': l:match[3] + 1,
            \   'type': ale_linters#ruby#steep#GetType(l:match[4]),
            \   'text': l:match[5],
            \}

            " Done with this line, mark being in-message and go on with next line
            let l:in = 1
            continue
        endif

        " We're past the first line of a message block
        if l:in > 0
            " Look for code in subsequent lines of the message block
            if l:line =~# '^│ Diagnostic ID:'
                let l:match = matchlist(l:line, '^│ Diagnostic ID: \(.*\)')

                if len(l:match) > 0
                    let l:item.code = l:match[1]
                endif

                " Done with the line
                continue
            endif

            " Look for last line of the message block
            if l:line =~# '^└'
                " Done with the line, mark looking for underline and go on with the next line
                let l:in = 2
                continue
            endif

            " Look for underline right after last line
            if l:in == 2
                let l:match = matchlist(l:line, '\([~][~]*\)')

                if len(l:match) > 0
                    let l:item.end_col = l:item['col'] + len(l:match[1]) - 1
                endif

                call add(l:output, l:item)

                " Done with the line, mark looking for first line and go on with the next line
                let l:in = 0
                let l:item = {}
                continue
            endif
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('ruby', {
\   'name': 'steep',
\   'executable': {b -> ale#Var(b, 'ruby_steep_executable')},
\   'language': 'ruby',
\   'command': function('ale_linters#ruby#steep#GetCommand'),
\   'project_root': function('ale_linters#ruby#steep#FindRoot'),
\   'callback': 'ale_linters#ruby#steep#HandleOutput',
\})
