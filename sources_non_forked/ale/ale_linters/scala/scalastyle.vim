" Author: Kevin Kays - https://github.com/okkays
" Description: Support for the scalastyle checker.

call ale#Set('scala_scalastyle_options', '')
" TODO: Remove support for the old option name in ALE 3.0.
call ale#Set('scala_scalastyle_config',
\   get(g:, 'ale_scalastyle_config_loc', '')
\)

function! ale_linters#scala#scalastyle#Handle(buffer, lines) abort
    " Look for help output from scalastyle first, which indicates that no
    " configuration file was found.
    for l:line in a:lines[:10]
        if l:line =~# '-c, --config'
            return [{
            \   'lnum': 1,
            \   'text': '(See :help ale-scala-scalastyle)'
            \       . ' No scalastyle configuration file was found.',
            \}]
        endif
    endfor

    " Matches patterns like the following:
    "
    " warning file=/home/blurble/Doop.scala message=Missing or badly formed ScalaDoc: Extra @param foobles line=190
    let l:patterns = [
    \   '^\(.\+\) .\+ message=\(.\+\) line=\(\d\+\)$',
    \   '^\(.\+\) .\+ message=\(.\+\) line=\(\d\+\) column=\(\d\+\)$',
    \]
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:patterns)
        let l:args = {
        \   'lnum': l:match[3] + 0,
        \   'type': l:match[1] =~? 'error' ? 'E' : 'W',
        \   'text': l:match[2]
        \}

        if !empty(l:match[4])
            let l:args['col'] = l:match[4] + 1
        endif

        call add(l:output, l:args)
    endfor

    return l:output
endfunction

function! ale_linters#scala#scalastyle#GetCommand(buffer) abort
    " Search for scalastyle config in parent directories.
    let l:scalastyle_config = ''
    let l:potential_configs = [
    \   'scalastyle_config.xml',
    \   'scalastyle-config.xml'
    \]

    for l:config in l:potential_configs
        let l:scalastyle_config = ale#path#ResolveLocalPath(
        \   a:buffer,
        \   l:config,
        \   ''
        \)

        if !empty(l:scalastyle_config)
            break
        endif
    endfor

    " If all else fails, try the global config.
    if empty(l:scalastyle_config)
        let l:scalastyle_config = ale#Var(a:buffer, 'scala_scalastyle_config')
    endif

    return 'scalastyle'
    \ . (!empty(l:scalastyle_config) ? ' --config ' . ale#Escape(l:scalastyle_config) : '')
    \ . ale#Pad(ale#Var(a:buffer, 'scala_scalastyle_options'))
    \ . ' %t'
endfunction

call ale#linter#Define('scala', {
\   'name': 'scalastyle',
\   'executable': 'scalastyle',
\   'output_stream': 'stdout',
\   'command': function('ale_linters#scala#scalastyle#GetCommand'),
\   'callback': 'ale_linters#scala#scalastyle#Handle',
\})
