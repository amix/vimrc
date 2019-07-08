" Author: Jesse Harris - https://github.com/zigford
" Description: This file adds support for powershell scripts synatax errors

call ale#Set('powershell_powershell_executable', 'pwsh')

function! ale_linters#powershell#powershell#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'powershell_powershell_executable')
endfunction

" Some powershell magic to show syntax errors without executing the script
" thanks to keith hill:
" https://rkeithhill.wordpress.com/2007/10/30/powershell-quicktip-preparsing-scripts-to-check-for-syntax-errors/
function! ale_linters#powershell#powershell#GetCommand(buffer) abort
    let l:script = ['Param($Script);
    \   trap {$_;continue} & {
    \   $Contents = Get-Content -Path $Script;
    \   $Contents = [string]::Join([Environment]::NewLine, $Contents);
    \   [void]$ExecutionContext.InvokeCommand.NewScriptBlock($Contents);
    \   };']

    return ale#powershell#RunPowerShell(
    \   a:buffer, 'powershell_powershell', l:script)
endfunction

" Parse powershell error output using regex into a list of dicts
function! ale_linters#powershell#powershell#Handle(buffer, lines) abort
    let l:output = []
    " Our 3 patterns we need to scrape the data for the dicts
    let l:patterns = [
    \   '\v^At line:(\d+) char:(\d+)',
    \   '\v^(At|\+| )@!.*',
    \   '\vFullyQualifiedErrorId : (\w+)',
    \]

    let l:matchcount = 0

    for l:match in ale#util#GetMatches(a:lines, l:patterns)
        " We want to work with 3 matches per syntax error
        let l:matchcount = l:matchcount + 1

        if l:matchcount == 1 || str2nr(l:match[1])
            " First match consists of 2 capture groups, and
            " can capture the line and col
            if exists('l:item')
                " We may be here because the last syntax
                " didn't emit a code, and so only had 2
                " matches
                call add(l:output, l:item)
                let l:matchcount = 1
            endif

            " If the match is 0, it was a failed match
            " probably due to an unexpected token which
            " contained a newline. Reset matchcount. to
            " continue to the next match
            if !empty(l:match[1])
                let l:item = {
                \   'lnum': str2nr(l:match[1]),
                \   'col': str2nr(l:match[2]),
                \   'type': 'E',
                \}
            else
                let l:matchcount = 0
            endif
        elseif l:matchcount == 2
            " Second match[0] grabs the full line in order
            " to handles the text
            let l:item['text'] = l:match[0]
        else
            " Final match handles the code, however
            " powershell only emits 1 code for all errors
            " so, we get the final code on the last error
            " and loop over the previously added items to
            " append the code we now know
            call add(l:output, l:item)
            unlet l:item

            if len(l:match[1]) > 0
                for l:i in l:output
                    let l:i['code'] = l:match[1]
                endfor
            endif

            " Reset the matchcount so we can begin gathering
            " matches for the next syntax error
            let l:matchcount = 0
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('powershell', {
\   'name': 'powershell',
\   'executable': function('ale_linters#powershell#powershell#GetExecutable'),
\   'command': function('ale_linters#powershell#powershell#GetCommand'),
\   'output_stream': 'stdout',
\   'callback': 'ale_linters#powershell#powershell#Handle',
\})
