" Author: Jesse Harris - https://github.com/zigford
" Description: This file adds support for lintng powershell scripts
"   using the PSScriptAnalyzer module.

" let g:ale_powershell_psscriptanalyzer_exclusions =
" \ 'PSAvoidUsingWriteHost,PSAvoidGlobalVars'
call ale#Set('powershell_psscriptanalyzer_exclusions', '')
call ale#Set('powershell_psscriptanalyzer_executable', 'pwsh')
call ale#Set('powershell_psscriptanalyzer_module',
\ 'psscriptanalyzer')

function! ale_linters#powershell#psscriptanalyzer#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'powershell_psscriptanalyzer_executable')
endfunction

" Run Invoke-ScriptAnalyzer and output each linting message as 4 separate lines
" for each parsing
function! ale_linters#powershell#psscriptanalyzer#GetCommand(buffer) abort
    let l:exclude_option = ale#Var(
    \   a:buffer, 'powershell_psscriptanalyzer_exclusions')
    let l:module = ale#Var(
    \   a:buffer, 'powershell_psscriptanalyzer_module')
    let l:script = ['Param($Script);
    \   Invoke-ScriptAnalyzer "$Script" '
    \   . (!empty(l:exclude_option) ? '-Exclude ' . l:exclude_option : '')
    \   . '| ForEach-Object {
    \   $_.Line;
    \   $_.Severity;
    \   $_.Message;
    \   $_.RuleName}']

    return ale#powershell#RunPowerShell(
    \   a:buffer,
    \   'powershell_psscriptanalyzer',
    \   l:script)
endfunction

" add every 4 lines to an item(Dict) and every item to a list
" return the list
function! ale_linters#powershell#psscriptanalyzer#Handle(buffer, lines) abort
    let l:output = []
    let l:lcount = 0

    for l:line in a:lines
        if l:lcount is# 0
            " the very first line
            let l:item = {'lnum': str2nr(l:line)}
        elseif l:lcount is# 1
            if l:line is# 'Error'
                let l:item['type'] = 'E'
            elseif l:line is# 'Information'
                let l:item['type'] = 'I'
            else
                let l:item['type'] = 'W'
            endif
        elseif l:lcount is# 2
            let l:item['text'] = l:line
        elseif l:lcount is# 3
            let l:item['code'] = l:line
            call add(l:output, l:item)
            let l:lcount = -1
        endif

        let l:lcount = l:lcount + 1
    endfor

    return l:output
endfunction

call ale#linter#Define('powershell', {
\   'name': 'psscriptanalyzer',
\   'executable': function('ale_linters#powershell#psscriptanalyzer#GetExecutable'),
\   'command': function('ale_linters#powershell#psscriptanalyzer#GetCommand'),
\   'output_stream': 'stdout',
\   'callback': 'ale_linters#powershell#psscriptanalyzer#Handle',
\})
