" cmd.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-08-23.
" @Last Change: 2013-05-14.
" @Revision:    0.0.46

if &cp || exists("loaded_tlib_cmd_autoload")
    finish
endif
let loaded_tlib_cmd_autoload = 1


let g:tlib#cmd#last_output = []


function! tlib#cmd#OutputAsList(command) "{{{3
    " TLogVAR a:command
    if exists('s:redir_lines')
        redir END
        let cache = s:redir_lines
    endif
    let s:redir_lines = ''
    redir =>> s:redir_lines
    silent! exec a:command
    redir END
    let g:tlib#cmd#last_output = split(s:redir_lines, '\n')
    unlet s:redir_lines
    if exists('cache')
        let s:redir_lines = cache
        redir =>> s:redir_lines
    endif
    return g:tlib#cmd#last_output
endf


" See |:TBrowseOutput|.
function! tlib#cmd#BrowseOutput(command) "{{{3
    call tlib#cmd#BrowseOutputWithCallback("tlib#cmd#DefaultBrowseOutput", a:command)
endf

" :def: function! tlib#cmd#BrowseOutputWithCallback(callback, command)
" Execute COMMAND and present its output in a |tlib#input#List()|;
" when a line is selected, execute the function named as the CALLBACK
" and pass in that line as an argument.
"
" The CALLBACK function gives you an opportunity to massage the COMMAND output
" and possibly act on it in a meaningful way. For example, if COMMAND listed
" all URIs found in the current buffer, CALLBACK could validate and then open
" the selected URI in the system's default browser.
"
" This function is meant to be a tool to help compose the implementations of
" powerful commands that use |tlib#input#List()| as a common interface. See
" |TBrowseScriptnames| as an example.
"
" EXAMPLES: >
"   call tlib#cmd#BrowseOutputWithCallback('tlib#cmd#ParseScriptname', 'scriptnames')
function! tlib#cmd#BrowseOutputWithCallback(callback, command) "{{{3
    let list = tlib#cmd#OutputAsList(a:command)
    let cmd = tlib#input#List('s', 'Output of: '. a:command, list)
    if !empty(cmd)
        let Callback = function(a:callback)
        call call(Callback, [cmd])
    endif
endf

function! tlib#cmd#DefaultBrowseOutput(cmd) "{{{3
    call feedkeys(':'. a:cmd)
endf

function! tlib#cmd#ParseScriptname(line) "{{{3
    let parsedValue = substitute(a:line, '^.\{-}\/', '/', '')
    exe ':e '. parsedValue
endf

" :def: function! tlib#cmd#UseVertical(?rx='')
" Look at the history whether the command was called with vertical. If 
" an rx is provided check first if the last entry in the history matches 
" this rx.
function! tlib#cmd#UseVertical(...) "{{{3
    TVarArg ['rx']
    let h0 = histget(':')
    let rx0 = '\C\<vert\%[ical]\>\s\+'
    if !empty(rx)
        let rx0 .= '.\{-}'.rx
    endif
    " TLogVAR h0, rx0
    return h0 =~ rx0
endf


" Print the time in seconds or milliseconds (if your version of VIM 
" has |+reltime|) a command takes.
function! tlib#cmd#Time(cmd) "{{{3
    if has('reltime')
        let start = tlib#time#Now()
        exec a:cmd
        let end = tlib#time#Now()
        let diff = string(tlib#time#Diff(end, start)) .'ms'
    else
        let start = localtime()
        exec a:cmd
        let diff = (localtime() - start) .'s'
    endif
    echom 'Time: '. diff .': '. a:cmd
endf

