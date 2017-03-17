" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     https://github.com/tomtom
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2015-11-23
" @Revision:    134


if !exists('g:tlib#trace#backtrace')
    " The length of the backtrace that should be included in 
    " |tlib#trace#Print()|.
    let g:tlib#trace#backtrace = 2   "{{{2
endif


if !exists('g:tlib#trace#printf')
    " The command used for printing traces from |tlib#trace#Print()|.
    let g:tlib#trace#printf = 'echom %s'   "{{{2
endif


let s:trace_hl = {'error': 'ErrorMsg', 'fatal': 'ErrorMsg', 'warning': 'WarningMsg'}


" Set |g:tlib#trace#printf| to make |tlib#trace#Print()| print to 
" `filename`.
function! tlib#trace#PrintToFile(filename) abort "{{{3
    let g:tlib#trace#printf = 'call writefile([%s], '. string(a:filename) .', "a")'
endf


" Set the tracing |regexp|. See |:Tlibtrace|.
" This will also call |tlib#trace#Enable()|.
"
" Examples:
"   call tlib#trace#Set(["+foo", "-bar"])
"   call tlib#trace#Set("+foo,-bar")
function! tlib#trace#Set(vars) abort "{{{3
    call tlib#trace#Enable()
    if type(a:vars) == 1
        let vars = tlib#string#SplitCommaList(a:vars, '[,[:space:]]\+')
    else
        let vars = a:vars
    endif
    for rx in vars
        let rx1 = substitute(rx, '^[+-]', '', 'g')
        if rx1 !~# '^\%(error\|fatal\)$' && s:trace_rx !~# '[(|]'. tlib#rx#Escape(rx1) .'\\'
            " TLogVAR rx, rx1
            if rx =~ '^+'
                let s:trace_rx = substitute(s:trace_rx, '\ze\\)\$', '\\|'. tlib#rx#EscapeReplace(rx1), '')
            elseif rx =~ '^-'
                let s:trace_rx = substitute(s:trace_rx, '\\|'. tlib#rx#Escape(rx1), '', '')
            else
                echohl WarningMsg
                echom 'tlib#trace#Print: Unsupported syntax:' rx
                echohl NONE
            endif
        endif
    endfor
    echom "SetTrace:" s:trace_rx
endf


function! tlib#trace#Backtrace(caller) abort "{{{3
    let caller = split(a:caller, '\.\.')
    let start  = max([0, len(caller) - g:tlib#trace#backtrace - 1])
    let caller = caller[start : -1]
    return join(caller, '..')
endf


" Print the values of vars. The first value is a "guard" (see 
" |:Tlibtrace|).
function! tlib#trace#Print(caller, vars, values) abort "{{{3
    let msg = ['TRACE']
    let guard = a:values[0]
    if type(guard) == 0
        let cond = guard
    else
        let cond = guard =~# s:trace_rx
    endif
    " TLogVAR guard, cond, a:vars, a:values
    if cond
        call add(msg, guard)
        call add(msg, tlib#time#FormatNow() .':')
        if g:tlib#trace#backtrace > 0
            let bt = tlib#trace#Backtrace(a:caller)
            if !empty(bt)
                call add(msg, bt .':')
            endif
        endif
        for i in range(1, len(a:vars) - 1)
            let v = substitute(a:vars[i], ',$', '', '')
            let r = string(a:values[i])
            call add(msg, v .'='. r .';')
        endfor
        exec printf(g:tlib#trace#printf, string(join(msg)))
    endif
endf


" Enable tracing via |:Tlibtrace|.
function! tlib#trace#Enable() abort "{{{3
    if !exists('s:trace_rx')
        let s:trace_rx = '^\%(error\)$'
        " :nodoc:
        command! -nargs=+ -bar Tlibtrace call tlib#trace#Print(expand('<sfile>'), [<f-args>], [<args>])
    endif
endf


" Disable tracing via |:Tlibtrace|.
function! tlib#trace#Disable() abort "{{{3
    " :nodoc:
    command! -nargs=+ -bang -bar Tlibtrace :
    unlet! s:trace_rx
endf

