" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     https://github.com/tomtom
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2017-03-09
" @Revision:    205


if !exists('g:tlib#trace#backtrace')
    " The length of the backtrace that should be included in 
    " |tlib#trace#Print()|.
    let g:tlib#trace#backtrace = 2   "{{{2
endif


if !exists('g:tlib#trace#printer')
    " Possible values:
    "   - 'echom'
    "   - ['file', FILENAME]
    let g:tlib#trace#printer = 'echom'   "{{{2
endif


if !exists('g:tlib#trace#hl')
    let g:tlib#trace#hl = {'error': 'ErrorMsg', 'fatal': 'ErrorMsg', 'warn': 'WarningMsg'}   "{{{2
endif


" Print traces from |tlib#trace#Print()|.
function! tlib#trace#Printer_echom(type, text, args) abort "{{{3
    let hl = get(g:tlib#trace#hl, a:type, '')
    try
        if !empty(hl)
            exec 'echohl' hl
        endif
        echom a:text
    finally
        if !empty(hl)
            echohl NONE
        endif
    endtry
endf


function! tlib#trace#Printer_file(type, text, args) abort "{{{3
    let filename = get(a:args, 0, '')
    if exists(filename) && !filewritable(filename)
        throw 'tlib#trace#Printer_file: Cannot write to file: '. filename
    else
        call writefile([a:text], filename, 'a')
    endif
endf


" Set the tracing |regexp|. See |:Tlibtrace|.
" This will also call |tlib#trace#Enable()|.
"
" Examples:
"   call tlib#trace#Set(["+foo", "-bar"])
"   call tlib#trace#Set("+foo,-bar")
function! tlib#trace#Set(vars, ...) abort "{{{3
    let reset = a:0 >= 1 ? a:1 : 0
    if reset
        call tlib#trace#Reset()
    endif
    if empty(a:vars)
        return
    endif
    call tlib#trace#Enable()
    if type(a:vars) == v:t_string
        let vars = tlib#string#SplitCommaList(a:vars, '[,[:space:]]\+')
        let opts = {}
    elseif type(a:vars) == v:t_dict
        let vars = a:vars.__rest__
        if has_key(a:vars, 'file')
            let g:tlib#trace#printer = ['file', a:vars.file]
        endif
        if has_key(a:vars, 'echo')
            let g:tlib#trace#printer = 'echom'
        endif
    else
        let vars = a:vars
        let opts = {}
    endif
    " TLogVAR vars
    for rx in vars
        let rx1 = substitute(rx, '^[+-]', '', 'g')
        if rx1 !~# '^\%(error\|warn\|fatal\)$'
            let erx1 = tlib#rx#Escape(rx1)
            " TLogVAR rx, rx1
            " echom "DBG" s:trace_rx
            if rx =~ '^-'
                let erx1 .= '\[0-\d\]\\?'
                if s:trace_rx =~# '[(|]'. erx1 .'\\'
                    let s:trace_rx = substitute(s:trace_rx, '\\|'. erx1, '', '')
                endif
                " elseif rx =~ '^+'
            else
                if erx1 =~ '\d$'
                    let erx1 = substitute(erx1, '\d$', '[0-\0]\\?', '')
                else
                    let erx1 .= '[0-9]\?'
                endif
                if s:trace_rx !~# '[(|]'. erx1 .'\\'
                    let s:trace_rx = substitute(s:trace_rx, '\ze\\)\$', '\\|'. escape(erx1, '\'), '')
                endif
                " else
                "     echohl WarningMsg
                "     echom 'tlib#trace#Print: Unsupported syntax:' rx
                "     echohl NONE
            endif
            " echom "DBG" s:trace_rx
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
    " echom "DBG tlib#trace#Print" string(a:vars) string(a:values)
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
        if len(a:vars) == len(a:values)
            for i in range(1, len(a:vars) - 1)
                let v = substitute(a:vars[i], ',$', '', '')
                if type(a:values[i]) == v:t_func
                    let r = string(a:values[i])
                else
                    let r = a:values[i]
                endif
                if v =~# '^\([''"]\).\{-}\1$'
                    call add(msg, r .';')
                else
                    call add(msg, v .'='. string(r) .';')
                endif
                unlet r
            endfor
        else
            call add(msg, join(a:values[1:-1]))
        endif
        if type(g:tlib#trace#printer) == v:t_string
            let printer = g:tlib#trace#printer
            let args = []
        else
            let [printer; args] = g:tlib#trace#printer
        endif
        call tlib#trace#Printer_{printer}(guard, join(msg), args)
    endif
endf


function! tlib#trace#Reset() abort "{{{3
    let s:trace_rx = '^\%(error\|fatal\|warn\|info\)$'
    let g:tlib#trace#printer = 'echom'
endf


" Enable tracing via |:Tlibtrace|.
function! tlib#trace#Enable() abort "{{{3
    if !exists('s:trace_rx')
        call tlib#trace#Reset()
        " :nodoc:
        command! -nargs=+ -bang Tlibtrace call tlib#trace#Print(expand('<sfile>'), [<f-args>], [<args>])
    endif
endf


" Disable tracing via |:Tlibtrace|.
function! tlib#trace#Disable() abort "{{{3
    " :nodoc:
    command! -nargs=+ -bang Tlibtrace :
    unlet! s:trace_rx
endf

