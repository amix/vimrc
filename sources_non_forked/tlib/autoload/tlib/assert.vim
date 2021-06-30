" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     https://github.com/tomtom
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2015-11-23
" @Revision:    38


" Enable tracing via |:Tlibassert|.
function! tlib#assert#Enable() abort "{{{3
    " :nodoc:
    command! -nargs=+ -bar Tlibassert call tlib#assert#Assert(expand('<sfile>'), <q-args>, [<args>])
endf


" Disable tracing via |:Tlibassert|.
function! tlib#assert#Disable() abort "{{{3
    " :nodoc:
    command! -nargs=+ -bang -bar Tlibassert :
endf


function! tlib#assert#Assert(caller, check, vals) abort "{{{3
    for val in a:vals
        " TLogVAR val
        if type(val) == 3
            call tlib#assert#Assert(a:caller, a:check, val)
        elseif !val
            throw 'Tlibassert: '. tlib#trace#Backtrace(a:caller) .': '. a:check
        endif
    endfor
endf


function! tlib#assert#Map(vals, expr) abort "{{{3
    return tlib#assert#All(map(a:vals, a:expr))
endf


function! tlib#assert#All(vals) abort "{{{3
    " TLogVAR a:vals, empty(filter(a:vals, '!v:val'))
    return empty(filter(a:vals, '!v:val'))
endf


