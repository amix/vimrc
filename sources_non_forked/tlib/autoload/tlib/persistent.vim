" persistent.vim -- Persistent data
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2012-05-11.
" @Last Change: 2012-05-11.
" @Revision:    7

" The directory for persistent data files. If empty, use 
" |tlib#dir#MyRuntime|.'/share'.
TLet g:tlib_persistent = ''


" :display: tlib#persistent#Dir(?mode = 'bg')
" Return the full directory name for persistent data files.
function! tlib#persistent#Dir() "{{{3
    TVarArg ['mode', 'bg']
    let dir = tlib#var#Get('tlib_persistent', mode)
    if empty(dir)
        let dir = tlib#file#Join([tlib#dir#MyRuntime(), 'share'])
    endif
    return dir
endf

" :def: function! tlib#persistent#Filename(type, ?file=%, ?mkdir=0)
function! tlib#persistent#Filename(type, ...) "{{{3
    " TLogDBG 'bufname='. bufname('.')
    let file = a:0 >= 1 ? a:1 : ''
    let mkdir = a:0 >= 2 ? a:2 : 0
    return tlib#cache#Filename(a:type, file, mkdir, tlib#persistent#Dir())
endf

function! tlib#persistent#Get(cfile) "{{{3
    if !empty(a:cfile) && filereadable(a:cfile)
        let val = readfile(a:cfile, 'b')
        return eval(join(val, "\n"))
    else
        return {}
    endif
endf

function! tlib#persistent#Value(...) "{{{3
    return call('tlib#cache#Value', a:000)
endf

function! tlib#persistent#Save(cfile, dictionary) "{{{3
    if !empty(a:cfile)
        " TLogVAR a:dictionary
        call writefile([string(a:dictionary)], a:cfile, 'b')
    endif
endf

