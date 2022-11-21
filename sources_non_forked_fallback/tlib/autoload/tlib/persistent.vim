" persistent.vim -- Persistent data
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2012-05-11.
" @Last Change: 2017-03-29.
" @Revision:    15

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

" :display: tlib#persistent#EncodedFilename(type, file, ?mkdir=0, ?dir='')
" Encode `file` and call |tlib#persistent#Filename()|.
function! tlib#persistent#EncodedFilename(type, file, ...) "{{{3
    let file = tlib#url#Encode(a:file)
    return call(function('tlib#persistent#Filename'), [a:type, file] + a:000)
endf

" :def: function! tlib#persistent#Filename(type, ?file=%, ?mkdir=0)
function! tlib#persistent#Filename(type, ...) "{{{3
    " TLogDBG 'bufname='. bufname('.')
    let file = a:0 >= 1 ? a:1 : ''
    let mkdir = a:0 >= 2 ? a:2 : 0
    return tlib#cache#Filename(a:type, file, mkdir, tlib#persistent#Dir())
endf

function! tlib#persistent#Get(...) "{{{3
    return call('tlib#cache#Get', a:000)
endf

function! tlib#persistent#MTime(cfile) "{{{3
    return tlib#cache#MTime(a:cfile)
endf

function! tlib#persistent#Value(...) "{{{3
    return call('tlib#cache#Value', a:000)
endf

function! tlib#persistent#Save(...) "{{{3
    call call(function('tlib#cache#Save'), a:000)
endf

