" buffer.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2017-09-28.
" @Revision:    12.1.352


" Where to display the line when using |tlib#buffer#ViewLine|.
" For possible values for position see |scroll-cursor|.
TLet g:tlib_viewline_position = 'zz'


let s:bmru = []


function! tlib#buffer#EnableMRU() abort "{{{3
    call tlib#autocmdgroup#Init()
    autocmd TLib BufEnter * call s:BMRU_Push(bufnr('%'))
endf


function! tlib#buffer#DisableMRU() abort "{{{3
    call tlib#autocmdgroup#Init()
    autocmd! TLib BufEnter
endf


function! s:BMRU_Push(bnr) abort "{{{3
    let i = index(s:bmru, a:bnr)
    if i >= 0
        call remove(s:bmru, i)
    endif
    call insert(s:bmru, a:bnr)
endf


function! s:CompareBuffernameByBasename(a, b) abort "{{{3
    let rx = '"\zs.\{-}\ze" \+\S\+ \+\d\+$'
    let an = matchstr(a:a, rx)
    let an = fnamemodify(an, ':t')
    let bn = matchstr(a:b, rx)
    let bn = fnamemodify(bn, ':t')
    let rv = an == bn ? 0 : an > bn ? 1 : -1
    return rv
endf


function! s:CompareBufferNrByMRU(a, b) abort "{{{3
    let an = matchstr(a:a, '\s*\zs\d\+\ze')
    let bn = matchstr(a:b, '\s*\zs\d\+\ze')
    let ai = index(s:bmru, 0 + an)
    if ai == -1
        return 1
    else
        let bi = index(s:bmru, 0 + bn)
        if bi == -1
            return -1
        else
            return ai == bi ? 0 : ai > bi ? 1 : -1
        endif
    endif
endf


" Set the buffer to buffer and return a command as string that can be 
" evaluated by |:execute| in order to restore the original view.
function! tlib#buffer#Set(buffer) abort "{{{3
    let lazyredraw = &lazyredraw
    set lazyredraw
    try
        let cb = bufnr('%')
        let sn = bufnr(a:buffer)
        if sn != cb
            let ws = bufwinnr(sn)
            if ws != -1
                let wb = bufwinnr('%')
                exec ws.'wincmd w'
                return wb.'wincmd w'
            else
                silent exec 'sbuffer! '. sn
                return 'wincmd c'
            endif
        else
            return ''
        endif
    finally
        let &lazyredraw = lazyredraw
    endtry
endf


" :def: function! tlib#buffer#Eval(buffer, code) abort
" Evaluate CODE in BUFFER.
"
" EXAMPLES: >
"   call tlib#buffer#Eval('foo.txt', 'echo b:bar')
function! tlib#buffer#Eval(buffer, code) abort "{{{3
    " let cb = bufnr('%')
    " let wb = bufwinnr('%')
    " " TLogVAR cb
    " let sn = bufnr(a:buffer)
    " let sb = sn != cb
    let lazyredraw = &lazyredraw
    set lazyredraw
    let restore = tlib#buffer#Set(a:buffer)
    try
        exec a:code
        " if sb
        "     let ws = bufwinnr(sn)
        "     if ws != -1
        "         try
        "             exec ws.'wincmd w'
        "             exec a:code
        "         finally
        "             exec wb.'wincmd w'
        "         endtry
        "     else
        "         try
        "             silent exec 'sbuffer! '. sn
        "             exec a:code
        "         finally
        "             wincmd c
        "         endtry
        "     endif
        " else
        "     exec a:code
        " endif
    finally
        exec restore
        let &lazyredraw = lazyredraw
    endtry
endf


" :def: function! tlib#buffer#GetList(?show_hidden=0, ?show_number=0, " ?order='bufnr') abort
" Possible values for the "order" argument:
"   bufnr    :: Default behaviour
"   mru      :: Sort buffers according to most recent use
"   basename :: Sort by the file's basename (last component)
"
" NOTE: MRU order works on second invocation only. If you want to always 
" use MRU order, call tlib#buffer#EnableMRU() in your ~/.vimrc file.
function! tlib#buffer#GetList(...) abort
    TVarArg ['show_hidden', 0], ['show_number', 0], ['order', '']
    " TLogVAR show_hidden, show_number, order
    let ls_bang = show_hidden ? '!' : ''
    redir => bfs
    exec 'silent ls'. ls_bang
    redir END
    let buffer_list = split(bfs, '\n')
    if order ==# 'mru'
        if empty(s:bmru)
            call tlib#buffer#EnableMRU()
            echom 'tlib: Installed Buffer MRU logger; disable with: call tlib#buffer#DisableMRU()'
        else
            call sort(buffer_list, function('s:CompareBufferNrByMRU'))
        endif
    elseif order ==# 'basename'
        call sort(buffer_list, function('s:CompareBuffernameByBasename'))
    endif
    let buffer_nr = map(copy(buffer_list), 'str2nr(matchstr(v:val, ''\s*\zs\d\+\ze''))')
    " TLogVAR buffer_list, buffer_nr
    if show_number
        call map(buffer_list, 'matchstr(v:val, ''^\s*\d\+.\{-}\ze\s\+\S\+ \d\+\s*$'')')
    else
        call map(buffer_list, 'matchstr(v:val, ''^\s*\d\+\zs.\{-}\ze\s\+\S\+ \d\+\s*$'')')
    endif
    " TLogVAR buffer_list
    " call map(buffer_list, 'matchstr(v:val, ''^.\{-}\ze\s\+line \d\+\s*$'')')
    " TLogVAR buffer_list
    call map(buffer_list, 'matchstr(v:val, ''^[^"]\+''). printf("%-20s   %s", fnamemodify(matchstr(v:val, ''"\zs.\{-}\ze"$''), ":t"), fnamemodify(matchstr(v:val, ''"\zs.\{-}\ze"$''), ":h"))')
    " TLogVAR buffer_list
    return [buffer_nr, buffer_list]
endf


" :def: function! tlib#buffer#ViewLine(line, ?position='z') abort
" line is either a number or a string that begins with a number.
" For possible values for position see |scroll-cursor|.
" See also |g:tlib_viewline_position|.
function! tlib#buffer#ViewLine(line, ...) abort "{{{3
    if a:line
        TVarArg 'pos'
        let ln = matchstr(a:line, '^\d\+')
        let lt = matchstr(a:line, '^\d\+: \zs.*')
        " TLogVAR pos, ln, lt
        exec ln
        if empty(pos)
            let pos = tlib#var#Get('tlib_viewline_position', 'wbg')
        endif
        " TLogVAR pos
        if !empty(pos)
            exec 'norm! '. pos
        endif
        call tlib#buffer#HighlightLine(ln)
        " let @/ = '\%'. ln .'l.*'
    endif
endf


function! s:UndoHighlightLine() abort "{{{3
    2match none
    autocmd! TLib CursorMoved,CursorMovedI <buffer>
    autocmd! TLib CursorHold,CursorHoldI <buffer>
    autocmd! TLib InsertEnter,InsertChange,InsertLeave <buffer>
    autocmd! TLib BufLeave,BufWinLeave,WinLeave,BufHidden <buffer>
endf


function! tlib#buffer#HighlightLine(...) abort "{{{3
    TVarArg ['line', line('.')]
    " exec '2match MatchParen /^\%'. a:line .'l.*/'
    exec '2match Search /^\%'. line .'l.*/'
    call tlib#autocmdgroup#Init()
    exec 'autocmd TLib CursorMoved,CursorMovedI <buffer> if line(".") != '. line .' | call s:UndoHighlightLine() | endif'
    autocmd TLib CursorHold,CursorHoldI <buffer> call s:UndoHighlightLine()
    autocmd TLib InsertEnter <buffer> call s:UndoHighlightLine()
    " autocmd TLib BufLeave,BufWinLeave,WinLeave,BufHidden <buffer> call s:UndoHighlightLine()
endf


" Delete the lines in the current buffer. Wrapper for |:delete|.
function! tlib#buffer#DeleteRange(line1, line2) abort "{{{3
    let r = @t
    try
        exec a:line1.','.a:line2.'delete t'
    finally
        let @t = r
    endtry
endf


" Replace a range of lines.
function! tlib#buffer#ReplaceRange(line1, line2, lines) abort
    call tlib#buffer#DeleteRange(a:line1, a:line2)
    call append(a:line1 - 1, a:lines)
endf


" Initialize some scratch area at the bottom of the current buffer.
function! tlib#buffer#ScratchStart() abort "{{{3
    norm! Go
    let b:tlib_inbuffer_scratch = line('$')
    return b:tlib_inbuffer_scratch
endf


" Remove the in-buffer scratch area.
function! tlib#buffer#ScratchEnd() abort "{{{3
    if !exists('b:tlib_inbuffer_scratch')
        echoerr 'tlib: In-buffer scratch not initalized'
    endif
    call tlib#buffer#DeleteRange(b:tlib_inbuffer_scratch, line('$'))
    unlet b:tlib_inbuffer_scratch
endf


" Run exec on all buffers via bufdo and return to the original buffer.
function! tlib#buffer#BufDo(exec) abort "{{{3
    let bn = bufnr('%')
    exec 'bufdo '. a:exec
    exec 'buffer! '. bn
endf


" :def: function! tlib#buffer#InsertText(text, keyargs) abort
" Keyargs:
"   'shift': 0|N
"   'col': col('.')|N
"   'lineno': line('.')|N
"   'indent': 0|1
"   'pos': 'e'|'s' ... Where to locate the cursor (somewhat like s and e in {offset})
" Insert text (a string) in the buffer.
function! tlib#buffer#InsertText(text, ...) abort "{{{3
    TVarArg ['keyargs', {}]
    " TLogVAR a:text, keyargs
    let keyargs = extend({
                \ 'shift': 0, 'col': col('.'), 'lineno': line('.'), 'pos': 'e', 'indent': 0
                \ }, keyargs)
    " TLogVAR keyargs
    let grow = 0
    let post_del_last_line = line('$') == 1
    let line = getline(keyargs.lineno)
    if keyargs.col + keyargs.shift > 0
        let pre  = line[0 : (keyargs.col - 1 + keyargs.shift)]
        let post = line[(keyargs.col + keyargs.shift): -1]
    else
        let pre  = ''
        let post = line
    endif
    " TLogVAR keyargs.lineno, line, pre, post
    let text0 = pre . a:text . post
    let text  = split(text0, '\n', 1)
    " TLogVAR text
    let icol = len(pre)
    " exec 'norm! '. keyargs.lineno .'G'
    call cursor(keyargs.lineno, keyargs.col)
    if keyargs.indent && keyargs.col > 1
		if &formatoptions =~# '[or]'
            " FIXME: Is the simple version sufficient?
            " VERSION 1
			" " This doesn't work because it's not guaranteed that the 
			" " cursor is set.
			" let cline = getline('.')
			" norm! a
			" "norm! o
			" " TAssertExec redraw | sleep 3
			" let idt = tlib#string#Strcharpart(getline('.'), 0, keyargs.col('.') + keyargs.shift)
			" " TLogVAR idt
			" let idtl = len(idt)
			" -1,.delete
			" " TAssertExec redraw | sleep 3
			" call append(keyargs.lineno - 1, cline)
			" call cursor(keyargs.lineno, keyargs.col)
			" " TAssertExec redraw | sleep 3
			" if idtl == 0 && icol != 0
			" 	let idt = matchstr(pre, '^\s\+')
			" 	let idtl = len(idt)
			" endif
            " VERSION 2
            let idt = matchstr(pre, '^\s\+')
            let idtl = len(idt)
		else
			let [m_0, idt, iline; rest] = matchlist(pre, '^\(\s*\)\(.*\)$')
			let idtl = len(idt)
		endif
		if idtl < icol
			let idt .= repeat(' ', icol - idtl)
		endif
        " TLogVAR idt
        let idtl1 = len(idt)
        for i in range(1, len(text) - 1)
            let text[i] = idt . text[i]
            let grow += idtl1
        endfor
    endif
    " TLogVAR text
    " exec 'norm! '. keyargs.lineno .'Gdd'
    call tlib#normal#WithRegister('"tdd', 't')
    call append(keyargs.lineno - 1, text)
    if post_del_last_line
        call tlib#buffer#KeepCursorPosition('$delete')
    endif
    let tlen = len(text)
    let posshift = matchstr(keyargs.pos, '\d\+')
    " TLogVAR keyargs.pos
    if keyargs.pos =~# '^e'
        exec keyargs.lineno + tlen - 1
        exec 'norm! 0'. (len(text[-1]) - len(post) + posshift - 1) .'l'
    elseif keyargs.pos =~# '^s'
        " TLogVAR keyargs.lineno, pre, posshift
        exec keyargs.lineno
        exec 'norm! '. len(pre) .'|'
        if !empty(posshift)
            exec 'norm! '. posshift .'h'
        endif
    endif
    " TLogDBG getline(keyargs.lineno)
    " TLogDBG string(getline(1, '$'))
    return grow
endf


function! tlib#buffer#InsertText0(text, ...) abort "{{{3
    TVarArg ['keyargs', {}]
    let mode = get(keyargs, 'mode', 'i')
    " TLogVAR mode
    if !has_key(keyargs, 'shift')
        let col = col('.')
        " if mode =~ 'i'
        "     let col += 1
        " endif
        let keyargs.shift = col >= col('$') ? 0 : -1
        " let keyargs.shift = col('.') >= col('$') ? 0 : -1
        " TLogVAR col
        " TLogDBG col('.') .'-'. col('$') .': '. string(getline('.'))
    endif
    " TLogVAR keyargs.shift
    return tlib#buffer#InsertText(a:text, keyargs)
endf


function! tlib#buffer#CurrentByte() abort "{{{3
    return line2byte(line('.')) + col('.')
endf


" Evaluate cmd while maintaining the cursor position and jump registers.
function! tlib#buffer#KeepCursorPosition(cmd) abort "{{{3
    " let pos = getpos('.')
    let view = winsaveview()
    try
        keepjumps exec a:cmd
    finally
        " call setpos('.', pos)
        call winrestview(view)
    endtry
endf

