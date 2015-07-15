"  oracle.vim -- Vim integration for the Go oracle.
"
"  Part of this plugin was taken directly from the oracle repo, however it's
"  massively changed for a better integration into vim-go. Thanks Alan Donovan
"  for the first iteration based on quickfix!  - Fatih Arslan
"

if !exists("g:go_oracle_bin")
    let g:go_oracle_bin = "oracle"
endif

" Parses (via regex) Oracle's 'plain' format output and puts them into a
" quickfix list.
func! s:qflist(output)
    let qflist = []
    " Parse GNU-style 'file:line.col-line.col: message' format.
    let mx = '^\(\a:[\\/][^:]\+\|[^:]\+\):\(\d\+\):\(\d\+\):\(.*\)$'
    for line in split(a:output, "\n")
        let ml = matchlist(line, mx)

        " Ignore non-match lines or warnings
        if ml == [] || ml[4] =~ '^ warning:'
            continue
        endif

        let item = {
                    \  'filename': ml[1],
                    \  'text': ml[4],
                    \  'lnum': ml[2],
                    \  'col': ml[3],
                    \}
        let bnr = bufnr(fnameescape(ml[1]))
        if bnr != -1
            let item['bufnr'] = bnr
        endif
        call add(qflist, item)
    endfor
    call setqflist(qflist)
    cwindow
endfun

" This uses Vim's errorformat to parse the output from Oracle's 'plain output
" and put it into quickfix list. I believe using errorformat is much more
" easier to use. If we need more power we can always switch back to parse it
" via regex.
func! s:qflistSecond(output)
    " backup users errorformat, will be restored once we are finished
    let old_errorformat = &errorformat

    " match two possible styles of errorformats:
    "
    "   'file:line.col-line2.col2: message'
    "   'file:line:col: message'
    "
    " We discard line2 and col2 for the first errorformat, because it's not
    " useful and quickfix only has the ability to show one line and column
    " number
    let &errorformat = "%f:%l.%c-%[%^:]%#:\ %m,%f:%l:%c:\ %m"

    " create the quickfix list and open it
    cgetexpr split(a:output, "\n")
    cwindow

    let &errorformat = old_errorformat
endfun

func! s:getpos(l, c)
    if &encoding != 'utf-8'
        let buf = a:l == 1 ? '' : (join(getline(1, a:l-1), "\n") . "\n")
        let buf .= a:c == 1 ? '' : getline('.')[:a:c-2]
        return len(iconv(buf, &encoding, 'utf-8'))
    endif
    return line2byte(a:l) + (a:c-2)
endfun

func! s:RunOracle(mode, selected) range abort
    let fname = expand('%:p')
    let dname = expand('%:p:h')
    let pkg = go#package#ImportPath(dname)

    if exists('g:go_oracle_scope')
        " let the user defines the scope, must be a space separated string,
        " example: 'fmt math net/http'
        let unescaped_scopes = split(get(g:, 'go_oracle_scope'))
        let scopes = []
        for unescaped_scope in unescaped_scopes
            call add(scopes, shellescape(unescaped_scope))
        endfor
    elseif exists('g:go_oracle_include_tests') && pkg != -1
        " give import path so it includes all _test.go files too
        let scopes = [shellescape(pkg)]
    else
        " best usable way, only pass the package itself, without the test
        " files
        let scopes = go#tool#Files()
    endif

    "return with a warning if the bin doesn't exist
    let bin_path = go#path#CheckBinPath(g:go_oracle_bin) 
    if empty(bin_path) 
        return 
    endif

    if a:selected != -1
        let pos1 = s:getpos(line("'<"), col("'<"))
        let pos2 = s:getpos(line("'>"), col("'>"))
        let cmd = printf('%s -format plain -pos=%s:#%d,#%d %s',
                    \  bin_path,
                    \  shellescape(fname), pos1, pos2, a:mode)
    else
        let pos = s:getpos(line('.'), col('.'))
        let cmd = printf('%s -format plain -pos=%s:#%d %s',
                    \  bin_path,
                    \  shellescape(fname), pos, a:mode)
    endif

    " now append each scope to the end as Oracle's scope parameter. It can be
    " a packages or go files, dependent on the User's own choice. For more
    " info check Oracle's User Manual section about scopes:
    " https://docs.google.com/document/d/1SLk36YRjjMgKqe490mSRzOPYEDe0Y_WQNRv-EiFYUyw/view#heading=h.nwso96pj07q8
    for scope in scopes
        let cmd .= ' ' . scope
    endfor

    echon "vim-go: " | echohl Identifier | echon "analysing ..." | echohl None

    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()

    let out = system(cmd)

    let $GOPATH = old_gopath

    if v:shell_error
        " unfortunaly oracle outputs a very long stack trace that is not
        " parsable to show the real error. But the main issue is usually the
        " package which doesn't build. 
        redraw | echon "vim-go: " | echohl Statement | echon out | echohl None
        return ""
    endif

    return out
endfunc

function! go#oracle#Scope(...)
    if len(a:000)
        if len(a:000) == 1 && a:1 == '""'
            let g:go_oracle_scope = ""
            echon "vim-go: " | echohl Function | echon "oracle scope is cleared"| echohl None
        else
            let g:go_oracle_scope = join(a:000, ' ')
            echon "vim-go: " | echohl Function | echon "oracle scope changed to: '". g:go_oracle_scope ."'" | echohl None
        endif

        return
    endif

    if !exists(g:go_oracle_scope)
        echon "vim-go: " | echohl Function | echon "oracle scope is not set"| echohl None
    else
        echon "vim-go: " | echohl Function | echon "current oracle scope: '". g:go_oracle_scope ."'" | echohl None
    endif
endfunction

" Show 'implements' relation for selected package
function! go#oracle#Implements(selected)
    let out = s:RunOracle('implements', a:selected)
    call s:qflistSecond(out)
endfunction

" Describe selected syntax: definition, methods, etc
function! go#oracle#Describe(selected)
    let out = s:RunOracle('describe', a:selected)
    call s:qflistSecond(out)
endfunction

" Show possible targets of selected function call
function! go#oracle#Callees(selected)
    let out = s:RunOracle('callees', a:selected)
    call s:qflistSecond(out)
endfunction

" Show possible callers of selected function
function! go#oracle#Callers(selected)
    let out = s:RunOracle('callers', a:selected)
    call s:qflistSecond(out)
endfunction

" Show path from callgraph root to selected function
function! go#oracle#Callstack(selected)
    let out = s:RunOracle('callstack', a:selected)
    call s:qflistSecond(out)
endfunction

" Show free variables of selection
function! go#oracle#Freevars(selected)
    let out = s:RunOracle('freevars', a:selected)
    call s:qflistSecond(out)
endfunction

" Show send/receive corresponding to selected channel op
function! go#oracle#ChannelPeers(selected)
    let out = s:RunOracle('peers', a:selected)
    call s:qflistSecond(out)
endfunction

" Show all refs to entity denoted by selected identifier
function! go#oracle#Referrers(selected)
    let out = s:RunOracle('referrers', a:selected)
    call s:qflistSecond(out)
endfunction

" vim:ts=4:sw=4:et
"
