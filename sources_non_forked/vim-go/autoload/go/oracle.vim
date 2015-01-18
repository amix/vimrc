" -*- text -*-
"  oracle.vim -- Vim integration for the Go oracle.
"
"  Part of this plugin was taken directly from the oracle repo, however it's
"  massively changed for a better integration into vim-go. Thanks Alan Donovan
"  for the first iteration based on quickfix!  - Fatih Arslan
"

if !exists("g:go_oracle_bin")
    let g:go_oracle_bin = "oracle"
endif

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

    if exists('g:go_oracle_scope_file')
        " let the user defines the scope
        let sname = shellescape(get(g:, 'go_oracle_scope_file'))
    elseif exists('g:go_oracle_include_tests') && pkg != -1
        " give import path so it includes all _test.go files too
        let sname = shellescape(pkg)
    else
        " best usable way, only pass the package itself, without the test
        " files
        let sname = join(go#tool#Files(), ' ')
    endif

    "return with a warning if the bin doesn't exist
    let bin_path = go#tool#BinPath(g:go_oracle_bin) 
    if empty(bin_path) 
        return 
    endif

    if a:selected != -1
        let pos1 = s:getpos(line("'<"), col("'<"))
        let pos2 = s:getpos(line("'>"), col("'>"))
        let cmd = printf('%s -format json -pos=%s:#%d,#%d %s %s',
                    \  bin_path,
                    \  shellescape(fname), pos1, pos2, a:mode, sname)
    else
        let pos = s:getpos(line('.'), col('.'))
        let cmd = printf('%s -format json -pos=%s:#%d %s %s',
                    \  bin_path,
                    \  shellescape(fname), pos, a:mode, sname)
    endif

    echon "vim-go: " | echohl Identifier | echon "analysing ..." | echohl None

    let out = system(cmd)
    if v:shell_error
        " unfortunaly oracle outputs a very long stack trace that is not
        " parsable to show the real error. But the main issue is usually the
        " package which doesn't build. 
        redraw | echon "vim-go: " | echohl Statement | echon out | echohl None
        return {}
    else
        let json_decoded = webapi#json#decode(out)
        return json_decoded
    endif
endfun


" Show 'implements' relation for selected package
function! go#oracle#Implements(selected)
    let out = s:RunOracle('implements', a:selected)
    if empty(out)
        return
    endif

    " be sure they exists before we retrieve them from the map
    if !has_key(out, "implements")
        return
    endif

    if has_key(out.implements, "from")
        let interfaces = out.implements.from
    elseif has_key(out.implements, "fromptr")
        let interfaces = out.implements.fromptr
    else
        redraw | echon "vim-go: " | echon "does not satisfy any interface"| echohl None
        return
    endif

    " get the type name from the type under the cursor
    let typeName = out.implements.type.name

    " prepare the title
    let title = typeName . " implements:"

    " start to populate our buffer content
    let result  = [title, ""]

    for interface in interfaces
        " don't add runtime interfaces
        if interface.name !~ '^runtime'
            let line = interface.name . "\t" . interface.pos
            call add(result, line)
        endif
    endfor

    " open a window and put the result
    call go#ui#OpenWindow("Implements", result)

    " define some buffer related mappings:
    "
    " go to definition when hit enter
    nnoremap <buffer> <CR> :<C-u>call go#ui#OpenDefinition("implements")<CR>
    " close the window when hit ctrl-c
    nnoremap <buffer> <c-c> :<C-u>call go#ui#CloseWindow()<CR>
endfunction

" Describe selected syntax: definition, methods, etc
function! go#oracle#Describe(selected)
    let out = s:RunOracle('describe', a:selected)
    if empty(out)
        return
    endif

    echo out
    return

    let detail = out["describe"]["detail"]
    let desc = out["describe"]["desc"]

    echo '# detail: '. detail
    " package, constant, variable, type, function or statement labe
    if detail == "package"
        echo desc
        return
    endif

    if detail == "value"
        echo desc
        echo out["describe"]["value"]
        return
    endif

    " the rest needs to be implemented
    echo desc
endfunction

" Show possible targets of selected function call
function! go#oracle#Callees(selected)
    let out = s:RunOracle('callees', a:selected)
    if empty(out)
        return
    endif

    " be sure the callees object exists which contains the position and names
    " of the callees, before we continue
    if !has_key(out, "callees")
        return
    endif

    " get the callees list
    if has_key(out.callees, "callees")
        let callees = out.callees.callees
    else
        redraw | echon "vim-go: " | echon "no callees available"| echohl None
        return
    endif

    let title = "Call targets:"

    " start to populate our buffer content
    let result  = [title, ""]

    for calls in callees
        let line = calls.name . "\t" . calls.pos
        call add(result, line)
    endfor

    " open a window and put the result
    call go#ui#OpenWindow("Callees", result)

    " define some buffer related mappings:
    "
    " go to definition when hit enter
    nnoremap <buffer> <CR> :<C-u>call go#ui#OpenDefinition("call targets")<CR>
    " close the window when hit ctrl-c
    nnoremap <buffer> <c-c> :<C-u>call go#ui#CloseWindow()<CR>
endfunction

" Show possible callers of selected function
function! go#oracle#Callers(selected)
    let out = s:RunOracle('callers', a:selected)
    echo out
endfunction

" Show the callgraph of the current program.
function! go#oracle#Callgraph(selected)
    let out = s:RunOracle('callgraph', a:selected)
    echo out
endfunction

" Show path from callgraph root to selected function
function! go#oracle#Callstack(selected)
    let out = s:RunOracle('callstack', a:selected)
    echo out
endfunction

" Show free variables of selection
function! go#oracle#Freevars(selected)
    let out = s:RunOracle('freevars', a:selected)
    echo out
endfunction

" Show send/receive corresponding to selected channel op
function! go#oracle#Peers(selected)
    let out = s:RunOracle('peers', a:selected)
    echo out
endfunction

" Show all refs to entity denoted by selected identifier
function! go#oracle#Referrers(selected)
    let out = s:RunOracle('referrers', a:selected)
    echo out
endfunction

" vim:ts=4:sw=4:et
