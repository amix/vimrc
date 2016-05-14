"  guru.vim -- Vim integration for the Go guru.

func! s:RunGuru(mode, format, selected, needs_scope) range abort
    "return with a warning if the binary doesn't exist
    let bin_path = go#path#CheckBinPath("guru") 
    if empty(bin_path)
        return {'err': "bin path not found"}
    endif

    let filename = expand('%:p')
    let dirname = expand('%:p:h')
    let pkg = go#package#ImportPath(dirname)

    " this is important, check it!
    if pkg == -1 && a:needs_scope
        return {'err': "current directory is not inside of a valid GOPATH"}
    endif

    " start constructing the 'command' variable
    let command = bin_path

    " enable outputting in json format
    if a:format == "json"
        let command .= " -json"
    endif

    " check for any tags
    if exists('g:go_guru_tags')
        let tags = get(g:, 'go_guru_tags')
        let command .= printf(" -tags %s", tags)
    endif

    " some modes require scope to be defined (such as callers). For these we
    " choose a sensible setting, which is using the current file's package
    let scopes = []
    if a:needs_scope
        let scopes = [pkg]
    endif

    " check for any user defined scope setting. users can define the scope,
    " in package pattern form. examples:
    "  golang.org/x/tools/cmd/guru # a single package
    "  golang.org/x/tools/...      # all packages beneath dir
    "  ...                         # the entire workspace.
    if exists('g:go_guru_scope')
        " check that the setting is of type list
        if type(get(g:, 'go_guru_scope')) != type([])
            return {'err' : "go_guru_scope should of type list"}
        endif

        let scopes = get(g:, 'go_guru_scope')
    endif

    " now add the scope to our command if there is any
    if !empty(scopes)
        " strip trailing slashes for each path in scoped. bug:
        " https://github.com/golang/go/issues/14584
        let scopes = go#util#StripTrailingSlash(scopes)

        " create shell-safe entries of the list
        let scopes = go#util#Shelllist(scopes)

        " guru expect a comma-separated list of patterns, construct it
        let scope = join(scopes, ",")
        let command .= printf(" -scope %s", scope)
    endif

    let pos = printf("#%s", go#util#OffsetCursor())
    if a:selected != -1
        " means we have a range, get it
        let pos1 = go#util#Offset(line("'<"), col("'<"))
        let pos2 = go#util#Offset(line("'>"), col("'>"))
        let pos = printf("#%s,#%s", pos1, pos2)
    endif

    " this is our final command
    let command .= printf(' %s %s:%s', a:mode, shellescape(filename), pos)

    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()

    " the query might take time, let us give some feedback
    call go#util#EchoProgress("analysing ...")

    " run, forrest run!!!
    let out = go#util#System(command)

    let $GOPATH = old_gopath
    if go#util#ShellError() != 0
        " the output contains the error message
        return {'err' : out}
    endif

    return {'out': out}
endfunc

" This uses Vim's errorformat to parse the output from Guru's 'plain output
" and put it into location list. I believe using errorformat is much more
" easier to use. If we need more power we can always switch back to parse it
" via regex.
func! s:loclistSecond(output)
    " backup users errorformat, will be restored once we are finished
    let old_errorformat = &errorformat

    " match two possible styles of errorformats:
    "
    "   'file:line.col-line2.col2: message'
    "   'file:line:col: message'
    "
    " We discard line2 and col2 for the first errorformat, because it's not
    " useful and location only has the ability to show one line and column
    " number
    let errformat = "%f:%l.%c-%[%^:]%#:\ %m,%f:%l:%c:\ %m"
    call go#list#ParseFormat("locationlist", errformat, split(a:output, "\n"))

    let errors = go#list#Get("locationlist")
    call go#list#Window("locationlist", len(errors))
endfun


function! go#guru#Scope(...)
    if a:0
        if a:0 == 1 && a:1 == '""'
            unlet g:go_guru_scope
            call go#util#EchoSuccess("guru scope is cleared")
        else
            let g:go_guru_scope = a:000
            call go#util#EchoSuccess("guru scope changed to: ". join(a:000, ","))
        endif

        return
    endif

    if !exists('g:go_guru_scope')
        call go#util#EchoError("guru scope is not set")
    else
        call go#util#EchoSuccess("current guru scope: ". join(g:go_guru_scope, ","))
    endif
endfunction

function! go#guru#Tags(...)
    if a:0
        if a:0 == 1 && a:1 == '""'
            unlet g:go_guru_tags
            call go#util#EchoSuccess("guru tags is cleared")
        else
            let g:go_guru_tags = a:1
            call go#util#EchoSuccess("guru tags changed to: ". a:1)
        endif

        return
    endif

    if !exists('g:go_guru_tags')
        call go#util#EchoSuccess("guru tags is not set")
    else
        call go#util#EchoSuccess("current guru tags: ". a:1)
    endif
endfunction

" Show 'implements' relation for selected package
function! go#guru#Implements(selected)
    let out = s:RunGuru('implements', 'plain', a:selected, 1)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    call s:loclistSecond(out.out)
endfunction

" Describe selected syntax: definition, methods, etc
function! go#guru#Describe(selected)
    let out = s:RunGuru('describe', 'plain', a:selected, 0)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    call s:loclistSecond(out.out)
endfunction

" Show possible targets of selected function call
function! go#guru#Callees(selected)
    let out = s:RunGuru('callees', 'plain', a:selected, 1)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    call s:loclistSecond(out.out)
endfunction

" Show possible callers of selected function
function! go#guru#Callers(selected)
    let out = s:RunGuru('callers', 'plain', a:selected, 1)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    call s:loclistSecond(out.out)
endfunction

" Show path from callgraph root to selected function
function! go#guru#Callstack(selected)
    let out = s:RunGuru('callstack', 'plain', a:selected, 1)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    call s:loclistSecond(out.out)
endfunction

" Show free variables of selection
function! go#guru#Freevars(selected)
    " Freevars requires a selection
    if a:selected == -1
        call go#util#EchoError("GoFreevars requires a selection (range) of code")
        return
    endif

    let out = s:RunGuru('freevars', 'plain', a:selected, 0)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    call s:loclistSecond(out.out)
endfunction

" Show send/receive corresponding to selected channel op
function! go#guru#ChannelPeers(selected)
    let out = s:RunGuru('peers', 'plain', a:selected, 1)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    call s:loclistSecond(out.out)
endfunction

" Show all refs to entity denoted by selected identifier
function! go#guru#Referrers(selected)
    let out = s:RunGuru('referrers', 'plain', a:selected, 0)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    call s:loclistSecond(out.out)
endfunction

function! go#guru#What(selected)
    " nvim doesn't have JSON support, though they work on it:
    " https://github.com/neovim/neovim/pull/4131
    if has('nvim')
        return {'err': "GoWhat is not supported in Neovim"}
    endif

    " json_encode() and friends are introduced with this patch
    " https://groups.google.com/d/msg/vim_dev/vLupTNhQhZ8/cDGIk0JEDgAJ
    if !has('patch-7.4.1304')
        return {'err': "GoWhat is supported with Vim version 7.4-1304 or later"}
    endif

    let out = s:RunGuru('what', 'json', a:selected, 0)
    if has_key(out, 'err')
        return out.err
    endif

    call s:loclistSecond(out.out)
    let result = json_decode(out.out)

    if type(result) != type({})
        return {'err': "malformed output from guru"}
    endif

    if !has_key(result, 'what')
        return {'err': "no what query found for the given identifier"}
    endif

    return {'out': result.what}
endfunction

function! go#guru#SameIds(selected)
    let result = go#guru#What(a:selected)
    if has_key(out, 'err')
        call go#util#EchoError(out.err)
        return
    endif

    if !has_key(result.out, 'sameids')
        call go#util#EchoError("no same_ids founds for the given identifier")
        return -1
    endif

    let same_ids = result.what.sameids
    echo same_ids
endfunction

" vim:ts=4:sw=4:et
