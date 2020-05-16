" Author: Eddie Lebow https://github.com/elebow
" Description: eruby checker using `erubi`

function! ale_linters#eruby#erubi#GetCommand(buffer, output, meta) abort
    let l:rails_root = ale#ruby#FindRailsRoot(a:buffer)

    if !empty(a:output)
        " The empty command in CheckErubi returns nothing if erubi runs and
        " emits an error if erubi is not present
        return ''
    endif

    if empty(l:rails_root)
        return 'ruby -r erubi/capture_end -e ' . ale#Escape('puts Erubi::CaptureEndEngine.new($stdin.read).src') . '< %t | ruby -c'
    endif

    " Rails-flavored eRuby does not comply with the standard as understood by
    " Erubi, so we'll have to do some substitution. This does not reduce the
    " effectiveness of the linter---the translated code is still evaluated.
    return 'ruby -r erubi/capture_end -e ' . ale#Escape('puts Erubi::CaptureEndEngine.new($stdin.read.gsub(%{<%=},%{<%}), nil, %{-}).src') . '< %t | ruby -c'
endfunction

call ale#linter#Define('eruby', {
\   'name': 'erubi',
\   'executable': 'ruby',
\   'command': {buffer -> ale#command#Run(
\       buffer,
\       'ruby -r erubi/capture_end -e ' . ale#Escape('""'),
\       function('ale_linters#eruby#erubi#GetCommand'),
\   )},
\   'callback': 'ale#handlers#ruby#HandleSyntaxErrors',
\})
