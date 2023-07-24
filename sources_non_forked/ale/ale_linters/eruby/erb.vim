" Author: Matthias Guenther - https://wikimatze.de, Eddie Lebow https://github.com/elebow
" Description: ERB from the Ruby standard library, for eruby/erb files

function! ale_linters#eruby#erb#GetCommand(buffer) abort
    let l:rails_root = ale#ruby#FindRailsRoot(a:buffer)

    if empty(l:rails_root)
        return 'erb -P -T - -x %t | ruby -c'
    endif

    " Rails-flavored eRuby does not comply with the standard as understood by
    " ERB, so we'll have to do some substitution. This does not reduce the
    " effectiveness of the linter—the translated code is still evaluated.
    return 'ruby -r erb -e ' . ale#Escape('puts ERB.new($stdin.read.gsub(%{<%=},%{<%}), trim_mode: %{-}).src') . '< %t | ruby -c'
endfunction

call ale#linter#Define('eruby', {
\   'name': 'erb',
\   'aliases': ['erubylint'],
\   'executable': 'erb',
\   'output_stream': 'stderr',
\   'command': function('ale_linters#eruby#erb#GetCommand'),
\   'callback': 'ale#handlers#ruby#HandleSyntaxErrors',
\})

