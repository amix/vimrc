" Simple indent support for SnipMate snippets files

if exists('b:did_indent')
    finish
endif
let b:did_indent = 1

setlocal nosmartindent
setlocal indentkeys=!^F,o,O,=snippet,=version,=extends
setlocal indentexpr=GetSnippetIndent()

if exists("*GetSnippetIndent")
    finish
endif

function! GetSnippetIndent()
    let line = getline(v:lnum)
    let prev_lnum = v:lnum - 1
    let prev_line = prev_lnum != 0 ? getline(prev_lnum) : ""

    if line =~# '\v^(snippet|extends|version) '
        return 0
    elseif indent(v:lnum) > 0
        return indent(v:lnum)
    elseif prev_line =~# '^snippet '
        return &sw
    elseif indent(prev_lnum) > 0
        return indent(prev_lnum)
    endif

    return 0
endfunction
