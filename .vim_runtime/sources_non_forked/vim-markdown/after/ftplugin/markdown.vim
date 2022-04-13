" vim: ts=4 sw=4:
" folding for Markdown headers, both styles (atx- and setex-)
" http://daringfireball.net/projects/markdown/syntax#header
"
" this code can be placed in file
"   $HOME/.vim/after/ftplugin/markdown.vim
"
" original version from Steve Losh's gist: https://gist.github.com/1038710

function! s:is_mkdCode(lnum)
    let name = synIDattr(synID(a:lnum, 1, 0), 'name')
    return (name =~ '^mkd\%(Code$\|Snippet\)' || name != '' && name !~ '^\%(mkd\|html\)')
endfunction

if get(g:, "vim_markdown_folding_style_pythonic", 0)
    function! Foldexpr_markdown(lnum)
        let l1 = getline(a:lnum)
        "~~~~~ keep track of fenced code blocks ~~~~~
        "If we hit a code block fence
        if l1 =~ '````*' || l1 =~ '\~\~\~\~*'
            " toggle the variable that says if we're in a code block
            if b:fenced_block == 0
                let b:fenced_block = 1
            elseif b:fenced_block == 1
                let b:fenced_block = 0
            endif
        " else, if we're caring about front matter
        elseif g:vim_markdown_frontmatter == 1
            " if we're in front matter and not on line 1
            if b:front_matter == 1 && a:lnum > 2
                let l0 = getline(a:lnum-1)
                " if the previous line fenced front matter
                if l0 == '---'
                    " we must not be in front matter
                    let b:front_matter = 0
                endif
            " else, if we're on line one
            elseif a:lnum == 1
                " if we hit a front matter fence
                if l1 == '---'
                    " we're in the front matter
                    let b:front_matter = 1
                endif
            endif
        endif

        " if we're in a code block or front matter
        if b:fenced_block == 1 || b:front_matter == 1
            if a:lnum == 1
                " fold any 'preamble'
                return '>1'
            else
                " keep previous foldlevel
                return '='
            endif
        endif

        let l2 = getline(a:lnum+1)
        " if the next line starts with two or more '='
        " and is not code
        if l2 =~ '^==\+\s*' && !s:is_mkdCode(a:lnum+1)
            " next line is underlined (level 1)
            return '>0'
        " else, if the nex line starts with two or more '-'
        " and is not code
        elseif l2 =~ '^--\+\s*' && !s:is_mkdCode(a:lnum+1)
            " next line is underlined (level 2)
            return '>1'
        endif

        "if we're on a non-code line starting with a pound sign
        if l1 =~ '^#' && !s:is_mkdCode(a:lnum)
            " set the fold level to the number of hashes -1
            " return '>'.(matchend(l1, '^#\+') - 1)
            " set the fold level to the number of hashes
            return '>'.(matchend(l1, '^#\+'))
        " else, if we're on line 1
        elseif a:lnum == 1
            " fold any 'preamble'
            return '>1'
        else
            " keep previous foldlevel
            return '='
        endif
    endfunction

    function! Foldtext_markdown()
        let line = getline(v:foldstart)
        let has_numbers = &number || &relativenumber
        let nucolwidth = &fdc + has_numbers * &numberwidth
        let windowwidth = winwidth(0) - nucolwidth - 6
        let foldedlinecount = v:foldend - v:foldstart
        let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
        let line = substitute(line, '\%("""\|''''''\)', '', '')
        let fillcharcount = windowwidth - len(line) - len(foldedlinecount) + 1
        return line . ' ' . repeat("-", fillcharcount) . ' ' . foldedlinecount
    endfunction
else " vim_markdown_folding_style_pythonic == 0
    function! Foldexpr_markdown(lnum)
        if (a:lnum == 1)
            let l0 = ''
        else
            let l0 = getline(a:lnum-1)
        endif

        " keep track of fenced code blocks
        if l0 =~ '````*' || l0 =~ '\~\~\~\~*'
            if b:fenced_block == 0
                let b:fenced_block = 1
            elseif b:fenced_block == 1
                let b:fenced_block = 0
            endif
        elseif g:vim_markdown_frontmatter == 1
            if b:front_matter == 1
                if l0 == '---'
                    let b:front_matter = 0
                endif
            elseif a:lnum == 2
                if l0 == '---'
                    let b:front_matter = 1
                endif
            endif
        endif

        if b:fenced_block == 1 || b:front_matter == 1
            " keep previous foldlevel
            return '='
        endif

        let l2 = getline(a:lnum+1)
        if  l2 =~ '^==\+\s*' && !s:is_mkdCode(a:lnum+1)
            " next line is underlined (level 1)
            return '>1'
        elseif l2 =~ '^--\+\s*' && !s:is_mkdCode(a:lnum+1)
            " next line is underlined (level 2)
            if s:vim_markdown_folding_level >= 2
                return '>1'
            else
                return '>2'
            endif
        endif

        let l1 = getline(a:lnum)
        if l1 =~ '^#' && !s:is_mkdCode(a:lnum)
            " fold level according to option
            if s:vim_markdown_folding_level == 1 || matchend(l1, '^#\+') > s:vim_markdown_folding_level
                if a:lnum == line('$')
                    return matchend(l1, '^#\+') - 1
                else
                    return -1
                endif
            else
                " headers are not folded
                return 0
            endif
        endif

        if l0 =~ '^#' && !s:is_mkdCode(a:lnum-1)
            " previous line starts with hashes
            return '>'.matchend(l0, '^#\+')
        else
            " keep previous foldlevel
            return '='
        endif
    endfunction
endif


let b:fenced_block = 0
let b:front_matter = 0
let s:vim_markdown_folding_level = get(g:, "vim_markdown_folding_level", 1)

function! s:MarkdownSetupFolding()
    if !get(g:, "vim_markdown_folding_disabled", 0)
        if get(g:, "vim_markdown_folding_style_pythonic", 0)
            if get(g:, "vim_markdown_override_foldtext", 1)
                setlocal foldtext=Foldtext_markdown()
            endif
        endif
        setlocal foldexpr=Foldexpr_markdown(v:lnum)
        setlocal foldmethod=expr
    endif
endfunction

function! s:MarkdownSetupFoldLevel()
    if get(g:, "vim_markdown_folding_style_pythonic", 0)
        " set default foldlevel
        execute "setlocal foldlevel=".s:vim_markdown_folding_level
    endif
endfunction

call s:MarkdownSetupFoldLevel()
call s:MarkdownSetupFolding()

augroup Mkd
    " These autocmds need to be kept in sync with the autocmds calling
    " s:MarkdownRefreshSyntax in ftplugin/markdown.vim.
    autocmd BufWinEnter,BufWritePost <buffer> call s:MarkdownSetupFolding()
    autocmd InsertEnter,InsertLeave <buffer> call s:MarkdownSetupFolding()
    autocmd CursorHold,CursorHoldI <buffer> call s:MarkdownSetupFolding()
augroup END
