if !jedi#init_python()
    finish
endif

if g:jedi#show_call_signatures > 0 && has('conceal')
    " +conceal is the default for vim >= 7.3

    let s:e = g:jedi#call_signature_escape
    let s:full = s:e.'jedi=.\{-}'.s:e.'.\{-}'.s:e.'jedi'.s:e
    let s:ignore = s:e.'jedi.\{-}'.s:e
    exe 'syn match jediIgnore "'.s:ignore.'" contained conceal'
    setlocal conceallevel=2
    syn match jediFatSymbol "\*_\*" contained conceal
    syn match jediFat "\*_\*.\{-}\*_\*" contained contains=jediFatSymbol
    syn match jediSpace "\v[ ]+( )@=" contained
    exe 'syn match jediFunction "'.s:full.'" keepend extend '
                \ .' contains=jediIgnore,jediFat,jediSpace'
                \ .' containedin=pythonComment,pythonString,pythonRawString'
    unlet! s:e s:full s:ignore

    hi def link jediIgnore Ignore
    hi def link jediFatSymbol Ignore
    hi def link jediSpace Normal

    if exists('g:colors_name')
        hi def link jediFunction CursorLine
        hi def link jediFat TabLine
    else
        hi jediFunction term=NONE cterm=NONE ctermfg=6 guifg=Black gui=NONE ctermbg=0 guibg=Grey
        hi jediFat term=bold,underline cterm=bold,underline gui=bold,underline ctermbg=0 guibg=#555555
    endif
endif
