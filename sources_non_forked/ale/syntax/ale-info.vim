if exists('b:current_syntax')
    finish
endif

" Exhaustively list different ALE Info directives to match here.
" This should hopefully avoid matching too eagerly.
syn match aleInfoDirective /^ *Current Filetype:/
syn match aleInfoDirective /^ *Available Linters:/
syn match aleInfoDirective /^ *Enabled Linters:/
syn match aleInfoDirective /^ *Ignored Linters:/
syn match aleInfoDirective /^ *Suggested Fixers:/
syn match aleInfoDirective /^ *Command History:/

syn match aleCommandNoOutput /^<<<NO OUTPUT RETURNED>>>$/

hi def link aleInfoDirective Title
hi def link aleInfoDirective Title
hi def link aleCommandNoOutput Comment

" Use Vim syntax highlighting for Vim options.
unlet! b:current_syntax
syntax include @srcVim syntax/vim.vim
syntax region aleInfoVimRegionLinter matchgroup=aleInfoDirective start="^ *Linter Variables:$" end="^ $" contains=@srcVim
syntax region aleInfoVimRegionGlobal matchgroup=aleInfoDirective start="^ *Global Variables:$" end="^ $" contains=@srcVim

unlet! b:current_syntax
syntax include @srcAleFixSuggest syntax/ale-fix-suggest.vim
syntax region aleInfoFixSuggestRegion matchgroup=aleInfoDirective start="^ *Suggested Fixers:$" end="^ $" contains=@srcAleFixSuggest

let b:current_syntax = 'ale-info'
