if !exists('g:rust_conceal') || g:rust_conceal == 0 || !has('conceal') || &enc != 'utf-8'
	finish
endif

" For those who don't want to see `::`...
if exists('g:rust_conceal_mod_path') && g:rust_conceal_mod_path != 0
	syn match rustNiceOperator "::" conceal cchar=ㆍ
endif

syn match rustRightArrowHead contained ">" conceal cchar= 
syn match rustRightArrowTail contained "-" conceal cchar=⟶
syn match rustNiceOperator "->" contains=rustRightArrowHead,rustRightArrowTail

syn match rustFatRightArrowHead contained ">" conceal cchar= 
syn match rustFatRightArrowTail contained "=" conceal cchar=⟹
syn match rustNiceOperator "=>" contains=rustFatRightArrowHead,rustFatRightArrowTail

syn match rustNiceOperator /\<\@!_\(_*\>\)\@=/ conceal cchar=′

" For those who don't want to see `pub`...
if exists('g:rust_conceal_pub') && g:rust_conceal_pub != 0
    syn match rustPublicSigil contained "pu" conceal cchar=＊
    syn match rustPublicRest contained "b" conceal cchar= 
    syn match rustNiceOperator "pub " contains=rustPublicSigil,rustPublicRest
endif

hi link rustNiceOperator Operator

if !(exists('g:rust_conceal_mod_path') && g:rust_conceal_mod_path != 0)
    hi! link Conceal Operator

    " And keep it after a colorscheme change
    au ColorScheme <buffer> hi! link Conceal Operator
endif
