" Language:    CoffeeScript
" Maintainer:  Mick Koch <mick@kochm.co>
" URL:         http://github.com/kchmck/vim-coffee-script
" License:     WTFPL

if exists('current_compiler')
  finish
endif

let current_compiler = 'cake'
call coffee#CoffeeSetUpVariables()

exec 'CompilerSet makeprg=' . escape(g:coffee_cake . ' ' .
\                                    g:coffee_cake_options . ' $*', ' ')
call coffee#CoffeeSetUpErrorFormat()
