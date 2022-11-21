" Language:    CoffeeScript
" Maintainer:  Sven Felix Oberquelle <Svelix.Github@gmail.com>
" URL:         http://github.com/kchmck/vim-coffee-script
" License:     WTFPL


if exists('b:current_syntax')
  let s:current_syntax_save = b:current_syntax
endif

" Inherit coffee from html so coffeeComment isn't redefined and given higher
" priority than hamlInterpolation.
syn cluster hamlCoffeescript contains=@htmlCoffeeScript
syn region  hamlCoffeescriptFilter matchgroup=hamlFilter
\                                  start="^\z(\s*\):coffee\z(script\)\?\s*$"
\                                  end="^\%(\z1 \| *$\)\@!"
\                                  contains=@hamlCoffeeScript,hamlInterpolation
\                                  keepend

if exists('s:current_syntax_save')
  let b:current_syntax = s:current_syntax_save
  unlet s:current_syntax_save
endif
