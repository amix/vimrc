" Language:    CoffeeScript
" Maintainer:  Sven Felix Oberquelle <Svelix.Github@gmail.com>
" URL:         http://github.com/kchmck/vim-coffee-script
" License:     WTFPL

" Inherit coffee from html so coffeeComment isn't redefined and given higher
" priority than hamlInterpolation.
syn cluster hamlCoffeescript contains=@htmlCoffeeScript
syn region  hamlCoffeescriptFilter matchgroup=hamlFilter
\                                  start="^\z(\s*\):coffee\z(script\)\?\s*$"
\                                  end="^\%(\z1 \| *$\)\@!"
\                                  contains=@hamlCoffeeScript,hamlInterpolation
\                                  keepend
