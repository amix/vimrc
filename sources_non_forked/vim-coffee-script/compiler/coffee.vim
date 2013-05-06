" Language:    CoffeeScript
" Maintainer:  Mick Koch <kchmck@gmail.com>
" URL:         http://github.com/kchmck/vim-coffee-script
" License:     WTFPL

" All this is needed to support compiling filenames with spaces, quotes, and
" such. The filename is escaped and embedded into the `makeprg` setting.
"
" Because of this, `makeprg` must be updated on every file rename. And because
" of that, `CompilerSet` can't be used because it doesn't exist when the
" rename autocmd is ran. So, we have to do some checks to see whether `compiler`
" was called locally or globally, and respect that in the rest of the script.

if exists('current_compiler')
  finish
else
  let current_compiler = 'coffee'
endif

" Pattern to check if coffee is the compiler
let s:pat = '^' . current_compiler

" Path to CoffeeScript compiler
if !exists('coffee_compiler')
  let coffee_compiler = 'coffee'
endif

" Extra options passed to CoffeeMake
if !exists('coffee_make_options')
  let coffee_make_options = ''
endif

" Get a `makeprg` for the current filename.
function! s:GetMakePrg()
  return g:coffee_compiler . ' -c ' . g:coffee_make_options . ' $* '
  \                        . fnameescape(expand('%'))
endfunction

" Set `makeprg` and return 1 if coffee is still the compiler, else return 0.
function! s:SetMakePrg()
  if &l:makeprg =~ s:pat
    let &l:makeprg = s:GetMakePrg()
  elseif &g:makeprg =~ s:pat
    let &g:makeprg = s:GetMakePrg()
  else
    return 0
  endif

  return 1
endfunction

" Set a dummy compiler so we can check whether to set locally or globally.
CompilerSet makeprg=coffee
call s:SetMakePrg()

CompilerSet errorformat=Error:\ In\ %f\\,\ %m\ on\ line\ %l,
                       \Error:\ In\ %f\\,\ Parse\ error\ on\ line\ %l:\ %m,
                       \SyntaxError:\ In\ %f\\,\ %m,
                       \%-G%.%#

" Compile the current file.
command! -bang -bar -nargs=* CoffeeMake make<bang> <args>

" Set `makeprg` on rename since we embed the filename in the setting.
augroup CoffeeUpdateMakePrg
  autocmd!

  " Update `makeprg` if coffee is still the compiler, else stop running this
  " function.
  function! s:UpdateMakePrg()
    if !s:SetMakePrg()
      autocmd! CoffeeUpdateMakePrg
    endif
  endfunction

  " Set autocmd locally if compiler was set locally.
  if &l:makeprg =~ s:pat
    autocmd BufFilePost,BufWritePost <buffer> call s:UpdateMakePrg()
  else
    autocmd BufFilePost,BufWritePost          call s:UpdateMakePrg()
  endif
augroup END
