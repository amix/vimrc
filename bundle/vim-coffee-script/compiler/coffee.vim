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
endif

let current_compiler = 'coffee'
call coffee#CoffeeSetUpVariables()

" Pattern to check if coffee is the compiler
let s:pat = '^' . current_compiler

" Get a `makeprg` for the current filename.
function! s:GetMakePrg()
  return g:coffee_compiler .
  \      ' -c' .
  \      ' ' . b:coffee_litcoffee .
  \      ' ' . g:coffee_make_options .
  \      ' $*' .
  \      ' ' . fnameescape(expand('%'))
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
exec 'CompilerSet makeprg=' . current_compiler
" Then actually set the compiler.
call s:SetMakePrg()
call coffee#CoffeeSetUpErrorFormat()

function! s:CoffeeMakeDeprecated(bang, args)
  echoerr 'CoffeeMake is deprecated! Please use :make instead, its behavior ' .
  \       'is identical.'
  sleep 5
  exec 'make' . a:bang a:args
endfunction

" Compile the current file.
command! -bang -bar -nargs=* CoffeeMake
\        call s:CoffeeMakeDeprecated(<q-bang>, <q-args>)

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
    autocmd BufWritePre,BufFilePost <buffer> call s:UpdateMakePrg()
  else
    autocmd BufWritePre,BufFilePost          call s:UpdateMakePrg()
  endif
augroup END
