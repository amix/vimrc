" Vim compiler file
" Language:             Scala SBT (http://www.scala-sbt.org/)
" Maintainer:           Derek Wyatt
" URL:                  https://github.com/derekwyatt/vim-scala
" License:              Apache 2
" ----------------------------------------------------------------------------

if exists('current_compiler')
  finish
endif
let current_compiler = 'sbt'

if exists(':CompilerSet') != 2          " older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=sbt\ -Dsbt.log.noformat=true\ compile

CompilerSet errorformat=
      \%E\ %#[error]\ %f:%l:\ %m,%C\ %#[error]\ %p^,%-C%.%#,%Z,
      \%W\ %#[warn]\ %f:%l:\ %m,%C\ %#[warn]\ %p^,%-C%.%#,%Z,
      \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save

" vim:set sw=2 sts=2 ts=8 et:
