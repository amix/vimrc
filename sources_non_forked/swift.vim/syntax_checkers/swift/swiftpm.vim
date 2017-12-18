if exists('g:loaded_syntastic_swift_swiftpm_checker')
  finish
endif
let g:loaded_syntastic_swift_swiftpm_checker = 1

if !exists('g:syntastic_swift_swiftpm_executable')
  let g:syntastic_swift_swiftpm_executable = 'swift'
endif

if !exists('g:syntastic_swift_swiftpm_arguments')
  let g:syntastic_swift_swiftpm_arguments = 'build'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_swift_swiftpm_IsAvailable() dict
  if !executable(self.getExec())
    return 0
  endif

  return filereadable('Package.swift')
endfunction

function! SyntaxCheckers_swift_swiftpm_GetLocList() dict
  let makeprg = self.makeprgBuild({
        \ 'fname': '',
        \ 'args': g:syntastic_swift_swiftpm_arguments })

  let errorformat =
        \ '%f:%l:%c: error: %m'

  return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'swift',
    \ 'name': 'swiftpm',
    \ 'exec': g:syntastic_swift_swiftpm_executable })

let &cpo = s:save_cpo
unlet s:save_cpo
