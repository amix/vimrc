if exists('g:loaded_syntastic_swift_swiftlint_checker')
  finish
endif
let g:loaded_syntastic_swift_swiftlint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_swift_swiftlint_IsAvailable() dict
  if !executable(self.getExec())
    return 0
  endif

  return get(g:, 'syntastic_swift_swiftlint_use_defaults', 0)
        \ || filereadable('.swiftlint.yml')
endfunction

function! SyntaxCheckers_swift_swiftlint_GetLocList() dict
  let makeprg = self.makeprgBuild({
        \ 'args': 'lint --use-script-input-files',
        \ 'fname': '' })

  let errorformat =
        \ '%f:%l:%c: %trror: %m,' .
        \ '%f:%l:%c: %tarning: %m,' .
        \ '%f:%l: %trror: %m,' .
        \ '%f:%l: %tarning: %m'

  let env = {
        \ 'SCRIPT_INPUT_FILE_COUNT': 1,
        \ 'SCRIPT_INPUT_FILE_0': expand('%:p'),
        \ }

  return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'swift',
    \ 'name': 'swiftlint' })

let &cpo = s:save_cpo
unlet s:save_cpo
