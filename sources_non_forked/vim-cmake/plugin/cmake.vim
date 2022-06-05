" ==============================================================================
" File:        cmake.vim
" Description: Vim-CMake, a Vim/Neovim plugin for working with CMake projects
" Maintainer:  Carlo Delle Donne <https://github.com/cdelledonne>
" Version:     0.7.1
" License:     MIT
" ==============================================================================

if exists('g:loaded_cmake') && g:loaded_cmake
    finish
endif
let g:loaded_cmake = 1

" Assign user/default values to coniguration variables.
" NOTE: must be done before loading other scripts.
let s:const = cmake#const#Get()
for s:cvar in items(s:const.config_vars)
    if !has_key(g:, s:cvar[0])
        let g:[s:cvar[0]] = s:cvar[1]
    endif
endfor

let s:logger = cmake#logger#Get()

" Check required features.
if has('nvim')
    if !has('nvim-0.5.0')
        call s:logger.EchoError('Only Neovim versions >= 0.5 are supported')
        call s:logger.LogError('Only Neovim versions >= 0.5 are supported')
        finish
    endif
else
    if has('win32')
        call s:logger.EchoError('Under Windows, only Neovim is supported at the moment')
        call s:logger.LogError('Under Windows, only Neovim is supported at the moment')
        finish
    endif
    if !has('terminal')
        call s:logger.EchoError('Must run Neovim, or Vim with +terminal')
        call s:logger.LogError('Must run Neovim, or Vim with +terminal')
        finish
    endif
endif

call s:logger.LogInfo('Loading Vim-CMake')

" Check if CMake executable exists.
if !executable(g:cmake_command)
    call s:logger.EchoError('Binary ''%s'' not found in PATH', g:cmake_command)
    call s:logger.LogError('Binary ''%s'' not found in PATH', g:cmake_command)
    finish
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Commands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

command -nargs=? -bang CMakeGenerate call cmake#Generate(<bang>0, <f-args>)
command -nargs=? CMakeClean call cmake#Clean()
command -nargs=1 -complete=custom,cmake#GetConfigs CMakeSwitch call cmake#Switch(<f-args>)

command -nargs=? -bang -complete=custom,cmake#GetBuildTargets CMakeBuild call cmake#Build(<bang>0, <f-args>)
command CMakeInstall call cmake#Install()

command CMakeOpen call cmake#Open()
command CMakeClose call cmake#Close()
command CMakeStop call cmake#Stop()

call s:logger.LogInfo('Commands defined')

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <silent> <Plug>(CMakeGenerate) :call cmake#Generate(0)<CR>
nnoremap <silent> <Plug>(CMakeClean) :call cmake#Clean()<CR>
nnoremap <Plug>(CMakeSwitch) :CMakeSwitch<Space>

nnoremap <silent> <Plug>(CMakeBuild) :call cmake#Build(0)<CR>
nnoremap <silent> <Plug>(CMakeInstall) :call cmake#Install()<CR>
nnoremap <Plug>(CMakeBuildTarget) :CMakeBuild<Space>

nnoremap <silent> <Plug>(CMakeOpen) :call cmake#Open()<CR>
nnoremap <silent> <Plug>(CMakeClose) :call cmake#Close()<CR>
nnoremap <silent> <Plug>(CMakeStop) :call cmake#Stop()<CR>

call s:logger.LogInfo('Mappings defined')

call s:logger.LogInfo('Vim-CMake loaded')
