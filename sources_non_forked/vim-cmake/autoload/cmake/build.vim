" ==============================================================================
" Location:    autoload/cmake/build.vim
" Description: Functions for building a project
" ==============================================================================

let s:build = {}

let s:buildsys = cmake#buildsys#Get()
let s:const = cmake#const#Get()
let s:logger = cmake#logger#Get()
let s:quickfix = cmake#quickfix#Get()
let s:system = cmake#system#Get()
let s:terminal = cmake#terminal#Get()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Private functions and callbacks
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Get dictionary of build arguments from command-line string.
"
" Params:
"     argstring : String
"         command-line arguments, like target and additional build options
"
" Returns:
"     Dictionary
"         CMake build options, target and native options
"
" Example:
"     argstring = --jobs 4 all -- VERBOSE=1
"     return = {
"             \ 'cmake_build_options': ['--jobs', '4'],
"             \ 'target': ['--target', 'all'],
"             \ 'native_build_options': ['VERBOSE=1']
"             \ }
"
function! s:GetBuildArgs(argstring) abort
    let l:argdict = {}
    let l:arglist = split(a:argstring)
    " Search arguments for one that matches the name of a target.
    for l:t in s:buildsys.GetTargets()
        let l:match_res = match(l:arglist, '\m\C^' . l:t)
        if l:match_res != -1
            " If found, get target and remove from list of arguments.
            let l:target =  l:arglist[l:match_res]
            let l:argdict['target'] = ['--target', l:target]
            call remove(l:arglist, l:match_res)
            break
        endif
    endfor
    " Search for command-line native build tool arguments.
    let l:match_res = match(l:arglist, '\m\C^--$')
    if l:match_res != -1
        " Get command-line native build tool arguments and remove from list.
        let l:argdict['native_build_options'] = l:arglist[l:match_res+1:]
        " Remove from list of other arguments.
        call remove(l:arglist, l:match_res, -1)
    endif
    " Get command-line CMake arguments.
    let l:argdict['cmake_build_options'] = l:arglist
    return l:argdict
endfunction

" Generate quickfix list after running build command.
"
function! s:GenerateQuickfix() abort
    call s:quickfix.Generate(s:terminal.GetOutput())
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Public functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Build a project using the generated buildsystem.
"
" Params:
"     clean : Boolean
"         whether to clean before building
"     argstring : String
"         build target and other options
"
function! s:build.Build(clean, argstring) abort
    call s:logger.LogDebug('Invoked: build.Build(%s, %s)',
            \ a:clean, string(a:argstring))
    let l:path_to_current_config = s:buildsys.GetPathToCurrentConfig()
    let l:build_dir = s:system.Path([l:path_to_current_config], v:true)
    let l:command = [g:cmake_command, '--build', l:build_dir]
    let l:options = {}
    " Parse additional options.
    let l:options = s:GetBuildArgs(a:argstring)
    " Add CMake build options to the command.
    let l:command += g:cmake_build_options
    let l:command += get(l:options, 'cmake_build_options', [])
    if a:clean
        let l:command += ['--clean-first']
    endif
    " Add target to the command, if any was provided.
    let l:command += get(l:options, 'target', [])
    " Add native build tool options to the command.
    if len(g:cmake_native_build_options) > 0 ||
            \ len(get(l:options, 'native_build_options', [])) > 0
        let l:command += ['--']
        let l:command += g:cmake_native_build_options
        let l:command += get(l:options, 'native_build_options', [])
    endif
    " Run build command.
    call s:terminal.Run(l:command, 'build',
            \ [function('s:GenerateQuickfix')],
            \ [function('s:GenerateQuickfix')],
            \ ['CMakeBuildSucceeded'], ['CMakeBuildFailed']
            \ )
endfunction

" Install a project.
"
function! s:build.Install() abort
    call s:logger.LogDebug('Invoked: build.Install()')
    let l:path_to_current_config = s:buildsys.GetPathToCurrentConfig()
    let l:build_dir = s:system.Path([l:path_to_current_config], v:true)
    let l:command = [g:cmake_command, '--install', l:build_dir]
    call s:terminal.Run(l:command, 'install', [], [], [], [])
endfunction

" Get build 'object'.
"
function! cmake#build#Get() abort
    return s:build
endfunction
