" ==============================================================================
" Location:    autoload/cmake.vim
" Description: API functions and global data for Vim-CMake
" ==============================================================================

let s:buildsys = cmake#buildsys#Get()
let s:build = cmake#build#Get()
let s:const = cmake#const#Get()
let s:logger = cmake#logger#Get()
let s:terminal = cmake#terminal#Get()

" Print news of new Vim-CMake versions.
call cmake#util#PrintNews(s:const.plugin_version, s:const.plugin_news)

" Log config options.
call s:logger.LogInfo('Configuration options:')
for s:cvar in sort(keys(s:const.config_vars))
    call s:logger.LogInfo('> g:%s: %s', s:cvar, string(g:[s:cvar]))
endfor

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" API functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" API function for :CMakeGenerate and <Plug>(CMakeGenerate).
"
" Params:
"     clean : Number
"         whether to clean before generating
"     a:1 : String
"         (optional) build configuration and additional CMake options
"
function! cmake#Generate(clean, ...) abort
    call s:logger.LogDebug('API invoked: cmake#Generate(%s, %s)', a:clean, a:000)
    call s:buildsys.Generate(a:clean, join(a:000))
endfunction

" API function for :CMakeClean and <Plug>(CMakeClean).
"
function! cmake#Clean() abort
    call s:logger.LogDebug('API invoked: cmake#Clean()')
    call s:buildsys.Clean()
endfunction

" API function for :CMakeSwitch.
"
" Params:
"     a:1 : String
"         build configuration
"
function! cmake#Switch(...) abort
    call s:logger.LogDebug('API invoked: cmake#Switch(%s)', string(a:1))
    call s:buildsys.Switch(a:1)
endfunction

" API function for completion for :CMakeSwitch.
"
" Params:
"     arg_lead : String
"         the leading portion of the argument currently being completed
"     cmd_line : String
"         the entire command line
"     cursor_pos : Number
"         the cursor position in the command line (byte index)
"
" Returns:
"     String
"         stringified list of existing configuration directories
"
function! cmake#GetConfigs(arg_lead, cmd_line, cursor_pos) abort
    call s:logger.LogDebug('API invoked: cmake#GetConfigs()')
    return join(s:buildsys.GetConfigs(), "\n")
endfunction

" API function for :CMakeBuild and <Plug>(CMakeBuild).
"
" Params:
"     clean : Number
"         whether to clean before building
"     a:1 : String
"         (optional) target and other build options
"
function! cmake#Build(clean, ...) abort
    call s:logger.LogDebug('API invoked: cmake#Build(%s, %s)', a:clean, a:000)
    call s:build.Build(a:clean, join(a:000))
endfunction

" API function for :CMakeInstall and <Plug>(CMakeInstall).
"
function! cmake#Install() abort
    call s:logger.LogDebug('API invoked: cmake#Install()')
    call s:build.Install()
endfunction

" API function for completion for :CMakeBuild.
"
" API function for completion for :CMakeBuild.
"
" Params:
"     arg_lead : String
"         the leading portion of the argument currently being completed
"     cmd_line : String
"         the entire command line
"     cursor_pos : Number
"         the cursor position in the command line (byte index)
"
" Returns:
"     String
"         available targets, one per line
"
function! cmake#GetBuildTargets(arg_lead, cmd_line, cursor_pos) abort
    call s:logger.LogDebug('API invoked: cmake#GetBuildTargets()')
    return join(s:buildsys.GetTargets(), "\n")
endfunction

" API function for :CMakeStop.
"
function! cmake#Stop() abort
    call s:logger.LogDebug('API invoked: cmake#Stop()')
    call s:terminal.Stop()
endfunction

" API function for :CMakeOpen.
"
function! cmake#Open() abort
    call s:logger.LogDebug('API invoked: cmake#Open()')
    call s:terminal.Open(v:false)
endfunction

" API function for :CMakeClose.
"
function! cmake#Close() abort
    call s:logger.LogDebug('API invoked: cmake#Close()')
    call s:terminal.Close()
endfunction
