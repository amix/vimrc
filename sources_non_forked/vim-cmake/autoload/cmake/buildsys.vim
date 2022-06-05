" ==============================================================================
" Location:    autoload/cmake/buildsys.vim
" Description: Functions for generating the buildsystem
" ==============================================================================

let s:buildsys = {}
let s:buildsys.cmake_version = 0
let s:buildsys.project_root = ''
let s:buildsys.current_config = ''
let s:buildsys.path_to_current_config = ''
let s:buildsys.configs = []
let s:buildsys.targets = []

let s:logger = cmake#logger#Get()
let s:statusline = cmake#statusline#Get()
let s:system = cmake#system#Get()
let s:terminal = cmake#terminal#Get()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Private functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Find project root by looking for g:cmake_root_markers upwards.
"
" Returns:
"     String
"         escaped path to the root of the project
"
function! s:FindProjectRoot() abort
    let l:root = getcwd()
    let l:escaped_cwd = fnameescape(getcwd())
    for l:marker in g:cmake_root_markers
        " Search CWD upward for l:marker, assuming it is a file.
        let l:marker_path = findfile(l:marker, l:escaped_cwd . ';' . $HOME)
        if len(l:marker_path) > 0
            " If found, strip l:marker from it.
            let l:root = fnamemodify(l:marker_path, ':h')
            break
        endif
        " Search CWD upward for l:marker, assuming it is a directory.
        let l:marker_path = finddir(l:marker, l:escaped_cwd . ';' . $HOME)
        if len(l:marker_path) > 0
            " If found, strip l:marker from it.
            let l:root = fnamemodify(l:marker_path, ':h')
            break
        endif
    endfor
    return l:root
endfunction

" Get absolute path to location where the build directory is located.
"
" Returns:
"     String
"         path to build directory location
"
function! s:GetBuildDirLocation() abort
    return s:system.Path(
            \ [s:buildsys.project_root, g:cmake_build_dir_location], v:false)
endfunction

" Find CMake variable in list of options.
"
" Params:
"     opts : List
"         list of options
"     variable : String
"         variable to find
"
" Returns:
"     String
"         value of the CMake variable, or an empty string if the variable was
"         not found
"
" Example:
"     to find the variable 'CMAKE_BUILD_TYPE', which would be passed by the user
"     as '-D CMAKE_BUILD_TYPE=<value>', call
"             s:FindVarInOpts(opts, 'CMAKE_BUILD_TYPE')
"
function! s:FindVarInOpts(opts, variable) abort
    if len(a:opts) > 0
        " Search the list of command-line options for an entry matching
        " '-D <variable>=<value>' or '-D <variable>:<type>=<value>' or
        " '-D<variable>=<value>' or '-D<variable>:<type>=<value>'.
        let l:opt = matchstr(a:opts, '\m\C-D\s*' . a:variable)
        " If found, return the value, otherwise return an empty string.
        if len(l:opt) > 0
            return split(l:opt, '=')[1]
        else
            return ''
        endif
    endif
endfunction

" Process build configuration.
"
" Params:
"     opts : List
"         list of options
"
function! s:ProcessBuildConfig(opts) abort
    let l:config = s:buildsys.current_config
    " Check if the first entry of the list of command-line options starts with a
    " letter (and not with a dash), in which case the user will have passed the
    " name of the build configuration as the first option.
    if (len(a:opts) > 0) && (match(a:opts[0], '\m\C^\w') >= 0)
        " Update build config name and remove from list of options.
        let l:config = a:opts[0]
        call s:SetCurrentConfig(l:config)
        call remove(a:opts, 0)
    endif
    " If the list of command-line options does not contain an explicit value for
    " the 'CMAKE_BUILD_TYPE' variable, add it.
    if s:FindVarInOpts(a:opts, 'CMAKE_BUILD_TYPE') ==# ''
        call add(a:opts, '-D CMAKE_BUILD_TYPE=' . l:config)
    endif
endfunction

" Get list of command-line options from string of arguments.
"
" Params:
"     argstring : String
"         string containing command-line arguments
"
" Returns:
"     List
"         list of unprocessed command-line options
"
" Example:
"     an argument string like the following
"         'Debug -D VAR_A=1 -DVAR_B=0 -Wdev -U VAR_C'
"     results in a list of options like the following
"         ['Debug', '-D VAR_A=1', '-DVAR_B=0', '-Wdev', '-U VAR_C']
"
function! s:ArgStringToOptList(argstring) abort
    let l:opts = []
    for l:arg in split(a:argstring)
        " If list of options is empty, append first argument.
        if len(l:opts) == 0
            call add(l:opts, l:arg)
        " If argument starts with a dash, append it to the list of options.
        elseif match(l:arg, '\m\C^-') >= 0
            call add(l:opts, l:arg)
        " If argument does not start with a dash, it must belong to the last
        " option that was added to the list, thus extend that option.
        else
            let l:opts[-1] = join([l:opts[-1], l:arg])
        endif
    endfor
    return l:opts
endfunction

" Process string of arguments and return parsed options.
"
" Params:
"     argstring : String
"         string containing command-line arguments
"
" Returns:
"     Dictionary
"         opts : List
"             list of options
"         source_dir : String
"             path to source directory
"         build_dir : String
"             path to build directory
"
function! s:ProcessArgString(argstring) abort
    let l:opts = s:ArgStringToOptList(a:argstring)
    call s:ProcessBuildConfig(l:opts)
    " If compile commands are to be exported, and the
    " 'CMAKE_EXPORT_COMPILE_COMMANDS' variable is not set, set it.
    if g:cmake_link_compile_commands
        if s:FindVarInOpts(l:opts, 'CMAKE_EXPORT_COMPILE_COMMANDS') ==# ''
            call add(l:opts, '-D CMAKE_EXPORT_COMPILE_COMMANDS=ON')
        endif
    endif
    " Set source and build directories. Must be done after processing the build
    " configuration so that the current build configuration is up to date before
    " setting the build directory.
    let l:source_dir = s:system.Path([s:buildsys.project_root], v:true)
    let l:build_dir = s:system.Path([s:buildsys.path_to_current_config], v:true)
    " Return dictionary of options.
    let l:optdict = {}
    let l:optdict.opts = l:opts
    let l:optdict.source_dir = l:source_dir
    let l:optdict.build_dir = l:build_dir
    return l:optdict
endfunction

" Refresh list of build configuration directories.
"
function! s:RefreshConfigs() abort
    " List of directories inside of which a CMakeCache file is found.
    let l:cache_dirs = findfile(
            \ 'CMakeCache.txt',
            \ s:GetBuildDirLocation() . '/**1',
            \ -1)
    " Transform paths to just names of directories. These will be the names of
    " existing configuration directories.
    call map(l:cache_dirs, {_, val -> fnamemodify(val, ':h:t')})
    let s:buildsys.configs = l:cache_dirs
    call s:logger.LogDebug('Build configs: %s', s:buildsys.configs)
endfunction

" Callback for RefreshTargets().
"
function! s:RefreshTargetsCb(...) abort
    let l:data = s:system.ExtractStdoutCallbackData(a:000)
    for l:line in l:data
        if match(l:line, '\m\C\.\.\.\s') == 0
            let l:target = split(l:line)[1]
            let s:buildsys.targets += [l:target]
        endif
    endfor
endfunction

" Refresh list of available CMake targets.
"
function! s:RefreshTargets() abort
    let s:buildsys.targets = []
    let l:build_dir = s:buildsys.path_to_current_config
    let l:command = [g:cmake_command,
            \ '--build', l:build_dir,
            \ '--target', 'help'
            \ ]
    call s:system.JobRun(
            \ l:command, v:true, function('s:RefreshTargetsCb'), v:null, v:false)
endfunction

" Check if build configuration directory exists.
"
" Params:
"     config : String
"         configuration to check
"
" Returns:
"     Boolean
"         v:true if the build configuration exists, v:false otherwise
"
function! s:ConfigExists(config) abort
    return index(s:buildsys.configs, a:config) >= 0
endfunction

" Set current build configuration.
"
" Params:
"     config : String
"         build configuration name
"
function! s:SetCurrentConfig(config) abort
    let s:buildsys.current_config = a:config
    let l:path = s:system.Path([s:GetBuildDirLocation(), a:config], v:false)
    let s:buildsys.path_to_current_config = l:path
    call s:logger.LogInfo('Current config: %s (%s)',
            \ s:buildsys.current_config,
            \ s:buildsys.path_to_current_config
            \ )
    call s:statusline.SetBuildInfo(s:buildsys.current_config)
endfunction

" Link compile commands from source directory to build directory.
"
function! s:LinkCompileCommands() abort
    if !g:cmake_link_compile_commands
        return
    endif
    let l:target = s:system.Path(
            \ [s:buildsys.path_to_current_config, 'compile_commands.json'],
            \ v:true
            \ )
    let l:link = s:system.Path(
            \ [s:buildsys.project_root, 'compile_commands.json'],
            \ v:true,
            \ )
    let l:command = [g:cmake_command, '-E', 'create_symlink', l:target, l:link]
    call s:system.JobRun(l:command, v:true, v:null, v:null, v:false)
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Public functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Generate a buildsystem for the project using CMake.
"
" Params:
"     clean : Boolean
"         whether to clean before generating
"     argstring : String
"         build configuration and additional CMake options
"
function! s:buildsys.Generate(clean, argstring) abort
    call s:logger.LogDebug('Invoked: buildsys.Generate(%s, %s)',
            \ a:clean, string(a:argstring))
    let l:command = [g:cmake_command]
    let l:optdict = s:ProcessArgString(a:argstring)
    " Construct command.
    call extend(l:command, g:cmake_generate_options)
    call extend(l:command, l:optdict.opts)
    if l:self.cmake_version < 313
        call add(l:command, '-H' . l:optdict.source_dir)
        call add(l:command, '-B' . l:optdict.build_dir)
    else
        call add(l:command, '-S ' . l:optdict.source_dir)
        call add(l:command, '-B ' . l:optdict.build_dir)
    endif
    " Clean project buildsystem, if requested.
    if a:clean
        call l:self.Clean()
    endif
    " Run generate command.
    call s:terminal.Run(
            \ l:command, 'generate',
            \ [
                \ function('s:RefreshConfigs'),
                \ function('s:RefreshTargets'),
                \ function('s:LinkCompileCommands')
            \ ],
            \ [function('s:RefreshConfigs')],
            \ [], []
            \ )
endfunction

" Clean buildsystem.
"
function! s:buildsys.Clean() abort
    call s:logger.LogDebug('Invoked: buildsys.Clean()')
    if isdirectory(l:self.path_to_current_config)
        call delete(l:self.path_to_current_config, 'rf')
    endif
    call s:RefreshConfigs()
endfunction

" Set current build configuration after checking that the configuration exists.
"
" Params:
"     config : String
"         build configuration name
"
function! s:buildsys.Switch(config) abort
    call s:logger.LogDebug('Invoked: buildsys.Switch(%s)', a:config)
    " Check that config exists.
    if !s:ConfigExists(a:config)
        call s:logger.EchoError(
                \ "Build configuration '%s' not found, run ':CMakeGenerate %s'",
                \ a:config, a:config)
        call s:logger.LogError(
                \ "Build configuration '%s' not found, run ':CMakeGenerate %s'",
                \ a:config, a:config)
        return
    endif
    call s:SetCurrentConfig(a:config)
    call s:LinkCompileCommands()
endfunction

" Get list of configuration directories (containing a buildsystem).
"
" Returns:
"     List
"         list of existing configuration directories
"
function! s:buildsys.GetConfigs() abort
    return l:self.configs
endfunction

" Get list of available build targets.
"
" Returns:
"     List
"         list of available build targets
"
function! s:buildsys.GetTargets() abort
    if len(l:self.targets) == 0
        call s:RefreshTargets()
    endif
    return l:self.targets
endfunction

" Get current build configuration.
"
" Returns:
"     String
"         build configuration
"
function! s:buildsys.GetCurrentConfig() abort
    return l:self.current_config
endfunction

" Get path to CMake source directory of current project.
"
" Returns:
"     String
"         path to CMake source directory
"
function! s:buildsys.GetSourceDir() abort
    return l:self.project_root
endfunction

" Get path to current build configuration.
"
" Returns:
"     String
"         path to build configuration
"
function! s:buildsys.GetPathToCurrentConfig() abort
    return l:self.path_to_current_config
endfunction

" Get buildsys 'object'.
"
function! cmake#buildsys#Get() abort
    return s:buildsys
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Initialization
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! s:GetCMakeVersionCb(...) abort
    let l:data = s:system.ExtractStdoutCallbackData(a:000)
    for l:line in l:data
        if match(l:line, '\m\C^cmake\S* version') == 0
            let l:version_str = split(split(l:line)[2], '\.')
            let l:major = str2nr(l:version_str[0])
            let l:minor = str2nr(l:version_str[1])
            let s:buildsys.cmake_version = l:major * 100 + l:minor
            break
        endif
    endfor
endfunction

" Get CMake version. The version is stored as MAJOR * 100 + MINOR (e.g., version
" 3.13.3 would result in 313).
let s:command = [g:cmake_command, '--version']
call s:system.JobRun(
        \ s:command, v:true, function('s:GetCMakeVersionCb'), v:null, v:false)

" Must be done before any other initial configuration.
let s:buildsys.project_root = s:system.Path([s:FindProjectRoot()], v:false)
call s:logger.LogInfo('Project root: %s', s:buildsys.project_root)

call s:SetCurrentConfig(g:cmake_default_config)
call s:RefreshConfigs()
