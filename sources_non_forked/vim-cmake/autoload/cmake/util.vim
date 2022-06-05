" ==============================================================================
" Location:    autoload/cmake/util.vim
" Description: Utility functions
" ==============================================================================

let s:logger = cmake#logger#Get()
let s:system = cmake#system#Get()

let s:repo_dir = expand('<sfile>:p:h:h:h')
let s:data_dir = s:system.Path([s:repo_dir, '.data'], v:false)
let s:data_file = s:system.Path([s:data_dir, 'previous-version.bin'], v:false)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Private functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! s:VersionToNumber(_, version) abort
    let l:version = split(a:version, '\.')
    let l:major = str2nr(l:version[0])
    let l:minor = str2nr(l:version[1])
    let l:patch = str2nr(l:version[2])
    let l:number = l:major * 10000 + l:minor * 100 + l:patch
    return l:number
endfunction

function! s:NumberToVersion(number) abort
    let l:major = a:number / 10000
    let l:minor = (a:number - l:major * 10000) / 100
    let l:patch = a:number - l:major * 10000 - l:minor * 100
    let l:version = l:major . '.' . l:minor . '.' . l:patch
    return l:version
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Public functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Print news of newer Vim-CMake versions.
"
" Params:
"     current_version : String
"         current version of the plugin (in the format <major>.<minor>.<patch>)
"     news : Dictionary
"         dictionary of news, where a key identifies a version (in the format
"         <major>.<minor>.<patch>), and a value is a string containing the news
"         to print for a version
"
function! cmake#util#PrintNews(current_version, news) abort
    " Make a list of all version numbers, transform to integers, and sort.
    let l:all_version_numbers = keys(a:news)
    call map(l:all_version_numbers, function('s:VersionToNumber'))
    call sort(l:all_version_numbers)
    try
        " Try to read previous version number from file.
        let l:line = readfile(s:data_file, 'b')
    catch /.*/
        " Store current version number.
        try
            call mkdir(s:data_dir, 'p')
            call writefile([a:current_version], s:data_file, 'b')
        catch /.*/
        endtry
        return
    endtry
    " Get previous version number from file, then write current version number.
    let l:previous_version_number = s:VersionToNumber('', l:line[0])
    if l:previous_version_number < s:VersionToNumber('', a:current_version)
        try
            call writefile([a:current_version], s:data_file, 'b')
        catch /.*/
        endtry
    endif
    " Print updates for newer versions.
    for l:number in l:all_version_numbers
        if l:number > l:previous_version_number
            for l:news_item in a:news[s:NumberToVersion(l:number)]
                call s:logger.EchoInfo(l:news_item)
            endfor
        endif
    endfor
endfunction
