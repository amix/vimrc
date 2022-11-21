" cache.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2019-01-02.
" @Revision:    125.1.243


" The cache directory. If empty, use |tlib#dir#MyRuntime|.'/cache'.
" You might want to delete old files from this directory from time to 
" time with a command like: >
"   find ~/vimfiles/cache/ -atime +31 -type f -print -delete
TLet g:tlib_cache = ''

" |tlib#cache#Purge()|: Remove cache files older than N days.
TLet g:tlib#cache#purge_days = 31

" Purge the cache every N days. Disable automatic purging by setting 
" this value to a negative value.
TLet g:tlib#cache#purge_every_days = 31

" The encoding used for the purge-cache script.
" Default: 'enc'
TLet g:tlib#cache#script_encoding = &enc

" Whether to run the directory removal script:
"    0 ... No
"    1 ... Query user
"    2 ... Yes
TLet g:tlib#cache#run_script = 1

" Verbosity level:
"     0 ... Be quiet
"     1 ... Display informative message
"     2 ... Display detailed messages
TLet g:tlib#cache#verbosity = 1

" A list of regexps that are matched against partial filenames of the 
" cached files. If a regexp matches, the file won't be removed by 
" |tlib#cache#Purge()|.
TLet g:tlib#cache#dont_purge = ['[\/]\.last_purge$']

" If the cache filename is longer than N characters, use 
" |pathshorten()|.
TLet g:tlib#cache#max_filename = 200

TLet g:tlib#cache#use_json = 0

TLet g:tlib#cache#use_encoding = ''


let s:cache = {}


" :display: tlib#cache#Dir(?mode = 'bg', ?ensure_dir = true)
" The default cache directory.
function! tlib#cache#Dir(...) "{{{3
    TVarArg ['mode', 'bg'], ['ensure_dir', 1]
    let dir = tlib#var#Get('tlib_cache', mode)
    if empty(dir)
        let dir = tlib#file#Join([tlib#dir#MyRuntime(), 'cache'])
    endif
    if ensure_dir
        call tlib#dir#Ensure(dir)
    endif
    return dir
endf


" :display: tlib#cache#EncodedFilename(type, file, ?mkdir=0, ?dir='')
" Encode `file` and call |tlib#cache#Filename()|.
function! tlib#cache#EncodedFilename(type, file, ...) "{{{3
    let file = tlib#url#Encode(a:file)
    return call(function('tlib#cache#Filename'), [a:type, file] + a:000)
endf


" :def: function! tlib#cache#Filename(type, ?file=%, ?mkdir=0, ?dir='')
function! tlib#cache#Filename(type, ...) "{{{3
    " TLogDBG 'bufname='. bufname('.')
    let dir0 = a:0 >= 3 && !empty(a:3) ? a:3 : tlib#cache#Dir()
    let dir = dir0
    if a:0 >= 1 && !empty(a:1)
        let file  = a:1
    else
        if empty(expand('%:t'))
            return ''
        endif
        let file  = expand('%:p')
        let file  = tlib#file#Relative(file, tlib#file#Join([dir, '..']))
    endif
    " TLogVAR file, dir
    let mkdir = a:0 >= 2 ? a:2 : 0
    let file  = substitute(file, '\.\.\|[:&<>]\|//\+\|\\\\\+', '_', 'g')
    let dirs  = [dir, a:type]
    let dirf  = fnamemodify(file, ':h')
    if dirf != '.'
        call add(dirs, dirf)
    endif
    let dir   = tlib#file#Join(dirs)
    " TLogVAR dir
    let dir   = tlib#dir#PlainName(dir)
    " TLogVAR dir
    let file  = fnamemodify(file, ':t')
    " TLogVAR file, dir, mkdir
    let cache_file = tlib#file#Join([dir, file])
    if len(cache_file) > g:tlib#cache#max_filename
        " echom "DBG long filename" cache_file
        " echom "DBG long filename" dir
        if v:version >= 704
            let shortfilename = sha256(file)
        else
            let shortfilename = tlib#hash#Adler32(file)
        endif
        " let cache_file = tlib#cache#Filename(a:type, shortfilename, mkdir, dir0)
        let cache_file = tlib#file#Join([dir, shortfilename])
    else
        if mkdir && !isdirectory(dir)
            try
                call mkdir(dir, 'p')
            catch /^Vim\%((\a\+)\)\=:E739:/
                if filereadable(dir) && !isdirectory(dir)
                    echoerr 'TLib: Cannot create directory for cache file because a file with the same name exists (please delete it):' dir
                    " call delete(dir)
                    " call mkdir(dir, 'p')
                endif
            endtry
        endif
    endif
    " TLogVAR cache_file
    return cache_file
endf


let s:timestamps = {}


function! s:SetTimestamp(cfile, type) "{{{3
    if !has_key(s:timestamps, a:cfile)
        let s:timestamps[a:cfile] = {}
    endif
    let s:timestamps[a:cfile].atime = getftime(a:cfile)
    let s:timestamps[a:cfile][a:type] = s:timestamps[a:cfile].atime
endf


function! s:PutValue(cfile, value) abort "{{{3
    let s:cache[a:cfile] = {'mtime': localtime(), 'data': a:value}
endf


function! s:GetValue(cfile, default) abort "{{{3
    return get(get(s:cache, a:cfile, {}), 'data', a:default)
endf


function! s:GetCacheTime(cfile) abort "{{{3
    let not_found = !has_key(s:cache, a:cfile)
    let cftime = not_found ? -1 : s:cache[a:cfile].mtime
    return cftime
endf


function! tlib#cache#Save(cfile, value, ...) "{{{3
    TVarArg ['options', {}]
    let in_memory = get(options, 'in_memory', 0)
    if in_memory
        " TLogVAR in_memory, a:cfile, localtime()
        call s:PutValue(a:cfile, a:value)
    elseif !empty(a:cfile)
        " TLogVAR a:value
        let cfile = a:cfile
        if g:tlib#cache#use_json && exists('*json_encode')
            try
                let value = json_encode(a:value)
                let cfile .= '.json'
            catch
                echoerr v:exception
                let value = string(a:value)
            endtry
        else
            let value = string(a:value)
        endif
        Tlibtrace 'tlib', cfile, value
        call writefile([value], cfile, 'b')
        call s:SetTimestamp(a:cfile, 'write')
    endif
endf


function! tlib#cache#MTime(cfile) "{{{3
    let mtime = {'mtime': getftime(a:cfile)}
    let mtime = extend(mtime, get(s:timestamps, a:cfile, {}))
    return mtime
endf


function! tlib#cache#Get(cfile, ...) "{{{3
    TVarArg ['default', {}], ['options', {}]
    let in_memory = get(options, 'in_memory', 0)
    if in_memory
        " TLogVAR in_memory, a:cfile
        return s:GetValue(a:cfile, default)
    else
        call tlib#cache#MaybePurge()
        if !empty(a:cfile)
            let jsonfile = a:cfile .'.json'
            let use_json = g:tlib#cache#use_json && exists('*json_decode') && exists('v:none') && filereadable(jsonfile)
            if use_json
                let use_json = 1
                let cfile = jsonfile
            else
                let cfile = a:cfile
            endif
            let mt = s:GetCacheTime(cfile)
            let ft = getftime(cfile)
            if mt != -1 && mt >= ft
                return s:GetValue(cfile, default)
            elseif ft != -1
                call s:SetTimestamp(cfile, 'read')
                let val = join(readfile(cfile, 'b'), '\n')
                try
                    if use_json
                        " NOTE: Copy result of json_decode() in order to 
                        " avoid "E741: value is locked" error in vim8.
                        let value = json_decode(val)
                        if value is v:none
                            let value = default
                        else
                            let value = copy(value)
                        endif
                    else
                        let value = eval(val)
                    endif
                    call s:PutValue(cfile, value)
                    return value
                catch
                    echohl ErrorMsg
                    echom v:exception
                    echom 'tlib#cache#Get: Invalid value in:' cfile
                    echom 'Value:' string(val)
                    echom 'Please review the file and delete it if necessary'
                    echom 'Will use default value:' string(default)
                    echohl NONE
                    if g:tlib#debug
                        let @* = string(val)
                    endif
                    " call s:PutValue(cfile, default)
                    return default
                endtry
            endif
        endif
        return default
    endif
endf


" :display: tlib#cache#Value(cfile, generator, ftime, ?generator_args=[], ?options={})
" Get a cached value from cfile. If it is outdated (compared to ftime) 
" or does not exist, create it calling a generator function.
function! tlib#cache#Value(cfile, generator, ftime, ...) "{{{3
    TVarArg ['args', []], ['options', {}]
    let in_memory = get(options, 'in_memory', 0)
    if in_memory
        let cftime = s:GetCacheTime(a:cfile)
    else
        let cftime = getftime(a:cfile)
    endif
    let ftime = a:ftime
    " TLogVAR in_memory, cftime
    if cftime == -1 || ftime == -1 || (ftime != 0 && cftime < ftime)
        " TLogVAR a:generator, args
        let val = call(a:generator, args)
        " TLogVAR val
        let cval = {'val': val}
        " TLogVAR cval
        call tlib#cache#Save(a:cfile, cval, options)
        return val
    else
        let val = tlib#cache#Get(a:cfile, {}, options)
        if !has_key(val, 'val')
            throw 'tlib#cache#Value: Internal error: '. a:cfile
        else
            return val.val
        endif
    endif
endf


function! tlib#cache#ValueFromName(type, name, ...) abort "{{{3
    let cfile = tlib#cache#Filename(a:type, tlib#url#Encode(a:name), 1)
    return call(function('tlib#cache#Value'), [cfile] + a:000)
endf


" Call |tlib#cache#Purge()| if the last purge was done before 
" |g:tlib#cache#purge_every_days|.
function! tlib#cache#MaybePurge() "{{{3
    if g:tlib#cache#purge_every_days < 0
        return
    endif
    let dir = tlib#cache#Dir('g')
    let last_purge = tlib#file#Join([dir, '.last_purge'])
    let last_purge_exists = filereadable(last_purge)
    if last_purge_exists
        let threshold = localtime() - g:tlib#cache#purge_every_days * g:tlib#date#dayshift
        let should_purge = getftime(last_purge) < threshold
    else
        let should_purge = 0 " should ignore empty dirs, like the tmru one: !empty(glob(tlib#file#Join([dir, '**'])))
    endif
    if should_purge
        if last_purge_exists
            let yn = 'y'
        else
            let txt = "TLib: The cache directory '". dir ."' should be purged of old files.\nDelete files older than ". g:tlib#cache#purge_days ." days now?"
            let yn = tlib#input#Dialog(txt, ['yes', 'no'], 'no')
        endif
        if yn =~ '^y\%[es]$'
            call tlib#cache#Purge()
        else
            let g:tlib#cache#purge_every_days = -1
            if !last_purge_exists
                call s:PurgeTimestamp(dir)
            endif
            echohl WarningMsg
            echom "TLib: Please run :call tlib#cache#Purge() to clean up ". dir
            echohl NONE
        endif
    elseif !last_purge_exists
        call s:PurgeTimestamp(dir)
    endif
endf


" Delete old files.
function! tlib#cache#Purge() "{{{3
    let threshold = localtime() - g:tlib#cache#purge_days * g:tlib#date#dayshift
    let dir = tlib#cache#Dir('g')
    if g:tlib#cache#verbosity >= 1
        echohl WarningMsg
        echom "TLib: Delete files older than ". g:tlib#cache#purge_days ." days from ". dir
        echohl NONE
    endif
    let files = tlib#cache#ListFilesInCache()
    let deldir = []
    let newer = []
    let msg = []
    let more = &more
    set nomore
    try
        for file in files
            if isdirectory(file)
                if empty(filter(copy(newer), 'tlib#string#Strcharpart(v:val, 0, len(file)) ==# file'))
                    call add(deldir, file)
                endif
            else
                if getftime(file) < threshold
                    call s:Delete(msg, file, '')
                else
                    call add(newer, file)
                endif
            endif
        endfor
    finally
        let &more = more
    endtry
    if !empty(msg) && g:tlib#cache#verbosity >= 1
        echo join(msg, "\n")
    endif
    if !empty(deldir)
        let deldir = filter(reverse(sort(deldir)), 's:Delete(msg, v:val, "d")')
        if !empty(deldir)
            if &shell =~ 'sh\(\.exe\)\?$'
                let scriptfile = 'deldir.sh'
                let rmdir = 'rm -rf %s'
            else
                let scriptfile = 'deldir.bat'
                let rmdir = 'rmdir /S /Q %s'
            endif
            let enc = g:tlib#cache#script_encoding
            if has('multi_byte') && enc != &enc
                call map(deldir, 'iconv(v:val, &enc, enc)')
            endif
            let scriptfile = tlib#file#Join([dir, scriptfile])
            if filereadable(scriptfile)
                let script = readfile(scriptfile)
            else
                let script = []
            endif
            let script += map(copy(deldir), 'printf(rmdir, shellescape(v:val, 1))')
            let script = tlib#list#Uniq(script)
            call writefile(script, scriptfile)
            call inputsave()
            if g:tlib#cache#run_script == 0
                if g:tlib#cache#verbosity >= 1
                    echohl WarningMsg
                    if g:tlib#cache#verbosity >= 2
                        echom "TLib: Purged cache. Need to run script to delete directories"
                    endif
                    echom "TLib: Please review and execute: ". scriptfile
                    echohl NONE
                endif
            else
                try
                    let yn = g:tlib#cache#run_script == 2 ? 'y' : tlib#input#Dialog("TLib: About to delete directories by means of a shell script.\nDirectory removal script: ". scriptfile ."\nRun script to delete directories now?", ['yes', 'no', 'edit'], 'no')
                    if yn =~ '^y\%[es]$'
                        exec 'silent cd '. fnameescape(dir)
                        exec '! ' &shell shellescape(scriptfile, 1)
                        exec 'silent cd -'
                        call delete(scriptfile)
                    elseif yn =~ '^e\%[dit]$'
                        exec 'edit '. fnameescape(scriptfile)
                    endif
                finally
                    call inputrestore()
                endtry
            endif
        endif
    endif
    call s:PurgeTimestamp(dir)
endf


function! s:Delete(msg, file, flags) abort "{{{3
    let rv = delete(a:file, a:flags)
    if !rv && g:tlib#cache#verbosity >= 2
        call add(a:msg, "TLib#cache: Delete ". file)
    endif
    return rv
endf


function! s:PurgeTimestamp(dir) "{{{3
    let last_purge = tlib#file#Join([a:dir, '.last_purge'])
    " TLogVAR last_purge
    call writefile([" "], last_purge)
endf

function! tlib#cache#ListFilesInCache(...) "{{{3
    let dir = a:0 >= 1 ? a:1 : tlib#cache#Dir('g')
    if v:version > 702 || (v:version == 702 && has('patch51'))
        let filess = glob(tlib#file#Join([dir, '**']), 1)
    else
        let filess = glob(tlib#file#Join([dir, '**']))
    endif
    let files = reverse(split(filess, '\n'))
    let pos0 = len(tlib#dir#CanonicName(dir))
    call filter(files, 's:ShouldPurge(tlib#string#Strcharpart(v:val, pos0))')
    return files
endf


function! s:ShouldPurge(partial_filename) "{{{3
    " TLogVAR a:partial_filename
    for rx in g:tlib#cache#dont_purge
        if a:partial_filename =~ rx
            " TLogVAR a:partial_filename, rx
            return 0
        endif
    endfor
    return 1
endf

