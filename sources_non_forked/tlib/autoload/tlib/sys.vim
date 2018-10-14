" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2015-11-07.
" @Revision:    51


if !exists('g:tlib#sys#special_protocols')
    " A list of |regexp|s matching protocol names that should be handled 
    " by |g:tlib#sys#system_browser|.
    " CAVEAT: Must be a |\V| |regexp|.
    let g:tlib#sys#special_protocols = ['https\?', 'nntp', 'mailto']   "{{{2
endif


if !exists('g:tlib#sys#special_suffixes')
    " A list of |regexp|s matching suffixes that should be handled by 
    " |g:tlib#sys#system_browser|.
    " CAVEAT: Must be a |\V| |regexp|.
    let g:tlib#sys#special_suffixes = ['xlsx\?', 'docx\?', 'pptx\?', 'accdb', 'mdb', 'sqlite', 'pdf', 'jpg', 'png', 'gif']    "{{{2
endif


if !exists('g:tlib#sys#system_rx')
    " Open links matching this |regexp| with |g:tlib#sys#system_browser|.
    " CAVEAT: Must be a |\V| |regexp|.
    let g:tlib#sys#system_rx = printf('\V\%(\^\%(%s\):\|.\%(%s\)\)', join(g:tlib#sys#special_protocols, '\|'), join(g:tlib#sys#special_suffixes, '\|'))   "{{{2
endif


if !exists("g:tlib#sys#system_browser")
    if exists('g:netrw_browsex_viewer')
        " Open files in the system browser.
        " :read: let g:tlib#sys#system_browser = ... "{{{2
        let g:tlib#sys#system_browser = "exec 'silent !'. g:netrw_browsex_viewer shellescape('%s')"
    elseif has("win32") || has("win16") || has("win64")
        " let g:tlib#sys#system_browser = "exec 'silent ! start \"\"' shellescape('%s')"
        let g:tlib#sys#system_browser = "exec 'silent ! RunDll32.EXE URL.DLL,FileProtocolHandler' shellescape('%s')"
    elseif has("mac")
        let g:tlib#sys#system_browser = "exec 'silent !open' shellescape('%s')"
    elseif exists('$XDG_CURRENT_DESKTOP') && !empty($XDG_CURRENT_DESKTOP)
        let g:tlib#sys#system_browser = "exec 'silent !xdg-open' shellescape('%s') '&'"
    elseif $GNOME_DESKTOP_SESSION_ID != "" || $DESKTOP_SESSION == 'gnome'
        let g:tlib#sys#system_browser = "exec 'silent !gnome-open' shellescape('%s')"
    elseif exists("$KDEDIR") && !empty($KDEDIR)
        let g:tlib#sys#system_browser = "exec 'silent !kfmclient exec' shellescape('%s')"
    else
        let g:tlib#sys#system_browser = ''
    endif
endif


if !exists('g:tlib#sys#windows')
    let g:tlib#sys#windows = &shell !~ 'sh' && (has('win16') || has('win32') || has('win64'))   "{{{2
endif


if !exists('g:tlib#sys#null')
    let g:tlib#sys#null = g:tlib#sys#windows ? 'NUL' : (filereadable('/dev/null') ? '/dev/null' : '')    "{{{2
endif


let s:cygwin = {}

function! tlib#sys#IsCygwinBin(cmd) "{{{3
    " TLogVAR a:cmd
    if !g:tlib#sys#windows
        return 0
    elseif has_key(s:cygwin, a:cmd)
        let rv = s:cygwin[a:cmd]
    else
        if !tlib#sys#IsExecutable('cygpath', 1) || !tlib#sys#IsExecutable('which', 1)
            let rv = 0
        else
            let which = substitute(system('which '. shellescape(a:cmd)), '\n$', '', '')
            " echom "DBG which:" which
            if which =~ '^/'
                let filename = system('cygpath -ma '. shellescape(which))
                " echom "DBG filename:" filename
                let rv = filename =~ g:tlib#sys#cygwin_path_rx
            else
                let rv = 0
            endif
        endif
        let s:cygwin[a:cmd] = rv
    endif
    " TLogVAR rv
    return rv
endf


let s:executables = {}

function! tlib#sys#IsExecutable(cmd, ...) "{{{3
    " TLogVAR a:cmd
    " echom "DBG has_key(s:executables, a:cmd)" has_key(s:executables, a:cmd)
    if !has_key(s:executables, a:cmd)
        let executable = executable(a:cmd)
        " TLogVAR 1, executable
        let ignore_cyg = a:0 >= 1 ? a:1 : !g:tlib#sys#windows
        if !executable && !ignore_cyg
            let executable = tlib#sys#IsCygwinBin(a:cmd)
            " TLogVAR 2, executable
        endif
        let s:executables[a:cmd] = executable
    endif
    " echom "DBG s:executables[a:cmd]" s:executables[a:cmd]
    return s:executables[a:cmd]
endf


if !exists('g:tlib#sys#check_cygpath')
    " If true, check whether we have to convert a path via cyppath -- 
    " see |tlib#sys#MaybeUseCygpath|
    let g:tlib#sys#check_cygpath = g:tlib#sys#windows && tlib#sys#IsExecutable('cygpath', 1)   "{{{2
endif


if !exists('g:tlib#sys#cygwin_path_rx')
    " If a full windows filename (with slashes instead of backslashes) 
    " matches this |regexp|, it is assumed to be a cygwin executable.
    let g:tlib#sys#cygwin_path_rx = '/cygwin/'   "{{{2
endif


if !exists('g:tlib#sys#cygwin_expr')
    " For cygwin binaries, convert command calls using this vim 
    " expression.
    let g:tlib#sys#cygwin_expr = '"bash -c ''". escape(%s, "''\\") ."''"'   "{{{2
endif


function! tlib#sys#GetCmd(cmd) "{{{3
    if !empty(g:tlib#sys#cygwin_expr) && tlib#sys#IsCygwinBin(matchstr(a:cmd, '^\S\+'))
        let cmd = eval(printf(g:tlib#sys#cygwin_expr, string(a:cmd)))
        " TLogVAR cmd
        return cmd
    else
        return a:cmd
    endif
endf


" If cmd seems to be a cygwin executable, use cygpath to convert 
" filenames. This assumes that cygwin's which command returns full 
" filenames for non-cygwin executables.
function! tlib#sys#MaybeUseCygpath(cmd) "{{{3
    " echom "DBG" a:cmd
    if g:tlib#sys#check_cygpath && tlib#sys#IsCygwinBin(a:cmd)
        return 'cygpath -u "%s"'
    endif
    return ''
endf


function! tlib#sys#ConvertPath(converter, filename) "{{{3
    return tlib#string#Chomp(system(printf(a:converter, shellescape(a:filename))))
endf


let s:native_filenames = {}

function! tlib#sys#FileArgs(cmd, files) "{{{3
    let cygpath = tlib#sys#MaybeUseCygpath(a:cmd)
    " TLogVAR cygpath
    if empty(cygpath)
        return a:files
    else
        let files = map(copy(a:files), 'has_key(s:native_filenames, v:val) ? s:native_filenames[v:val] : tlib#sys#CygPath(v:val)')
        return files
    endif
endf


" Check whether filename matches |g:tlib#sys#system_rx|, i.e. whether it 
" is a special file that should not be opened in vim.
function! tlib#sys#IsSpecial(filename) abort "{{{3
    return a:filename =~ g:tlib#sys#system_rx
endf


" Open filename with the default OS application (see 
" |g:tlib#sys#system_browser|), if |tlib#sys#IsSpecial()| return 1. 
" Returns 1 if successful or 0 otherwise.
function! tlib#sys#Open(filename) abort "{{{3
    Tlibtrace 'tlib', a:filename
    if !empty(g:tlib#sys#system_browser) && tlib#sys#IsSpecial(a:filename)
        try
            let cmd = printf(g:tlib#sys#system_browser, escape(a:filename, ' %#!'))
            Tlibtrace 'tlib', cmd
            exec cmd
            return 1
        catch
            echohl ErrorMsg
            echom v:exception
            echohl NONE
        endtry
    endif
    return 0
endf


" :def: function! tlib#sys#SystemInDir(dir, expr, ?input='')
function! tlib#sys#SystemInDir(dir, ...) abort "{{{3
    call tlib#dir#CD(a:dir)
    try
        return call(function('system'), a:000)
    finally
        cd! -
    endtry
endf

