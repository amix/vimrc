" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2012-03-08.
" @Last Change: 2015-11-07.
" @Revision:    190

scriptencoding utf-8


" A dictionarie of supported VCS (currently: git, hg, svn, bzr).
" :display: g:tlib#vcs#def                 {...}
TLet g:tlib#vcs#def = {
            \ 'git': {
            \     'dir': '.git',
            \     'ls': 'git ls-files --full-name',
            \     'ls.postprocess': '*tlib#vcs#GitLsPostprocess',
            \     'diff': 'git diff --no-ext-diff -U0 %s'
            \ },
            \ 'hg': {
            \     'dir': '.hg',
            \     'diff': 'hg diff -U0 %s',
            \     'ls': 'hg manifest'
            \ },
            \ 'svn': {
            \     'dir': '.svn',
            \     'diff': 'svn diff --diff-cmd diff --extensions -U0 %s',
            \ },
            \ 'bzr': {
            \     'dir': '.bzr',
            \     'diff': 'bzr diff --diff-options=-U0 %s',
            \ }
            \ }


" A dictionary of custom executables for VCS commands. If the value is 
" empty, support for that VCS will be removed. If no key is present, it 
" is assumed that the VCS "type" is the name of the executable.
" :display: g:tlib#vcs#executables         {...}
TLet g:tlib#vcs#executables = {} 


" If non-empty, use it as a format string to check whether a VCS is 
" installed on your computer.
TLet g:tlib#vcs#check = has('win16') || has('win32') || has('win64') ? '%s.exe' : '%s'


if !empty(g:tlib#vcs#check)
    for [s:cmd, s:def] in items(g:tlib#vcs#def)
        if !has_key(g:tlib#vcs#executables, s:cmd)
            let s:cmd1 = printf(g:tlib#vcs#check, s:cmd)
            let g:tlib#vcs#executables[s:cmd] = executable(s:cmd1) ? s:cmd1 : ''
        endif
    endfor
    unlet! s:cmd s:def s:cmd1
endif


function! tlib#vcs#Executable(type) "{{{3
    return get(g:tlib#vcs#executables, a:type, '')
endf


function! tlib#vcs#FindVCS(filename) "{{{3
    let type = ''
    let dir  = ''
    let dirname = fnamemodify(a:filename, isdirectory(a:filename) ? ':p' : ':p:h')
    let path = escape(dirname, ';') .';'
    " TLogVAR a:filename, dirname, path
    Tlibtrace 'tlib', a:filename, path
    let depth = -1
    for vcs in keys(g:tlib#vcs#def)
        let subdir = g:tlib#vcs#def[vcs].dir
        let vcsdir = finddir(subdir, path)
        " TLogVAR vcs, subdir, vcsdir
        Tlibtrace 'tlib', vcs, subdir, vcsdir
        if !empty(vcsdir)
            let vcsdir_depth = len(split(fnamemodify(vcsdir, ':p'), '\/'))
            if vcsdir_depth > depth
                let depth = vcsdir_depth
                let type = vcs
                let dir = vcsdir
                " TLogVAR type, depth
            endif
        endif
    endfor
    Tlibtrace 'tlib', type, dir
    " TLogVAR type, dir
    if empty(type)
        return ['', '']
    else
        return [type, dir]
    endif
endf


function! s:GetCmd(vcstype, cmd)
    let vcsdef = get(g:tlib#vcs#def, a:vcstype, {})
    if has_key(vcsdef, a:cmd)
        let cmd = vcsdef[a:cmd]
        if cmd =~ '^\*'
            let cmd = substitute(cmd, '^\*', '', '')
        else
            let bin = get(g:tlib#vcs#executables, a:vcstype, '')
            if empty(bin)
                let cmd = ''
            elseif bin != a:vcstype
                " let bin = escape(shellescape(bin), '\')
                let bin = escape(bin, '\')
                let cmd = substitute(cmd, '^.\{-}\zs'. escape(a:vcstype, '\'), bin, '')
            endif
        endif
        return cmd
    else
        return ''
    endif
endf


" :display: tlib#vcs#Ls(?filename=bufname('%'), ?vcs=[type, dir])
" Return the files under VCS.
function! tlib#vcs#Ls(...) "{{{3
    if a:0 >= 2
        let vcs = a:2
    else
        let vcs = tlib#vcs#FindVCS(a:0 >= 1 ? a:1 : bufname('%'))
    endif
    Tlibtrace 'tlib', vcs, a:000
    " TLogVAR vcs
    if !empty(vcs)
        let [vcstype, vcsdir] = vcs
        if has_key(g:tlib#vcs#def, vcstype)
            let ls = s:GetCmd(vcstype, 'ls')
            " TLogVAR ls
            if !empty(ls)
                let rootdir = fnamemodify(vcsdir, ':p:h:h')
                " TLogVAR vcsdir, rootdir
                if ls =~ '%s'
                    let cmd = printf(ls, shellescape(rootdir))
                else
                    let cmd = ls
                endif
                " TLogVAR cmd, getcwd()
                Tlibtrace 'tlib', getcwd(), vcstype, vcsdir, rootdir, cmd
                let filess = tlib#sys#SystemInDir(rootdir, cmd)
                " TLogVAR filess
                let files = split(filess, '\n')
                let postprocess = s:GetCmd(vcstype, 'ls.postprocess')
                if !empty(postprocess)
                    call map(files, 'call(postprocess, [v:val])')
                endif
                call map(files, 'join([rootdir, v:val], "/")')
                " TLogVAR files
                return files
            endif
        endif
    endif
    return []
endf


" :display: tlib#vcs#Diff(filename, ?vcs=[type, dir])
" Return the diff for "filename"
function! tlib#vcs#Diff(filename, ...) "{{{3
    let vcs = a:0 >= 1 ? a:1 : tlib#vcs#FindVCS(a:filename)
    if !empty(vcs)
        let [vcstype, vcsdir] = vcs
        let diff = s:GetCmd(vcstype, 'diff')
        if !empty(diff)
            let cmd = printf(diff, shellescape(fnamemodify(a:filename, ':p')))
            let patch = system(cmd)
            return patch
        endif
    endif
    return []
endf


function! tlib#vcs#GitLsPostprocess(filename) abort "{{{3
    if a:filename =~ '^".\{-}"$'
        let filename = matchstr(a:filename, '^"\zs.\{-}\ze"$')
        let filename = substitute(filename, '\%(\\\@<!\\\(\d\d\d\)\)\+', '\=eval(''"''. submatch(0) .''"'')', 'g')
        " let filename = eval(a:filename)
        " TLogVAR a:filename, filename, &enc
        return filename
    else
        return a:filename
    endif
endf

