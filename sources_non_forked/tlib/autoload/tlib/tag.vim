" tag.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-11-01.
" @Last Change: 2011-03-10.
" @Revision:    0.0.53

if &cp || exists("loaded_tlib_tag_autoload")
    finish
endif
let loaded_tlib_tag_autoload = 1


" :def: function! tlib#tag#Retrieve(rx, ?extra_tags=0)
" Get all tags matching rx. Basically, this function simply calls 
" |taglist()|, but when extra_tags is true, the list of the tag files 
" (see 'tags') is temporarily expanded with |g:tlib_tags_extra|.
"
" Example use:
" If you want to include tags for, eg, JDK, normal tags use can become 
" slow. You could proceed as follows:
"     1. Create a tags file for the JDK sources. When creating the tags 
"     file, make sure to include inheritance information and the like 
"     (command-line options like --fields=+iaSm --extra=+q should be ok).
"     In this example, we want tags only for public methods (there are 
"     most likely better ways to do this): >
"          ctags -R --fields=+iaSm --extra=+q ${JAVA_HOME}/src
"          head -n 6 tags > tags0
"          grep access:public tags >> tags0
" <    2. Make 'tags' include project specific tags files. In 
"      ~/vimfiles/after/ftplugin/java.vim insert: >
"          let b:tlib_tags_extra = $JAVA_HOME .'/tags0'
" <    3. When this function is invoked as >
"          echo tlib#tag#Retrieve('print')
" <    it will return only project-local tags. If it is invoked as >
"          echo tlib#tag#Retrieve('print', 1)
" <    tags from the JDK will be included.
function! tlib#tag#Retrieve(rx, ...) "{{{3
    TVarArg ['extra_tags', 0]
    if extra_tags
        let tags_orig = &l:tags
        if empty(tags_orig)
            setlocal tags<
        endif
        try
            let more_tags = tlib#var#Get('tlib_tags_extra', 'bg')
            if !empty(more_tags)
                let &l:tags .= ','. more_tags
            endif
            let taglist = taglist(a:rx)
        finally
            let &l:tags = tags_orig
        endtry
    else
        let taglist = taglist(a:rx)
    endif
    return taglist
endf


" Retrieve tags that meet the constraints (a dictionnary of fields and 
" regexp, with the exception of the kind field which is a list of chars). 
" For the use of the optional use_extra argument see 
" |tlib#tag#Retrieve()|.
" :def: function! tlib#tag#Collect(constraints, ?use_extra=1, ?match_front=1)
function! tlib#tag#Collect(constraints, ...) "{{{3
    TVarArg ['use_extra', 0], ['match_end', 1], ['match_front', 1]
    " TLogVAR a:constraints, use_extra
    let rx = get(a:constraints, 'name', '')
    if empty(rx) || rx == '*'
        let rx = '.'
    else
        let rxl = ['\C']
        if match_front
            call add(rxl, '^')
        endif
        " call add(rxl, tlib#rx#Escape(rx))
        call add(rxl, rx)
        if match_end
            call add(rxl, '$')
        endif
        let rx = join(rxl, '')
    endif
    " TLogVAR rx, use_extra
    let tags = tlib#tag#Retrieve(rx, use_extra)
    " TLogDBG len(tags)
    for [field, rx] in items(a:constraints)
        if !empty(rx) && rx != '*'
            " TLogVAR field, rx
            if field == 'kind'
                call filter(tags, 'v:val.kind =~ "['. rx .']"')
            elseif field != 'name'
                call filter(tags, '!empty(get(v:val, field)) && get(v:val, field) =~ rx')
            endif
        endif
    endfor
    " TLogVAR tags
    return tags
endf


function! tlib#tag#Format(tag) "{{{3
    if has_key(a:tag, 'signature')
        let name = a:tag.name . a:tag.signature
    elseif a:tag.cmd[0] == '/'
        let name = a:tag.cmd
        let name = substitute(name, '^/\^\?\s*', '', '')
        let name = substitute(name, '\s*\$\?/$', '', '')
        let name = substitute(name, '\s\{2,}', ' ', 'g')
        let tsub = tlib#var#Get('tlib_tag_substitute', 'bg')
        if has_key(tsub, &filetype)
            for [rx, rplc, sub] in tsub[&filetype]
                let name = substitute(name, rx, rplc, sub)
            endfor
        endif
    else
        let name = a:tag.name
    endif
    return name
endf

