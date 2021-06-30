"
" Python filetype plugin for running flake8
" Language:     Python (ft=python)
" Maintainer:   Vincent Driessen <vincent@3rdcloud.com>
" Version:      Vim 7 (may work with lower Vim versions, but not tested)
" URL:          http://github.com/nvie/vim-flake8

let s:save_cpo = &cpo
set cpo&vim

"" ** external ** {{{

function! flake8#Flake8()
    call s:Flake8()
    call s:Warnings()
endfunction

function! flake8#Flake8UnplaceMarkers()
    call s:UnplaceMarkers()
    call s:Warnings()
endfunction

function! flake8#Flake8ShowError()
    call s:ShowErrorMessage()
endfunction

"" }}}

"" ** internal ** {{{

"" warnings 

let s:displayed_warnings = 0
function s:Warnings()
  if !s:displayed_warnings
    let l:show_website_url = 0

    let l:msg = "has been deprecated in favour of flake8 config files"
    for setting_name in ['g:flake8_ignore', 'g:flake8_builtins', 'g:flake8_max_line_length', 'g:flake8_max_complexity']
      if exists(setting_name)
        echohl WarningMsg | echom setting_name l:msg | echohl None
        let l:show_website_url = 1
      endif
    endfor

    if l:show_website_url
      let l:url = "http://flake8.readthedocs.org/en/latest/config.html"
      echohl WarningMsg | echom l:url | echohl None
    endif
    let s:displayed_warnings = 1
  endif
endfunction

"" config

function! s:DeclareOption(name, globalPrefix, default)  " {{{
    if !exists('g:'.a:name)
        if a:default != ''
            execute 'let s:'.a:name.'='.a:default
        else
            execute 'let s:'.a:name.'=""'
        endif
    else
        execute 'let l:global="g:".a:name'
        if l:global != ''
            execute 'let s:'.a:name.'="'.a:globalPrefix.'".g:'.a:name
        else
            execute 'let s:'.a:name.'=""'
        endif
    endif
endfunction  " }}}

function! s:Setup()  " {{{
    "" read options

    " flake8 command
    call s:DeclareOption('flake8_cmd', '', '"flake8"')
    " quickfix
    call s:DeclareOption('flake8_quickfix_location', '', '"belowright"')
    call s:DeclareOption('flake8_quickfix_height', '', 5)
    call s:DeclareOption('flake8_show_quickfix', '', 1)
    " markers to show
    call s:DeclareOption('flake8_show_in_gutter', '',   0)
    call s:DeclareOption('flake8_show_in_file', '',   0)
    call s:DeclareOption('flake8_max_markers', '', 500)
    " marker signs
    call s:DeclareOption('flake8_error_marker', '', '"E>"')
    call s:DeclareOption('flake8_warning_marker', '', '"W>"')
    call s:DeclareOption('flake8_pyflake_marker', '', '"F>"')
    call s:DeclareOption('flake8_complexity_marker', '', '"C>"')
    call s:DeclareOption('flake8_naming_marker', '', '"N>"')

    "" setup markerdata

    if !exists('s:markerdata')
        let s:markerdata = {}
        let s:markerdata['E'] = {'name': 'Flake8_Error'}
        let s:markerdata['W'] = {'name': 'Flake8_Warning'}
        let s:markerdata['F'] = {'name': 'Flake8_PyFlake'}
        let s:markerdata['C'] = {'name': 'Flake8_Complexity'}
        let s:markerdata['N'] = {'name': 'Flake8_Nameing'}
    endif
    let s:markerdata['E'].marker = s:flake8_error_marker
    let s:markerdata['W'].marker = s:flake8_warning_marker
    let s:markerdata['F'].marker = s:flake8_pyflake_marker
    let s:markerdata['C'].marker = s:flake8_complexity_marker
    let s:markerdata['N'].marker = s:flake8_naming_marker

endfunction  " }}}

"" do flake8

function! s:Flake8()  " {{{
    " read config
    call s:Setup()

    let l:executable = split(s:flake8_cmd)[0]

    if !executable(l:executable)
        echoerr "File " . l:executable . " not found. Please install it first."
        return
    endif

    " clear old
    call s:UnplaceMarkers()
    let s:matchids = []
    let s:signids  = []

    " store old grep settings (to restore later)
    let l:old_gfm=&grepformat
    let l:old_gp=&grepprg
    let l:old_shellpipe=&shellpipe
    let l:old_t_ti=&t_ti
    let l:old_t_te=&t_te

    " write any changes before continuing
    if &readonly == 0
        update
    endif

    set lazyredraw   " delay redrawing

    " prevent terminal from blinking
    set shellpipe=>
    set t_ti=
    set t_te=

    " perform the grep itself
    let &grepformat="%f:%l:%c: %m\,%f:%l: %m"
    let &grepprg=s:flake8_cmd
    silent! grep! "%"
    " close any existing cwindows,
    " placed after 'grep' in case quickfix is open on autocmd QuickFixCmdPost
    cclose

    " restore grep settings
    let &grepformat=l:old_gfm
    let &grepprg=l:old_gp
    let &shellpipe=l:old_shellpipe
    let &t_ti=l:old_t_ti
    let &t_te=l:old_t_te
    " store mapping of line number to error string

    " process results
    let s:resultDict = {} 

    let l:results=getqflist()
    let l:has_results=results != []
    if l:has_results
	" save line number of each error message	
        for result in l:results
	    let linenum = result.lnum
            let s:resultDict[linenum] = result.text
	endfor

        " markers
        if !s:flake8_show_in_gutter == 0 || !s:flake8_show_in_file == 0
            call s:PlaceMarkers(l:results)
        endif
        " quickfix
        if !s:flake8_show_quickfix == 0
            " open cwindow
            execute s:flake8_quickfix_location." copen".s:flake8_quickfix_height
            setlocal wrap
            nnoremap <buffer> <silent> c :cclose<CR>
            nnoremap <buffer> <silent> q :cclose<CR>
        endif
    endif

    set nolazyredraw
    redraw!

    " Show status
    if l:has_results == 0
        echon "Flake8 check OK"
    else
        echon "Flake8 found issues"
    endif
endfunction  " }}}



"" markers
function! s:PlaceMarkers(results)  " {{{
    " in gutter?
    if !s:flake8_show_in_gutter == 0
        " define signs
        for val in values(s:markerdata)
            if val.marker != ''
                execute "sign define ".val.name." text=".val.marker." texthl=".val.name
            endif
        endfor
    endif

    " place
    let l:index0 = 100
    let l:index  = l:index0
    for result in a:results
        if l:index >= (s:flake8_max_markers+l:index0)
            break
        endif
        let l:type = strpart(result.text, 0, 1)
        if has_key(s:markerdata, l:type) && s:markerdata[l:type].marker != ''
            " file markers
            if !s:flake8_show_in_file == 0
                if !has_key(s:markerdata[l:type], 'matchstr')
                    let s:markerdata[l:type].matchstr = '\%('
                else
                    let s:markerdata[l:type].matchstr .= '\|'
                endif
                let s:markerdata[l:type].matchstr .= '\%'.result.lnum.'l\%'.result.col.'c'
            endif
            " gutter markers
            if !s:flake8_show_in_gutter == 0
                execute ":sign place ".index." name=".s:markerdata[l:type].name
                            \ . " line=".result.lnum." file=".expand("%:p")
                let s:signids += [l:index]
            endif
            let l:index += 1
        endif
    endfor

    " in file?
    if !s:flake8_show_in_file == 0
        for l:val in values(s:markerdata)
            if l:val.marker != '' && has_key(l:val, 'matchstr')
                let l:val.matchid = matchadd(l:val.name, l:val.matchstr.'\)')
            endif
        endfor
    endif
endfunction  " }}}

function! s:UnplaceMarkers()  " {{{
    " gutter markers
    if exists('s:signids')
        for i in s:signids
            execute ":sign unplace ".i
        endfor
        unlet s:signids
    endif
    " file markers
    for l:val in values(s:markerdata)
        if has_key(l:val, 'matchid')
            call matchdelete(l:val.matchid)
            unlet l:val.matchid
            unlet l:val.matchstr
        endif
    endfor
endfunction  " }}}

function! s:ShowErrorMessage()  " {{{
    let l:cursorPos = getpos(".")
    if !exists('s:resultDict')
	return
    endif

    " if there is a message on the current line,
    " then echo it 
    if has_key(s:resultDict, l:cursorPos[1])
	let l:errorText = get(s:resultDict, l:cursorPos[1]) 
	echo strpart(l:errorText, 0, &columns-1)
	let b:showing_message = 1
    endif

    " if a message is already being shown,
    " then clear it
    if !b:showing_message == 0
	echo
	let b:showing_message = 0
    endif

endfunction  " }}}

"" }}}

let &cpo = s:save_cpo
unlet s:save_cpo

