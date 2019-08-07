scriptencoding utf-8
" EasyMotion - Vim motions on speed!
"
" Author: Kim Silkebækken <kim.silkebaekken+vim@gmail.com>
"         haya14busa <hayabusa1419@gmail.com>
" Source: https://github.com/easymotion/vim-easymotion
"=============================================================================
" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

let s:TRUE = !0
let s:FALSE = 0
let s:DIRECTION = { 'forward': 0, 'backward': 1, 'bidirection': 2}


" Init: {{{
let s:loaded = s:FALSE
function! EasyMotion#init()
    if s:loaded
        return
    endif
    let s:loaded = s:TRUE
    call EasyMotion#highlight#load()
    " Store previous motion info
    let s:previous = {}
    " Store previous operator-pending motion info for '.' repeat
    let s:dot_repeat = {}
    " Prepare 1-key Migemo Dictionary
    let s:migemo_dicts = {}
    let s:EasyMotion_is_active = 0
    call EasyMotion#reset()
    " Anywhere regular expression: {{{
    let re = '\v' .
        \    '(<.|^$)' . '|' .
        \    '(.>|^$)' . '|' .
        \    '(\l)\zs(\u)' . '|' .
        \    '(_\zs.)' . '|' .
        \    '(#\zs.)'
    " 1. word
    " 2. end of word
    " 3. CamelCase
    " 4. after '_' hoge_foo
    " 5. after '#' hoge#foo
    let g:EasyMotion_re_anywhere = get(g:, 'EasyMotion_re_anywhere', re)

    " Anywhere regular expression within line:
    let re = '\v' .
        \    '(<.|^$)' . '|' .
        \    '(.>|^$)' . '|' .
        \    '(\l)\zs(\u)' . '|' .
        \    '(_\zs.)' . '|' .
        \    '(#\zs.)'
    let g:EasyMotion_re_line_anywhere = get(g:, 'EasyMotion_re_line_anywhere', re)
    "}}}
    " For other plugin
    let s:EasyMotion_is_cancelled = 0
    " 0 -> Success
    " 1 -> Cancel
    let g:EasyMotion_ignore_exception = 0
    return ""
endfunction
"}}}
" Reset: {{{
function! EasyMotion#reset()
    let s:flag = {
        \ 'within_line' : 0,
        \ 'dot_repeat' : 0,
        \ 'regexp' : 0,
        \ 'bd_t' : 0,
        \ 'find_bd' : 0,
        \ 'linewise' : 0,
        \ 'count_dot_repeat' : 0,
        \ }
        " regexp: -> regular expression
        "   This value is used when multi input find motion. If this values is
        "   1, input text is treated as regexp.(Default: escaped)
        " bd_t: -> bi-directional 't' motion
        "   This value is used to re-define regexp only for bi-directional 't'
        "   motion
        " find_bd: -> bi-directional find motion
        "   This value is used to recheck the motion is inclusive or exclusive
        "   because 'f' & 't' forward find motion is inclusive, but 'F' & 'T'
        "   backward find motion is exclusive
        " count_dot_repeat: -> dot repeat with count
        "   https://github.com/easymotion/vim-easymotion/issues/164
    let s:current = {
        \ 'is_operator' : 0,
        \ 'is_search' : 0,
        \ 'dot_repeat_target_cnt' : 0,
        \ 'dot_prompt_user_cnt' : 0,
        \ 'changedtick' : 0,
        \ }
        " \ 'start_position' : [],
        " \ 'cursor_position' : [],

        " is_operator:
        "   Store is_operator value first because mode(1) value will be
        "   changed by some operation.
        " dot_* :
        "   These values are used when '.' repeat for automatically
        "   select marker/label characters.(Using count avoid recursive
        "   prompt)
        " changedtick:
        "   :h b:changedtick
        "   This value is used to avoid side effect of overwriting buffer text
        "   which will change b:changedtick value. To overwrite g:repeat_tick
        "   value(defined tpope/vim-repeat), I can avoid this side effect of
        "   conflicting with tpope/vim-repeat
        " start_position:
        "   Original, start cursor position.
        " cursor_position:
        "   Usually, this values is same with start_position, but in
        "   visualmode and 'n' key motion, this value could be different.
    return ""
endfunction "}}}

" Motion Functions: {{{
" -- Find Motion -------------------------
" Note: {{{
" num_strokes:
"   The number of input characters. Currently provide 1, 2, or -1.
"   '-1' means no limit.
" visualmode:
"   Vim script couldn't detect the function is called in visual mode by
"   mode(1), so tell whether it is in visual mode by argument explicitly
" direction:
"   0 -> forward
"   1 -> backward
"   2 -> bi-direction (handle forward & backward at the same time) }}}
function! EasyMotion#S(num_strokes, visualmode, direction) " {{{
    if a:direction == 1
        let is_inclusive = 0
    else
        " Note: Handle bi-direction later because 'f' motion is inclusive but
        " 'F' motion is exclusive
        let is_inclusive = mode(1) ==# 'no' ? 1 : 0
    endif
    let s:flag.find_bd = a:direction == 2 ? 1 : 0
    let re = s:findMotion(a:num_strokes, a:direction)
    if s:handleEmpty(re, a:visualmode) | return | endif
    call s:EasyMotion(re, a:direction, a:visualmode ? visualmode() : '', is_inclusive)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#OverwinF(num_strokes) " {{{
    let re = s:findMotion(a:num_strokes, s:DIRECTION.bidirection)
    call EasyMotion#reset()
    if re isnot# ''
        return EasyMotion#overwin#move(re)
    endif
endfunction "}}}
function! EasyMotion#T(num_strokes, visualmode, direction) " {{{
    if a:direction == 1
        let is_inclusive = 0
    else
        " Note: Handle bi-direction later because 't' motion is inclusive but
        " 'T' motion is exclusive
        let is_inclusive = mode(1) ==# 'no' ? 1 : 0
    endif
    let s:flag.find_bd = a:direction == 2 ? 1 : 0
    let re = s:findMotion(a:num_strokes, a:direction)
    if s:handleEmpty(re, a:visualmode) | return | endif
    if a:direction == 2
        let s:flag.bd_t = 1
    elseif a:direction == 1
        let re = s:convert_t_regexp(re, 1) " backward
    else
        let re = s:convert_t_regexp(re, 0) " forward
    endif
    call s:EasyMotion(re, a:direction, a:visualmode ? visualmode() : '', is_inclusive)
    return s:EasyMotion_is_cancelled
endfunction " }}}
" -- Word Motion -------------------------
function! EasyMotion#WB(visualmode, direction) " {{{
    "FIXME: inconsistent with default vim motion
    "FIXED: -> EasyMotion#WBK()
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    call s:EasyMotion('\(\<.\|^$\)', a:direction, a:visualmode ? visualmode() : '', 0)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#WBW(visualmode, direction) " {{{
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let regex_without_file_ends = '\v(^|\s)\zs\S|^$'
    let regex = l:regex_without_file_ends
                \ . (a:direction == 1 ? '' : '|%$')
                \ . (a:direction == 0 ? '' : '|%^')
    call s:EasyMotion(l:regex, a:direction, a:visualmode ? visualmode() : '', 0)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#WBK(visualmode, direction) " {{{
    " vim's iskeyword style word motion
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let regex_without_file_ends = '\v<|^\S|\s\zs\S|>\zs\S|^$'
    let regex = l:regex_without_file_ends
                \ . (a:direction == 1 ? '' : '|%$')
                \ . (a:direction == 0 ? '' : '|%^')
    call s:EasyMotion(l:regex, a:direction, a:visualmode ? visualmode() : '', 0)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#E(visualmode, direction) " {{{
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let is_inclusive = mode(1) ==# 'no' ? 1 : 0
    call s:EasyMotion('\(.\>\|^$\)', a:direction, a:visualmode ? visualmode() : '', is_inclusive)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#EW(visualmode, direction) " {{{
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let is_inclusive = mode(1) ==# 'no' ? 1 : 0
    " Note: The stopping positions for 'E' and 'gE' differs. Thus, the regex
    " for direction==2 cannot be the same in both directions. This will be
    " ignored.
    let regex_stub = '\v\S(\s|$)'
    let regex = l:regex_stub
                \ . (a:direction == 0 ? '' : '|^$|%^')
                \ . (a:direction == 1 ? '' : '|%$')
    call s:EasyMotion(l:regex, a:direction, a:visualmode ? visualmode() : '', 0)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#EK(visualmode, direction) " {{{
    " vim's iskeyword style word motion
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let is_inclusive = mode(1) ==# 'no' ? 1 : 0
    " Note: The stopping positions for 'e' and 'ge' differs. Thus, the regex
    " for direction==2 cannot be the same in both directions. This will be
    " ignored.
    let regex_stub = '\v.\ze>|\S\ze\s*$|\S\ze\s|\k\zs>\S\ze|\S<'
    let regex = l:regex_stub
                \ . (a:direction == 0 ? '' : '|^$|%^')
                \ . (a:direction == 1 ? '' : '|%$')
    call s:EasyMotion(l:regex, a:direction, a:visualmode ? visualmode() : '', 0)


    return s:EasyMotion_is_cancelled
endfunction " }}}
" -- JK Motion ---------------------------
function! EasyMotion#JK(visualmode, direction) " {{{
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let s:flag.linewise = 1

    if g:EasyMotion_startofline
        call s:EasyMotion('^\(\w\|\s*\zs\|$\)', a:direction, a:visualmode ? visualmode() : '', 0)
    else
        let vcol  = EasyMotion#helper#vcol('.')
        let pattern = printf('^.\{-}\zs\(\%%<%dv.\%%>%dv\|$\)', vcol + 1, vcol)
        call s:EasyMotion(pattern, a:direction, a:visualmode ? visualmode() : '', 0)
    endif
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#Sol(visualmode, direction) " {{{
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let s:flag.linewise = 1
    call s:EasyMotion('^\(.\|$\)', a:direction, a:visualmode ? visualmode() : '', '')
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#Eol(visualmode, direction) " {{{
    let s:flag.linewise = 1
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    call s:EasyMotion('\(\w\|\s*\zs\|.\|^\)$', a:direction, a:visualmode ? visualmode() : '', '')
    return s:EasyMotion_is_cancelled
endfunction " }}}
" -- Search Motion -----------------------
function! EasyMotion#Search(visualmode, direction, respect_direction) " {{{
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let search_direction = a:respect_direction ?
    \   (a:direction == 1 ? v:searchforward : 1-v:searchforward) :
    \   (a:direction)
    call s:EasyMotion(@/, search_direction, a:visualmode ? visualmode() : '', 0)
    return s:EasyMotion_is_cancelled
endfunction " }}}
" -- JumpToAnywhere Motion ---------------
function! EasyMotion#JumpToAnywhere(visualmode, direction) " {{{
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    call s:EasyMotion( g:EasyMotion_re_anywhere, a:direction, a:visualmode ? visualmode() : '', 0)
    return s:EasyMotion_is_cancelled
endfunction " }}}
" -- Line Motion -------------------------
function! EasyMotion#SL(num_strokes, visualmode, direction) " {{{
    let s:flag.within_line = 1
    call EasyMotion#S(a:num_strokes, a:visualmode, a:direction)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#TL(num_strokes, visualmode, direction) " {{{
    let s:flag.within_line = 1
    call EasyMotion#T(a:num_strokes, a:visualmode, a:direction)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#WBL(visualmode, direction) " {{{
    let s:flag.within_line = 1
    call EasyMotion#WBK(a:visualmode, a:direction)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#EL(visualmode, direction) " {{{
    let s:flag.within_line = 1
    call EasyMotion#EK(a:visualmode, a:direction)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#LineAnywhere(visualmode, direction) " {{{
    let s:flag.within_line = 1
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let re = g:EasyMotion_re_line_anywhere
    call s:EasyMotion(re, a:direction, a:visualmode ? visualmode() : '', 0)
    return s:EasyMotion_is_cancelled
endfunction " }}}
" -- User Motion -------------------------
let s:config = {
\   'pattern': '',
\   'visualmode': s:FALSE,
\   'direction': s:DIRECTION.forward,
\   'inclusive': s:FALSE,
\   'accept_cursor_pos': s:FALSE,
\   'overwin': s:FALSE
\ }

function! s:default_config() abort
    let c = copy(s:config)
    let m = mode(1)
    let c.inclusive = m ==# 'no' ? s:TRUE : s:FALSE
    return c
endfunction

function! EasyMotion#go(...) abort
    let c = extend(s:default_config(), get(a:, 1, {}))
    if c.overwin
        return EasyMotion#overwin#move(c.pattern)
    else
        let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
        call s:EasyMotion(c.pattern, c.direction, c.visualmode ? visualmode() : '', c.inclusive, c)
        return s:EasyMotion_is_cancelled
    endif
endfunction
function! EasyMotion#User(pattern, visualmode, direction, inclusive, ...) " {{{
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    let is_inclusive = mode(1) ==# 'no' ? a:inclusive : 0
    let re = a:pattern
    call s:EasyMotion(re, a:direction, a:visualmode ? visualmode() : '', is_inclusive, get(a:, 1, {}))
    return s:EasyMotion_is_cancelled
endfunction " }}}
" -- Repeat Motion -----------------------
function! EasyMotion#Repeat(visualmode) " {{{
    " Repeat previous motion with previous targets
    if !has_key(s:previous, 'regexp')
        call s:Message("Previous targets doesn't exist")
        let s:EasyMotion_is_cancelled = 1
        return s:EasyMotion_is_cancelled
    endif
    let re = s:previous.regexp
    let direction = s:previous.direction
    let s:flag.within_line = s:previous.line_flag
    let s:flag.bd_t = s:previous.bd_t_flag
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    " FIXME: is_inclusive value is inappropriate but handling this value is
    " difficult and priorities is low because this motion maybe used usually
    " as a 'normal' motion.
    let is_inclusive = mode(1) ==# 'no' ? 1 : 0

    call s:EasyMotion(re, direction, a:visualmode ? visualmode() : '', is_inclusive)
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#DotRepeat() " {{{
    let cnt = v:count1 " avoid overwriting

    " Repeat previous '.' motion with previous targets and operator
    if !has_key(s:dot_repeat, 'regexp')
        call s:Message("Previous motion doesn't exist")
        let s:EasyMotion_is_cancelled = 1
        return s:EasyMotion_is_cancelled
    endif

    let re = s:dot_repeat.regexp
    let direction = s:dot_repeat.direction
    let is_inclusive = s:dot_repeat.is_inclusive

    for i in range(cnt)
        " s:EasyMotion() always call reset s:flag & s:current
        let s:flag.dot_repeat = 1
        let s:flag.within_line = s:dot_repeat.line_flag
        let s:flag.bd_t = s:dot_repeat.bd_t_flag
        let s:current.is_operator = 1

        let s:flag.count_dot_repeat = (i > 0 ? 1 : 0)
        silent call s:EasyMotion(re, direction, 0, is_inclusive)
    endfor
    return s:EasyMotion_is_cancelled
endfunction " }}}
function! EasyMotion#NextPrevious(visualmode, direction) " {{{
    " Move next/previous destination using previous motion regexp
    let cnt = v:count1 " avoid overwriting
    if !has_key(s:previous, 'regexp')
        call s:Message("Previous targets doesn't exist")
        let s:EasyMotion_is_cancelled = 1
        return s:EasyMotion_is_cancelled
    endif
    let re = s:previous.regexp
    let search_direction = (a:direction == 1 ? 'b' : '')

    if g:EasyMotion_move_highlight
        call EasyMotion#highlight#attach_autocmd()
        call EasyMotion#highlight#add_highlight(re, g:EasyMotion_hl_move)
    endif

    if ! empty(a:visualmode)
        " FIXME: blink highlight
        silent exec 'normal! gv'
    endif

    " Mark jump-list
    if cnt > 1
        " Consider Next/Previous motions as jump motion :h jump-motion
        " Note: It should add jumplist even if the count isn't given
        "       considering vim's default behavior of `n` & `N`, but just
        "       I don't want to do it without the count. Should I add a
        "       option?
        normal! m`
    endif

    " Jump
    " @vimlint(EVL102, 1, l:_)
    for _ in range(cnt)
        keepjumps call searchpos(re, search_direction)
    endfor

    normal! zv

    call EasyMotion#reset()
    " -- Activate EasyMotion ----------------- {{{
    let s:EasyMotion_is_active = 1
    call EasyMotion#attach_active_autocmd() "}}}
    return s:EasyMotion_is_cancelled
endfunction " }}}
" }}}
" Helper Functions: {{{
" -- Message -----------------------------
function! s:Message(message) " {{{
    if g:EasyMotion_verbose
        echo 'EasyMotion: ' . a:message
    else
        " Make the current message disappear
        echo ''
        " redraw
    endif
endfunction " }}}
function! s:Prompt(message) " {{{
    echohl Question
    echo a:message . ': '
    echohl None
endfunction " }}}
function! s:Throw(message) "{{{
    throw 'EasyMotion: ' . a:message
endfunction "}}}

" -- Save & Restore values ---------------
function! s:SaveValue() "{{{
    if ! s:current.is_search
        call EasyMotion#helper#VarReset('&scrolloff', 0)
    endif
    call EasyMotion#helper#VarReset('&modified', 0)
    call EasyMotion#helper#VarReset('&modifiable', 1)
    call EasyMotion#helper#VarReset('&readonly', 0)
    call EasyMotion#helper#VarReset('&spell', 0)
    call EasyMotion#helper#VarReset('&virtualedit', '')
    " if &foldmethod !=# 'expr'
        call EasyMotion#helper#VarReset('&foldmethod', 'manual')
    " endif
endfunction "}}}
function! s:RestoreValue() "{{{
    call EasyMotion#helper#VarReset('&scrolloff')
    call EasyMotion#helper#VarReset('&modified')
    call EasyMotion#helper#VarReset('&modifiable')
    call EasyMotion#helper#VarReset('&readonly')
    call EasyMotion#helper#VarReset('&spell')
    call EasyMotion#helper#VarReset('&virtualedit')
    " if &foldmethod !=# 'expr'
        call EasyMotion#helper#VarReset('&foldmethod')
    " endif
endfunction "}}}
function! s:turn_off_hl_error() "{{{
    let s:error_hl = EasyMotion#highlight#capture('Error')
    call EasyMotion#highlight#turn_off(s:error_hl)
    let s:matchparen_hl = EasyMotion#highlight#capture('MatchParen')
    call EasyMotion#highlight#turn_off(s:matchparen_hl)
endfunction "}}}
function! s:turn_on_hl_error() "{{{
    if exists('s:error_hl')
        call EasyMotion#highlight#turn_on(s:error_hl)
        unlet s:error_hl
    endif

    if exists('s:matchparen_hl')
        call EasyMotion#highlight#turn_on(s:matchparen_hl)
        unlet s:matchparen_hl
    endif
endfunction "}}}

" -- Draw --------------------------------
function! s:SetLines(lines, key) " {{{
    for [line_num, line] in a:lines
        keepjumps call setline(line_num, line[a:key])
    endfor
endfunction " }}}

" -- Get characters from user input ------
function! s:GetChar(...) abort "{{{
    let mode = get(a:, 1, 0)
    while 1
        " Workaround for https://github.com/osyo-manga/vital-over/issues/53
        try
            let char = call('getchar', a:000)
        catch /^Vim:Interrupt$/
            let char = 3 " <C-c>
        endtry
        if char == 27 || char == 3
            " Escape or <C-c> key pressed
            redraw
            call s:Message('Cancelled')
            return ''
        endif
        " Workaround for the <expr> mappings
        if string(char) !=# "\x80\xfd`"
            return mode == 1 ? !!char
            \    : type(char) == type(0) ? nr2char(char) : char
        endif
    endwhile
endfunction "}}}

" -- Find Motion Helper ------------------
function! s:findMotion(num_strokes, direction) "{{{
    " Find Motion: S,F,T
    let s:current.is_operator = mode(1) ==# 'no' ? 1: 0
    " store cursor pos because 'n' key find motion could be jump to offscreen
    let s:current.original_position = [line('.'), col('.')]
    let s:current.is_search = a:num_strokes == -1 ? 1: 0
    let s:flag.regexp = a:num_strokes == -1 ? 1 : 0 " TODO: remove?

    if g:EasyMotion_add_search_history && a:num_strokes == -1
        let s:previous['input'] = @/
    else
        let s:previous['input'] = get(s:previous, 'input', '')
    endif
    let input = EasyMotion#command_line#GetInput(
                    \ a:num_strokes, s:previous.input, a:direction)
    let s:previous['input'] = input

    " Check that we have an input char
    if empty(input)
        return ''
    endif

    let re = s:convertRegep(input)

    if g:EasyMotion_add_search_history && a:num_strokes == -1
        let history_re = substitute(re, '\\c\|\\C', '', '')
        let @/ = history_re "For textobject: 'gn'
        call histadd('search', history_re)
    endif

    return re
endfunction "}}}
function! s:convertRegep(input) "{{{
    " 1. regexp
    " 2. migemo
    " 3. smartsign
    " 4. smartcase
    let use_migemo = s:should_use_migemo(a:input)
    let re = use_migemo || s:should_use_regexp() ? a:input : s:escape_regexp_char(a:input)

    " Convert space to match only start of spaces
    if re ==# ' '
        let re = '\s\+'
    endif

    if use_migemo
        let re = s:convertMigemo(re)
    endif

    if s:should_use_smartsign(a:input)
        let r = s:convertSmartsign(a:input)
        if use_migemo
            let re = re . '\m\|' . r
        else
            let re = r
        endif
    endif

    let case_flag = EasyMotion#helper#should_case_sensitive(
                        \ a:input, s:current.is_search) ? '\c' : '\C'
    let re = case_flag . re
    return re
endfunction "}}}
function! s:convertMigemo(re) "{{{
    let re = a:re

    if len(re) > 1
        " System cmigemo
        return EasyMotion#cmigemo#getMigemoPattern(re)
    endif

    " EasyMotion migemo one key dict
    if ! has_key(s:migemo_dicts, &l:encoding)
        let s:migemo_dicts[&l:encoding] = EasyMotion#helper#load_migemo_dict()
    endif
    return get(s:migemo_dicts[&l:encoding], re, a:re)
endfunction "}}}
function! s:convertSmartsign(chars) "{{{
    " Convert given chars to smartsign string
    " Example: 12 -> [1!][2@]
    "          a] -> a[]}]

    " Load smartsign dictionary
    let smart_dict = s:load_smart_dict()
    " Prepare converted string
    let converted_str = ''
    " Get `upper_sign` for each given chars
    " Split chars into list
    for char in split(a:chars, '\zs')
        let upper_sign = s:get_escaped_group_char(smart_dict, char)
        if upper_sign ==# ''
            let converted_str .= s:escape_regexp_char(char)
        else
            " [1!]
            let converted_str .= '[' . char . upper_sign . ']'
        endif
    endfor
    return converted_str
endfunction "}}}
function! s:get_escaped_group_char(dict, char) "{{{
    " Get escaped char from given dictionary
    " return '' if char is not find
    " Used inside `[]`
    return escape(get(a:dict, a:char, ''), '^')
endfunction "}}}
function! s:escape_regexp_char(char) "{{{
    return escape(a:char, '.$^~\[]*')
endfunction "}}}
function! s:convertSmartcase(re, char) "{{{
    let re = a:re
    if a:char =~# '\U' "nonuppercase
        return '\c' . re
    else "uppercase
        return '\C' . re
    endif
endfunction "}}}
function! s:should_use_regexp() "{{{
    return g:EasyMotion_use_regexp == 1 && s:flag.regexp == 1
endfunction "}}}
function! s:should_use_migemo(char) "{{{
    if ! g:EasyMotion_use_migemo || match(a:char, '[^!-~]') != -1
        return 0
    endif

    " TODO: use direction to improve
    if s:flag.within_line == 1
        let first_line = line('.')
        let end_line = line('.')
    else
        let first_line = line('w0')
        let end_line = line('w$')
    endif

    " Skip folded line and check if text include multibyte characters
    for line in range(first_line, end_line)
        if EasyMotion#helper#is_folded(line)
            continue
        endif

        if EasyMotion#helper#include_multibyte_char(getline(line)) == 1
            return 1
        endif
    endfor

    return 0
endfunction "}}}
function! s:should_use_smartsign(char) "{{{
    " Smartsign Dictionary exists?
    " \A: non-alphabetic character
    " Do not use smartsign for n-key find search motions
    if (exists('g:EasyMotion_use_smartsign_us')  ||
    \   exists('g:EasyMotion_use_smartsign_jp')) &&
    \  match(a:char, '\A') != -1 &&
    \ exists('s:current.is_search') && s:current.is_search == 0
        return 1
    else
        return 0
    endif
endfunction "}}}
function! s:convert_t_regexp(re, direction) "{{{
    if a:direction == 0 "forward
        return '\_.\ze\('.a:re.'\)'
    elseif a:direction == 1 "backward
        return '\('.a:re.'\)\@<=\_.'
    endif
endfunction "}}}

" -- Handle Visual Mode ------------------
function! s:GetVisualStartPosition(c_pos, v_start, v_end, search_direction) "{{{
    let vmode = mode(1)
    if vmode !~# "^[Vv\<C-v>]"
        call s:Throw('Unkown visual mode:'.vmode)
    endif

    if vmode ==# 'V' "line-wise Visual
        " Line-wise Visual {{{
        if a:v_start[0] == a:v_end[0]
            if a:search_direction == ''
                return a:v_start
            elseif a:search_direction == 'b'
                return a:v_end
            else
                call s:throw('Unkown search_direction')
            endif
        else
            if a:c_pos[0] == a:v_start[0]
                return a:v_end
            elseif a:c_pos[0] == a:v_end[0]
                return a:v_start
            endif
        endif
        "}}}
    else
        " Character-wise or Block-wise Visual"{{{
        if a:c_pos == a:v_start
            return a:v_end
        elseif a:c_pos == a:v_end
            return a:v_start
        endif

        " virtualedit
        if a:c_pos[0] == a:v_start[0]
            return a:v_end
        elseif a:c_pos[0] == a:v_end[0]
            return a:v_start
        elseif EasyMotion#helper#is_greater_coords(a:c_pos, a:v_start) == 1
            return a:v_end
        else
            return a:v_start
        endif
        "}}}
    endif
endfunction "}}}

" -- Others ------------------------------
function! s:handleEmpty(input, visualmode) "{{{
    " if empty, reselect and return 1
    if empty(a:input)
        if ! empty(a:visualmode)
            silent exec 'normal! gv'
        endif
        let s:EasyMotion_is_cancelled = 1 " Cancel
        return 1
    endif
    return 0
endfunction "}}}
function! s:load_smart_dict() "{{{
    if exists('g:EasyMotion_use_smartsign_us')
        return g:EasyMotion#sticky_table#us
    elseif exists('g:EasyMotion_use_smartsign_jp')
        return g:EasyMotion#sticky_table#jp
    else
        return {}
    endif
endfunction "}}}
function! EasyMotion#attach_active_autocmd() "{{{
    " Reference: https://github.com/justinmk/vim-sneak
    augroup plugin-easymotion-active
        autocmd!
        autocmd InsertEnter,WinLeave,BufLeave <buffer>
            \ let s:EasyMotion_is_active = 0
            \  | autocmd! plugin-easymotion-active * <buffer>
        autocmd CursorMoved <buffer>
            \ autocmd plugin-easymotion-active CursorMoved <buffer>
            \ let s:EasyMotion_is_active = 0
            \  | autocmd! plugin-easymotion-active * <buffer>
    augroup END
endfunction "}}}
function! EasyMotion#is_active() "{{{
    return s:EasyMotion_is_active
endfunction "}}}
function! EasyMotion#activate(is_visual) "{{{
    let s:EasyMotion_is_active = 1
    call EasyMotion#attach_active_autocmd()
    call EasyMotion#highlight#add_highlight(s:previous.regexp,
                                          \ g:EasyMotion_hl_move)
    call EasyMotion#highlight#attach_autocmd()
    if a:is_visual == 1
        normal! gv
    endif
endfunction "}}}
function! s:restore_cursor_state(visualmode) "{{{
    " -- Restore original cursor position/selection
    if ! empty(a:visualmode)
        silent exec 'normal! gv'
        keepjumps call cursor(s:current.cursor_position)
    else
        keepjumps call cursor(s:current.original_position)
    endif
endfunction " }}}
" Grouping Algorithms: {{{
let s:grouping_algorithms = {
\   1: 'SCTree'
\ , 2: 'Original'
\ }
" -- Single-key/closest target priority tree {{{
" This algorithm tries to assign one-key jumps to all the targets closest to the cursor.
" It works recursively and will work correctly with as few keys as two.
function! s:GroupingAlgorithmSCTree(targets, keys) "{{{
    " Prepare variables for working
    let targets_len = len(a:targets)
    let keys_len = len(a:keys)

    let groups = {}

    let keys = reverse(copy(a:keys))

    " Semi-recursively count targets {{{
        " We need to know exactly how many child nodes (targets) this branch will have
        " in order to pass the correct amount of targets to the recursive function.

        " Prepare sorted target count list {{{
            " This is horrible, I know. But dicts aren't sorted in vim, so we need to
            " work around that. That is done by having one sorted list with key counts,
            " and a dict which connects the key with the keys_count list.

            let keys_count = []
            let keys_count_keys = {}

            let i = 0
            for key in keys
                call add(keys_count, 0)

                let keys_count_keys[key] = i

                let i += 1
            endfor
        " }}}

        let targets_left = targets_len
        let level = 0
        let i = 0

        while targets_left > 0
            " Calculate the amount of child nodes based on the current level
            let childs_len = (level == 0 ? 1 : (keys_len - 1) )

            for key in keys
                " Add child node count to the keys_count array
                let keys_count[keys_count_keys[key]] += childs_len

                " Subtract the child node count
                let targets_left -= childs_len

                if targets_left <= 0
                    " Subtract the targets left if we added too many too
                    " many child nodes to the key count
                    let keys_count[keys_count_keys[key]] += targets_left

                    break
                endif

                let i += 1
            endfor

            let level += 1
        endwhile
    " }}}
    " Create group tree {{{
        let i = 0
        let key = 0

        call reverse(keys_count)

        for key_count in keys_count
            if key_count > 1
                " We need to create a subgroup
                " Recurse one level deeper
                let groups[a:keys[key]] = s:GroupingAlgorithmSCTree(a:targets[i : i + key_count - 1], a:keys)
            elseif key_count == 1
                " Assign single target key
                let groups[a:keys[key]] = a:targets[i]
            else
                " No target
                continue
            endif

            let key += 1
            let i += key_count
        endfor
    " }}}

    " Finally!
    return groups
endfunction "}}}
" }}}
" -- Original ---------------------------- {{{
function! s:GroupingAlgorithmOriginal(targets, keys)
    " Split targets into groups (1 level)
    let targets_len = len(a:targets)
    " let keys_len = len(a:keys)

    let groups = {}

    let i = 0
    let root_group = 0
    try
        while root_group < targets_len
            let groups[a:keys[root_group]] = {}

            for key in a:keys
                let groups[a:keys[root_group]][key] = a:targets[i]

                let i += 1
            endfor

            let root_group += 1
        endwhile
    catch | endtry

    " Flatten the group array
    if len(groups) == 1
        let groups = groups[a:keys[0]]
    endif

    return groups
endfunction
" }}}

" -- Coord/key dictionary creation ------- {{{
function! s:CreateCoordKeyDict(groups, ...)
    " Dict structure:
    " 1,2 : a
    " 2,3 : b
    let sort_list = []
    let coord_keys = {}
    let group_key = a:0 == 1 ? a:1 : ''

    for [key, item] in items(a:groups)
        let key = group_key . key
        "let key = ( ! empty(group_key) ? group_key : key)

        if type(item) == type([]) " List
            " Destination coords

            " The key needs to be zero-padded in order to
            " sort correctly
            let dict_key = printf('%05d,%05d', item[0], item[1])
            let coord_keys[dict_key] = key

            " We need a sorting list to loop correctly in
            " PromptUser, dicts are unsorted
            call add(sort_list, dict_key)
        else
            " Item is a dict (has children)
            let coord_key_dict = s:CreateCoordKeyDict(item, key)

            " Make sure to extend both the sort list and the
            " coord key dict
            call extend(sort_list, coord_key_dict[0])
            call extend(coord_keys, coord_key_dict[1])
        endif

        unlet item
    endfor

    return [sort_list, coord_keys]
endfunction
" }}}
" }}}
"}}}
" Core Functions: {{{
function! s:PromptUser(groups) "{{{
    " Recursive
    let group_values = values(a:groups)

    " -- If only one possible match, jump directly to it {{{
    if len(group_values) == 1
        if mode(1) ==# 'no'
            " Consider jump to first match
            " NOTE: matchstr() handles multibyte characters.
            let s:dot_repeat['target'] = matchstr(g:EasyMotion_keys, '^.')
        endif
        redraw
        return group_values[0]
    endif
    " }}}

    " -- Prepare marker lines ---------------- {{{
    let lines = {}

    let coord_key_dict = s:CreateCoordKeyDict(a:groups)

    let prev_col_num = 0
    for dict_key in sort(coord_key_dict[0])
        " NOTE: {{{
        " let g:EasyMotion_keys = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        " Perform <Plug>(easymotion-w)
        "
        " lines[line_num]['orig']:
        "   Lorem ipsum dolor sit amet consectetur adipisicing
        "
        " {target_char}:
        "   {L}orem {i}psum {d}olor {s}it {a}met {c}onsectetur {a}dipisicing
        "
        " lines[line_num]['marker'], {marker_chars}:
        "   {A}orem {B}psum {C}olor {D}it {E}met {F}onsectetur {G}dipisicing
        "   2-key-combo: {marker_chars} could be 1 or 2 chars like {AB}
        "
        " }}}

        " Prepare original line and marker line {{{
        let [line_num, col_num] = split(dict_key, ',')

        let line_num = str2nr(line_num)
        let col_num = str2nr(col_num)
        if ! has_key(lines, line_num)
            let current_line = getline(line_num)
            let lines[line_num] = {
                \ 'orig': current_line,
                \ 'marker': current_line,
                \ 'mb_compensation': 0,
                \ }
            " mb_compensation -> multibyte compensation
            let prev_col_num = 0
        endif "}}}

        " Multibyte Compensation: {{{
        " Solve multibyte issues by matching the byte column
        " number instead of the visual column
        " Compensate for byte difference between marker
        " character and target character
        "
        " This has to be done in order to match the correct
        " column; \%c matches the byte column and not display
        " column.
        let col_num = max([prev_col_num + 1,
                        \  col_num - lines[line_num]['mb_compensation']])
        let prev_col_num = col_num
        "}}}

        " Prepare marker characters {{{
        let marker_chars = coord_key_dict[1][dict_key]
        let marker_chars_len = EasyMotion#helper#strchars(marker_chars)
        "}}}

        " Replace {target} with {marker} & Highlight {{{
        let col_add = 0 " Column add byte length
        " Disable two-key-combo feature?
        let marker_max_length = g:EasyMotion_disable_two_key_combo == 1
                                \ ? 1 : 2
        for i in range(min([marker_chars_len, marker_max_length]))
            let marker_char = split(marker_chars, '\zs')[i]
            " EOL {{{
            if strlen(lines[line_num]['marker']) < col_num + col_add
                " Append marker chars if target is EOL
                let lines[line_num]['marker'] .= ' '
            endif "}}}

            let target_col_regexp = '\%' . (col_num + col_add) . 'c.'
            let target_char = matchstr(lines[line_num]['marker'],
                                      \ target_col_regexp)
            let space_len = strdisplaywidth(target_char)
                        \ - strdisplaywidth(marker_char)
            " Substitute marker character
            let substitute_expr = marker_char . repeat(' ', space_len)

            let lines[line_num]['marker'] = substitute(
                \ lines[line_num]['marker'],
                \ target_col_regexp,
                \ escape(substitute_expr,'&'),
                \ '')

            " Highlight targets {{{
            let _hl_group =
            \   (marker_chars_len == 1) ? g:EasyMotion_hl_group_target
            \   : (i == 0) ? g:EasyMotion_hl2_first_group_target
            \   : g:EasyMotion_hl2_second_group_target

            if exists('*matchaddpos')
                call EasyMotion#highlight#add_pos_highlight(
                            \ line_num, col_num + col_add, _hl_group)
            else
                call EasyMotion#highlight#add_highlight(
                    \ '\%' . line_num . 'l' . target_col_regexp,
                    \ _hl_group)
            endif
            "}}}

            " Add marker/target length difference for multibyte compensation
            let lines[line_num]['mb_compensation'] +=
                \ strlen(target_char) - strlen(substitute_expr)
            " Shift column
            let col_add += strlen(marker_char)
        endfor
        "}}}
    endfor

    let lines_items = items(lines)
    " }}}

    " -- Put labels on targets & Get User Input & Restore all {{{
    " Save undo tree
    let undo_lock = EasyMotion#undo#save()
    try
        " Set lines with markers {{{
        call s:SetLines(lines_items, 'marker')
        redraw "}}}

        " Get target character {{{
        call s:Prompt('Target key')
        let char = s:GetChar()
        "}}}

        " Convert uppercase {{{
        if g:EasyMotion_use_upper == 1 && match(g:EasyMotion_keys, '\l') == -1
            let char = toupper(char)
        endif "}}}

        " Jump first target when Enter or Space key is pressed "{{{
        if (char ==# "\<CR>" && g:EasyMotion_enter_jump_first == 1) ||
        \  (char ==# "\<Space>" && g:EasyMotion_space_jump_first == 1)
            " NOTE: matchstr() is multibyte aware.
            let char = matchstr(g:EasyMotion_keys, '^.')
        endif "}}}

        " For dot repeat {{{
        if mode(1) ==# 'no'
            " Store previous target when operator pending mode
            if s:current.dot_prompt_user_cnt == 0
                " Store
                let s:dot_repeat['target'] = char
            else
                " Append target chars
                let s:dot_repeat['target'] .= char
            endif
        endif "}}}

    finally
        " Restore original lines
        call s:SetLines(lines_items, 'orig')

        " Un-highlight targets {{{
        call EasyMotion#highlight#delete_highlight(
            \ g:EasyMotion_hl_group_target,
            \ g:EasyMotion_hl2_first_group_target,
            \ g:EasyMotion_hl2_second_group_target,
            \ )
        " }}}

        " Restore undo tree
        call undo_lock.restore()

        redraw
    endtry "}}}

    " -- Check if we have an input char ------ {{{
    if empty(char)
        call s:Throw('Cancelled')
    endif
    " }}}
    " -- Check if the input char is valid ---- {{{
    if ! has_key(a:groups, char)
        call s:Throw('Invalid target')
    endif
    " }}}

    let target = a:groups[char]

    if type(target) == type([])
        " Return target coordinates
        return target
    else
        " Prompt for new target character
        let s:current.dot_prompt_user_cnt += 1
        return s:PromptUser(target)
    endif
endfunction "}}}
function! s:DotPromptUser(groups) "{{{
    " Get char from previous target
    let char = s:dot_repeat.target[s:current.dot_repeat_target_cnt]
    " For dot repeat target chars
    let s:current.dot_repeat_target_cnt += 1

    let target = a:groups[char]

    if type(target) == type([])
        " Return target coordinates
        return target
    else
        " Prompt for new target character
        return s:PromptUser(target)
    endif
endfunction "}}}

function! s:EasyMotion(regexp, direction, visualmode, is_inclusive, ...) " {{{
    let config = extend(s:default_config(), get(a:, 1, {}))
    " Store s:current original_position & cursor_position {{{
    " current cursor pos.
    let s:current.cursor_position = [line('.'), col('.')]
    " original start position.  This value could be changed later in visual
    " mode
    let s:current.original_position =
        \ get(s:current, 'original_position', s:current.cursor_position)
    "}}}

    let win_first_line = line('w0') " visible first line num
    let win_last_line  = line('w$') " visible last line num

    " Store the target positions list
    " e.g. targets = [ [line, col], [line2, col2], ...]
    let targets = []

    " Store info for Repeat motion {{{
    if s:flag.dot_repeat != 1
        " Store Regular Expression
        let s:previous['regexp'] = a:regexp
        let s:previous['direction'] = a:direction
        let s:previous['operator'] = v:operator

        " Note: 'is_inclusive' value could be changed later when
        " bi-directional find motion depend on 'true' direction the cursor
        " will move.
        let s:previous['is_inclusive'] = a:is_inclusive

        " For special motion flag
        let s:previous['line_flag'] = s:flag.within_line
        let s:previous['bd_t_flag'] = s:flag.bd_t " bi-directional t motion
    endif "}}}

    " To avoid side effect of overwriting buffer for tpope/repeat
    " store current b:changedtick. Use this value later
    let s:current.changedtick = b:changedtick

    try
        " -- Reset properties -------------------- {{{
        " Save original value and set new value
        call s:SaveValue()
        call s:turn_off_hl_error()
        " }}}
        " Setup searchpos args {{{
        let search_direction = (a:direction == 1 ? 'b' : '')
        let search_stopline = a:direction == 1 ? win_first_line : win_last_line

        if s:flag.within_line == 1
            let search_stopline = s:current.original_position[0]
        endif
        "}}}

        " Handle visual mode {{{
        if ! empty(a:visualmode)
            " Decide at where visual mode start {{{
            normal! gv
            let v_start = [line("'<"),col("'<")] " visual_start_position
            let v_end   = [line("'>"),col("'>")] " visual_end_position

            let v_original_pos = s:GetVisualStartPosition(
                \ s:current.cursor_position, v_start, v_end, search_direction)
            "}}}

            " Reselect visual text {{{
            keepjumps call cursor(v_original_pos)
            exec "normal! " . a:visualmode
            keepjumps call cursor(s:current.cursor_position)
            "}}}
            " Update s:current.original_position
            " overwrite original start position
            let s:current.original_position = v_original_pos
        endif "}}}

        " Handle bi-directional t motion {{{
        if s:flag.bd_t == 1
            let regexp = s:convert_t_regexp(a:regexp, 0) "forward
        else
            let regexp = a:regexp
        endif
        "}}}

        " Handle dot repeat with count
        if s:flag.count_dot_repeat
            let cursor_char = EasyMotion#helper#get_char_by_coord(s:current.cursor_position)
            if cursor_char =~# regexp
                call add(targets, s:current.cursor_position)
            endif
        endif

        " Construct match dict {{{
        " Note: searchpos() has side effect which jump cursor position.
        "       You can disable this side effect by add 'n' flags,
        "       but in this case, it's better to allows jump side effect
        "       to gathering matched targets coordinates.
        let pos = searchpos(regexp, search_direction . (config.accept_cursor_pos ? 'c' : ''), search_stopline)
        while 1
            " Reached end of search range
            if pos == [0, 0]
                break
            endif

            " Skip folded lines {{{
            if EasyMotion#helper#is_folded(pos[0])
                if search_direction ==# 'b'
                    " FIXME: Hmm... I should use filter()
                    " keepjumps call cursor(foldclosed(pos[0]), 0)
                else
                    keepjumps call cursor(foldclosedend(pos[0]+1), 0)
                endif
            else
                call add(targets, pos)
            endif
            "}}}
            let pos = searchpos(regexp, search_direction, search_stopline)
        endwhile
        "}}}

        " Handle bidirection "{{{
        " For bi-directional t motion {{{
        if s:flag.bd_t == 1
            let regexp = s:convert_t_regexp(a:regexp, 1) "backward
        endif
        "}}}
        " Reconstruct match dict
        if a:direction == 2
            " Backward

            " Jump back cursor_position
            keepjumps call cursor(s:current.cursor_position[0],
                                \ s:current.cursor_position[1])

            let targets2 = []
            if s:flag.within_line == 0
                let search_stopline = win_first_line
            else
                let search_stopline = s:current.cursor_position[0]
            endif
            while 1
                " TODO: refactoring
                let pos = searchpos(regexp, 'b', search_stopline)
                " Reached end of search range
                if pos == [0, 0]
                    break
                endif

                " Skip folded lines {{{
                if EasyMotion#helper#is_folded(pos[0])
                    " keepjumps call cursor(foldclosedend(pos[0]+1), 0)
                    continue
                endif
                "}}}

                call add(targets2, pos)
            endwhile
            " Merge match target dict"{{{
            let t1 = 0 " forward
            let t2 = 0 " backward
            let targets3 = []
            while t1 < len(targets) || t2 < len(targets2)
                " Forward -> Backward -> F -> B -> ...
                if t1 < len(targets)
                    call add(targets3, targets[t1])
                    let t1 += 1
                endif
                if t2 < len(targets2)
                    call add(targets3, targets2[t2])
                    let t2 += 1
                endif
            endwhile
            let targets = targets3
            "}}}
        endif
        "}}}
        " Handle no match"{{{
        let targets_len = len(targets)
        if targets_len == 0
            call s:Throw('No matches')
        endif
        "}}}

        " Attach specific key as marker to gathered matched coordinates
        let GroupingFn = function('s:GroupingAlgorithm' . s:grouping_algorithms[g:EasyMotion_grouping])
        let groups = GroupingFn(targets, split(g:EasyMotion_keys, '\zs'))

        " -- Shade inactive source --------------- {{{
        if g:EasyMotion_do_shade && targets_len != 1 && s:flag.dot_repeat != 1
            if a:direction == 1 " Backward
                let shade_hl_re = s:flag.within_line
                                \ ? '^.*\%#'
                                \ : '\%'. win_first_line .'l\_.*\%#'
            elseif a:direction == 0 " Forward
                let shade_hl_re = s:flag.within_line
                                \ ? '\%#.*$'
                                \ : '\%#\_.*\%'. win_last_line .'l'
            else " Both directions
                let shade_hl_re = s:flag.within_line
                                \ ? '^.*\%#.*$'
                                \ : '\_.*'
            endif

            call EasyMotion#highlight#add_highlight(
                \ shade_hl_re, g:EasyMotion_hl_group_shade)
            if g:EasyMotion_cursor_highlight
                let cursor_hl_re = '\%#'
                call EasyMotion#highlight#add_highlight(cursor_hl_re,
                    \ g:EasyMotion_hl_inc_cursor)
            endif
        endif
        " }}}

        " -- Jump back before prompt for visual scroll {{{
        " Because searchpos() change current cursor position and
        " if you just use cursor(s:current.cursor_position) to jump back,
        " current line will become middle of line window
        if ! empty(a:visualmode)
            keepjumps call winrestview({'lnum' : s:current.cursor_position[0], 'topline' : win_first_line})
        else
            " for adjusting cursorline
            keepjumps call cursor(s:current.cursor_position)
        endif
        "}}}

        " -- Prompt user for target group/character {{{
        if s:flag.dot_repeat != 1
            let coords = s:PromptUser(groups)
        else
            let coords = s:DotPromptUser(groups)
        endif
        "}}}

        " -- Update cursor position -------------- {{{
        " First, jump back cursor to original position
        keepjumps call cursor(s:current.original_position)

        " Consider EasyMotion as jump motion :h jump-motion
        normal! m`

        " Update selection for visual mode {{{
        if ! empty(a:visualmode)
            exec 'normal! ' . a:visualmode
        endif
        " }}}

        " For bi-directional motion, checking again whether the motion is
        " inclusive is necessary. This value will might be updated later
        let is_inclusive_check = a:is_inclusive
        " For bi-directional motion, store 'true' direction for dot repeat
        " to handling inclusive/exclusive motion
        if a:direction == 2
            let true_direction =
                \ EasyMotion#helper#is_greater_coords(
                \   s:current.original_position, coords) > 0 ?
                \ 0 : 1
                " forward : backward
        else
            let true_direction = a:direction
        endif

        if s:flag.dot_repeat == 1
            " support dot repeat {{{
            " Use visual mode to emulate dot repeat
            normal! v

            " Deal with exclusive {{{
            if s:dot_repeat.is_inclusive == 0
                " exclusive
                if s:dot_repeat.true_direction == 0 "Forward
                    let coords[1] -= 1
                elseif s:dot_repeat.true_direction == 1 "Backward
                    " Shift visual selection to left by making cursor one key
                    " left.
                    normal! hoh
                endif
            endif "}}}

            " Jump to destination
            keepjumps call cursor(coords[0], coords[1])

            " Execute previous operator
            let cmd = s:dot_repeat.operator
            if s:dot_repeat.operator ==# 'c'
                let cmd .= getreg('.')
            endif
            exec 'normal! ' . cmd
            "}}}
        else
            " Handle inclusive & exclusive {{{
            " Overwrite inclusive flag for special case {{{
            if s:flag.find_bd == 1 && true_direction == 1
                " Note: For bi-directional find motion s(f) & t
                " If true_direction is backward, the motion is 'exclusive'
                let is_inclusive_check = 0 " overwrite
                let s:previous.is_inclusive = 0 " overwrite
            endif "}}}
            if is_inclusive_check
                " Note: {{{
                " Inclusive motion requires that we eat one more
                " character to the right by forcing the motion to inclusive
                " if we're using a forward motion because
                " > :h exclusive
                " > Note that when using ':' any motion becomes characterwise
                " > exclusive.
                " and EasyMotion use ':'
                " See: h: o_v }}}
                normal! v
            endif " }}}

            if s:current.is_operator && s:flag.linewise
                " TODO: Is there better solution?
                " Maike it linewise
                normal! V
            endif

            " Adjust screen especially for visual scroll & offscreen search {{{
            " Otherwise, cursor line will move middle line of window
            keepjumps call winrestview({'lnum' : win_first_line, 'topline' : win_first_line})

            " Jump to destination
            keepjumps call cursor(coords[0], coords[1])

            " To avoid side effect of overwriting buffer {{{
            " for tpope/vim-repeat
            " See: :h b:changedtick
            if exists('g:repeat_tick')
                if g:repeat_tick == s:current.changedtick
                    let g:repeat_tick = b:changedtick
                endif
            endif "}}}
        endif

        " Set tpope/vim-repeat {{{
        if s:current.is_operator == 1 &&
                \ !(v:operator ==# 'y' && match(&cpo, 'y') == -1)
            " Store previous info for dot repeat {{{
            let s:dot_repeat.regexp = a:regexp
            let s:dot_repeat.direction = a:direction
            let s:dot_repeat.line_flag = s:flag.within_line
            let s:dot_repeat.is_inclusive = is_inclusive_check
            let s:dot_repeat.operator = v:operator
            let s:dot_repeat.bd_t_flag = s:flag.bd_t " Bidirectional t motion
            let s:dot_repeat.true_direction = true_direction " Check inclusive
            "}}}
            silent! call repeat#set("\<Plug>(easymotion-dotrepeat)")
        endif "}}}

        " Highlight all the matches by n-key find motions {{{
        if s:current.is_search == 1 && s:current.is_operator == 0 && g:EasyMotion_add_search_history
            " It seems let &hlsearch=&hlsearch doesn't work when called
            " in script, so use :h feedkeys() instead.
            " Ref: :h v:hlsearch
            " FIXME: doesn't work with `c` operator
            call EasyMotion#helper#silent_feedkeys(
                                    \ ":let &hlsearch=&hlsearch\<CR>",
                                    \ 'hlsearch', 'n')
        endif "}}}

        call s:Message('Jumping to [' . coords[0] . ', ' . coords[1] . ']')
        let s:EasyMotion_is_cancelled = 0 " Success
        "}}}
    catch /^EasyMotion:.*/
        redraw

        " Show exception message
        " The verbose option will take precedence
        if g:EasyMotion_verbose == 1 && g:EasyMotion_ignore_exception != 1
            echo v:exception
        endif

        let s:previous['regexp'] = a:regexp
        " -- Activate EasyMotion ----------------- {{{
        let s:EasyMotion_is_active = 1
        call EasyMotion#attach_active_autocmd() "}}}

        call s:restore_cursor_state(a:visualmode)
        let s:EasyMotion_is_cancelled = 1 " Cancel
    catch
        call s:Message(v:exception . ' : ' . v:throwpoint)
        call s:restore_cursor_state(a:visualmode)
        let s:EasyMotion_is_cancelled = 1 " Cancel
    finally
        " -- Restore properties ------------------ {{{
        call s:RestoreValue()
        call s:turn_on_hl_error()
        call EasyMotion#reset()
        " }}}
        " -- Remove shading ---------------------- {{{
        call EasyMotion#highlight#delete_highlight()
        " }}}

        if s:EasyMotion_is_cancelled == 0 " Success
            " -- Landing Highlight ------------------- {{{
            if g:EasyMotion_landing_highlight
                call EasyMotion#highlight#add_highlight(a:regexp,
                                                      \ g:EasyMotion_hl_move)
                call EasyMotion#highlight#attach_autocmd()
            endif "}}}
            " -- Activate EasyMotion ----------------- {{{
            let s:EasyMotion_is_active = 1
            call EasyMotion#attach_active_autocmd() "}}}
        endif
    endtry
endfunction " }}}
"}}}
" }}}

call EasyMotion#init()
" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
unlet s:save_cpo
" }}}
" vim: fdm=marker:et:ts=4:sw=4:sts=4
