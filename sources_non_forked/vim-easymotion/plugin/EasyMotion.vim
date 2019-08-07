scriptencoding utf-8
" EasyMotion - Vim motions on speed!
"
" Author: Kim Silkebækken <kim.silkebaekken+vim@gmail.com>
"         haya14busa <hayabusa1419@gmail.com>
" Source: https://github.com/easymotion/vim-easymotion
" == Script initialization {{{
if expand("%:p") ==# expand("<sfile>:p")
  unlet! g:EasyMotion_loaded
endif
if exists('g:EasyMotion_loaded') || &compatible || version < 703
    finish
endif

let g:EasyMotion_loaded = 1
" }}}

" == Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

" == Default configuration {{{
" -- Option ------------------------------ {{{
let g:EasyMotion_keys               = get(g:,
    \ 'EasyMotion_keys', 'asdghklqwertyuiopzxcvbnmfj;')
    " \ 'EasyMotion_keys', 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
let g:EasyMotion_do_mapping         = get(g: , 'EasyMotion_do_mapping'         , 1)
let g:EasyMotion_do_shade           = get(g: , 'EasyMotion_do_shade'           , 1)
let g:EasyMotion_grouping           = get(g: , 'EasyMotion_grouping'           , 1)
let g:EasyMotion_startofline        = get(g: , 'EasyMotion_startofline'        , 1)
let g:EasyMotion_smartcase          = get(g: , 'EasyMotion_smartcase'          , 0)
let g:EasyMotion_skipfoldedline     = get(g: , 'EasyMotion_skipfoldedline'     , 1)
let g:EasyMotion_use_migemo         = get(g: , 'EasyMotion_use_migemo'         , 0)
let g:EasyMotion_use_upper          = get(g: , 'EasyMotion_use_upper'          , 0)
let g:EasyMotion_enter_jump_first   = get(g: , 'EasyMotion_enter_jump_first'   , 0)
let g:EasyMotion_space_jump_first   = get(g: , 'EasyMotion_space_jump_first'   , 0)
let g:EasyMotion_inc_highlight      = get(g: , 'EasyMotion_inc_highlight'      , 1)
let g:EasyMotion_move_highlight     = get(g: , 'EasyMotion_move_highlight'     , 1)
let g:EasyMotion_landing_highlight  = get(g: , 'EasyMotion_landing_highlight'  , 0)
let g:EasyMotion_cursor_highlight   = get(g: , 'EasyMotion_cursor_highlight'   , 1)
let g:EasyMotion_use_regexp         = get(g: , 'EasyMotion_use_regexp'         , 1)
let g:EasyMotion_add_search_history = get(g: , 'EasyMotion_add_search_history' , 1)
let g:EasyMotion_off_screen_search  = get(g: , 'EasyMotion_off_screen_search'  , 1)
let g:EasyMotion_force_csapprox     = get(g: , 'EasyMotion_force_csapprox'     , 0)
let g:EasyMotion_show_prompt        = get(g: , 'EasyMotion_show_prompt'        , 1)
let g:EasyMotion_verbose            = get(g: , 'EasyMotion_verbose'            , 1)
let g:EasyMotion_prompt             =
    \ get(g: , 'EasyMotion_prompt' , 'Search for {n} character(s): ')
let g:EasyMotion_command_line_key_mappings =
    \ get(g: , 'EasyMotion_command_line_key_mappings' , {})
let g:EasyMotion_disable_two_key_combo     =
    \ get(g: , 'EasyMotion_disable_two_key_combo' , 0)

"}}}

" }}}

" == <Plug> Mapping {{{
" Note: bd is short for bidirectional
"       l is short for (within) line

function! s:motion_map_helper(motions) "{{{
    for [name, dict] in items(a:motions)
        let mapargs = []
        let xmapargs = []
        if dict.fnc ==# 'S' || dict.fnc ==# 'SL' || dict.fnc ==# 'T' || dict.fnc ==# 'TL'
            let mapargs  += [dict.cnt, 0, dict.direction]
            let xmapargs += [dict.cnt, 1, dict.direction]
        elseif dict.fnc ==# 'Search'
            let mapargs  += [0, dict.direction, dict.respect_direction]
            let xmapargs += [1, dict.direction, dict.respect_direction]
        else
            let mapargs  += [0, dict.direction]
            let xmapargs += [1, dict.direction]
        endif

        silent exec 'noremap  <silent><Plug>(easymotion-'.name.')' .
            \ '      :<C-u>call EasyMotion#' . dict.fnc . '('. join(mapargs, ',') . ')<CR>'
        silent exec 'xnoremap <silent><Plug>(easymotion-'.name.')' .
            \ ' <Esc>:<C-u>call EasyMotion#' . dict.fnc . '('. join(xmapargs, ',') . ')<CR>'
    " Example:
    " noremap  <silent><Plug>(easymotion-f2) :<C-u>call EasyMotion#S(2,1,0)<CR>
    " xnoremap <silent><Plug>(easymotion-f2) <Esc>:<C-u>call EasyMotion#S(2,1,0)<CR>
    endfor
endfunction "}}}

" Find Motion: {{{
call s:motion_map_helper({
    \ 'f'               : {'fnc' : 'S'  , 'cnt' : 1, 'direction'  : 0},
    \ 'F'               : {'fnc' : 'S'  , 'cnt' : 1, 'direction'  : 1},
    \ 's'               : {'fnc' : 'S'  , 'cnt' : 1, 'direction'  : 2},
    \ 'bd-f'            : {'fnc' : 'S'  , 'cnt' : 1, 'direction'  : 2},
    \ 't'               : {'fnc' : 'T'  , 'cnt' : 1, 'direction'  : 0},
    \ 'T'               : {'fnc' : 'T'  , 'cnt' : 1, 'direction'  : 1},
    \ 'bd-t'            : {'fnc' : 'T'  , 'cnt' : 1, 'direction'  : 2},
    \ 'fl'              : {'fnc' : 'SL' , 'cnt' : 1, 'direction'  : 0},
    \ 'Fl'              : {'fnc' : 'SL' , 'cnt' : 1, 'direction'  : 1},
    \ 'sl'              : {'fnc' : 'SL' , 'cnt' : 1, 'direction'  : 2},
    \ 'bd-fl'           : {'fnc' : 'SL' , 'cnt' : 1, 'direction'  : 2},
    \ 'tl'              : {'fnc' : 'TL' , 'cnt' : 1, 'direction'  : 0},
    \ 'Tl'              : {'fnc' : 'TL' , 'cnt' : 1, 'direction'  : 1},
    \ 'bd-tl'           : {'fnc' : 'TL' , 'cnt' : 1, 'direction'  : 2},
    \
    \ 'f2'              : {'fnc' : 'S'  , 'cnt' : 2, 'direction'  : 0},
    \ 'F2'              : {'fnc' : 'S'  , 'cnt' : 2, 'direction'  : 1},
    \ 's2'              : {'fnc' : 'S'  , 'cnt' : 2, 'direction'  : 2},
    \ 'bd-f2'           : {'fnc' : 'S'  , 'cnt' : 2, 'direction'  : 2},
    \ 't2'              : {'fnc' : 'T'  , 'cnt' : 2, 'direction'  : 0},
    \ 'T2'              : {'fnc' : 'T'  , 'cnt' : 2, 'direction'  : 1},
    \ 'bd-t2'           : {'fnc' : 'T'  , 'cnt' : 2, 'direction'  : 2},
    \ 'fl2'             : {'fnc' : 'SL' , 'cnt' : 2, 'direction'  : 0},
    \ 'Fl2'             : {'fnc' : 'SL' , 'cnt' : 2, 'direction'  : 1},
    \ 'sl2'             : {'fnc' : 'SL' , 'cnt' : 2, 'direction'  : 2},
    \ 'bd-fl2'          : {'fnc' : 'SL' , 'cnt' : 2, 'direction'  : 2},
    \ 'tl2'             : {'fnc' : 'TL' , 'cnt' : 2, 'direction'  : 0},
    \ 'Tl2'             : {'fnc' : 'TL' , 'cnt' : 2, 'direction'  : 1},
    \ 'bd-tl2'          : {'fnc' : 'TL' , 'cnt' : 2, 'direction'  : 2},
    \
    \ 'fn'              : {'fnc' : 'S'  , 'cnt' : -1, 'direction' : 0},
    \ 'Fn'              : {'fnc' : 'S'  , 'cnt' : -1, 'direction' : 1},
    \ 'sn'              : {'fnc' : 'S'  , 'cnt' : -1, 'direction' : 2},
    \ 'bd-fn'           : {'fnc' : 'S'  , 'cnt' : -1, 'direction' : 2},
    \ 'tn'              : {'fnc' : 'T'  , 'cnt' : -1, 'direction' : 0},
    \ 'Tn'              : {'fnc' : 'T'  , 'cnt' : -1, 'direction' : 1},
    \ 'bd-tn'           : {'fnc' : 'T'  , 'cnt' : -1, 'direction' : 2},
    \ 'fln'             : {'fnc' : 'SL' , 'cnt' : -1, 'direction' : 0},
    \ 'Fln'             : {'fnc' : 'SL' , 'cnt' : -1, 'direction' : 1},
    \ 'sln'             : {'fnc' : 'SL' , 'cnt' : -1, 'direction' : 2},
    \ 'bd-fln'          : {'fnc' : 'SL' , 'cnt' : -1, 'direction' : 2},
    \ 'tln'             : {'fnc' : 'TL' , 'cnt' : -1, 'direction' : 0},
    \ 'Tln'             : {'fnc' : 'TL' , 'cnt' : -1, 'direction' : 1},
    \ 'bd-tln'          : {'fnc' : 'TL' , 'cnt' : -1, 'direction' : 2},
    \ })

nnoremap <silent> <Plug>(easymotion-overwin-f) :<C-u>call EasyMotion#OverwinF(1)<CR>
nnoremap <silent> <Plug>(easymotion-overwin-f2) :<C-u>call EasyMotion#OverwinF(2)<CR>
nnoremap <silent> <Plug>(easymotion-overwin-line) :<C-u>call EasyMotion#overwin#line()<CR>
nnoremap <silent> <Plug>(easymotion-overwin-w) :<C-u>call EasyMotion#overwin#w()<CR>

"}}}

" -- Word Motion {{{
call s:motion_map_helper({
    \ 'w'               : {'fnc' : 'WB' , 'direction' : 0},
    \ 'b'               : {'fnc' : 'WB' , 'direction' : 1},
    \ 'bd-w'            : {'fnc' : 'WB' , 'direction' : 2},
    \ 'W'               : {'fnc' : 'WBW', 'direction' : 0},
    \ 'B'               : {'fnc' : 'WBW', 'direction' : 1},
    \ 'bd-W'            : {'fnc' : 'WBW', 'direction' : 2},
    \ 'iskeyword-w'     : {'fnc' : 'WBK', 'direction' : 0},
    \ 'iskeyword-b'     : {'fnc' : 'WBK', 'direction' : 1},
    \ 'iskeyword-bd-w'  : {'fnc' : 'WBK', 'direction' : 2},
    \
    \ 'e'               : {'fnc' : 'E'  , 'direction' : 0},
    \ 'ge'              : {'fnc' : 'E'  , 'direction' : 1},
    \ 'bd-e'            : {'fnc' : 'E'  , 'direction' : 2},
    \ 'E'               : {'fnc' : 'EW' , 'direction' : 0},
    \ 'gE'              : {'fnc' : 'EW' , 'direction' : 1},
    \ 'bd-E'            : {'fnc' : 'EW' , 'direction' : 2},
    \ 'iskeyword-e'     : {'fnc' : 'EK' , 'direction' : 0},
    \ 'iskeyword-ge'    : {'fnc' : 'EK' , 'direction' : 1},
    \ 'iskeyword-bd-e'  : {'fnc' : 'EK' , 'direction' : 2},
    \ })
"}}}

" -- JK Motion {{{
call s:motion_map_helper({
    \ 'j'               : {'fnc' : 'JK' , 'direction' : 0},
    \ 'k'               : {'fnc' : 'JK' , 'direction' : 1},
    \ 'bd-jk'           : {'fnc' : 'JK' , 'direction' : 2},
    \ 'sol-j'           : {'fnc' : 'Sol', 'direction' : 0},
    \ 'sol-k'           : {'fnc' : 'Sol', 'direction' : 1},
    \ 'sol-bd-jk'       : {'fnc' : 'Sol', 'direction' : 2},
    \ 'eol-j'           : {'fnc' : 'Eol', 'direction' : 0},
    \ 'eol-k'           : {'fnc' : 'Eol', 'direction' : 1},
    \ 'eol-bd-jk'       : {'fnc' : 'Eol', 'direction' : 2},
    \ })
"}}}

" -- Search Motion {{{
call s:motion_map_helper({
    \ 'n'               : {'fnc' : 'Search', 'direction': 0, 'respect_direction': 0},
    \ 'N'               : {'fnc' : 'Search', 'direction': 1, 'respect_direction': 0},
    \ 'bd-n'            : {'fnc' : 'Search', 'direction': 2, 'respect_direction': 0},
    \ 'vim-n'           : {'fnc' : 'Search', 'direction': 0, 'respect_direction': 1},
    \ 'vim-N'           : {'fnc' : 'Search', 'direction': 1, 'respect_direction': 1},
    \ })
"}}}

" -- Jump To Anywhere Motion {{{
call s:motion_map_helper({
    \ 'jumptoanywhere'  : {'fnc' : 'JumpToAnywhere', 'direction': 2},
    \ })
"}}}

" -- Line Motion {{{
call s:motion_map_helper({
    \ 'wl'              : {'fnc' : 'WBL', 'direction': 0},
    \ 'bl'              : {'fnc' : 'WBL', 'direction': 1},
    \ 'bd-wl'           : {'fnc' : 'WBL', 'direction': 2},
    \ 'el'              : {'fnc' : 'EL' , 'direction': 0},
    \ 'gel'             : {'fnc' : 'EL' , 'direction': 1},
    \ 'bd-el'           : {'fnc' : 'EL' , 'direction': 2},
    \ 'lineforward'     : {'fnc' : 'LineAnywhere', 'direction': 0},
    \ 'linebackward'    : {'fnc' : 'LineAnywhere', 'direction': 1},
    \ 'lineanywhere'    : {'fnc' : 'LineAnywhere', 'direction': 2},
    \ })
"}}}

" -- Next, Previous Motion {{{
noremap  <silent><Plug>(easymotion-next)
    \      :<C-u>call EasyMotion#NextPrevious(0,0)<CR>
xnoremap <silent><Plug>(easymotion-next)
    \      :<C-u>call EasyMotion#NextPrevious(1,0)<CR>

noremap  <silent><Plug>(easymotion-prev)
    \      :<C-u>call EasyMotion#NextPrevious(0,1)<CR>
xnoremap <silent><Plug>(easymotion-prev)
    \      :<C-u>call EasyMotion#NextPrevious(1,1)<CR>
"}}}

" -- Repeat Motion {{{
noremap  <silent><Plug>(easymotion-repeat)
    \      :<C-u>call EasyMotion#Repeat(0)<CR>
xnoremap <silent><Plug>(easymotion-repeat)
    \ <Esc>:<C-u>call EasyMotion#Repeat(1)<CR>

noremap  <silent><Plug>(easymotion-dotrepeat)
    \      :<C-u>call EasyMotion#DotRepeat()<CR>
"}}}

noremap  <silent><Plug>(easymotion-activate) :<C-u>call EasyMotion#activate(0)<CR>
xnoremap <silent><Plug>(easymotion-activate) :<C-u>call EasyMotion#activate(1)<CR>
" }}}

" == Default key mapping {{{
if g:EasyMotion_do_mapping == 1
    " Prepare Prefix: {{{
    if exists('g:EasyMotion_leader_key')
        exec 'map ' . g:EasyMotion_leader_key . ' <Plug>(easymotion-prefix)'
    else
        if !hasmapto('<Plug>(easymotion-prefix)')
            map <Leader><Leader> <Plug>(easymotion-prefix)
        endif
    endif
    "}}}

    function! s:default_mapping(motions, do_mapping) "{{{
        for motion in a:motions
            " Mapping {{{
            if exists('g:EasyMotion_mapping_' . motion)
                " Backward compatible mapping [deprecated]
                silent exec 'map <silent> ' .
                    \ eval('g:EasyMotion_mapping_' . motion) . ' <Plug>(easymotion-' . motion . ')'
            elseif a:do_mapping
                    \ && !hasmapto('<Plug>(easymotion-' . motion . ')')
                    \ && empty(maparg('<Plug>(easymotion-prefix)' . motion, 'nov'))

                " Do mapping
                silent exec 'map <silent> ' .
                    \'<Plug>(easymotion-prefix)' . motion . ' <Plug>(easymotion-' . motion . ')'
            endif "}}}
        endfor
    endfunction "}}}

    " Default Mapping:
    call s:default_mapping(
        \ ['f', 'F', 's', 't', 'T',
        \  'w', 'W', 'b', 'B', 'e', 'E', 'ge', 'gE',
        \  'j', 'k', 'n', 'N'], g:EasyMotion_do_mapping)
endif "}}}

" == CommandLine Mapping {{{
command! -nargs=*
\   EMCommandLineNoreMap
\   call EasyMotion#command_line#cnoremap([<f-args>])
command! -nargs=*
\   EMCommandLineMap
\   call EasyMotion#command_line#cmap([<f-args>])
command! -nargs=1
\   EMCommandLineUnMap
\   call EasyMotion#command_line#cunmap(<f-args>)
"}}}

" == Restore 'cpoptions' {{{
let &cpo = s:save_cpo
unlet s:save_cpo
" }}}
" vim: fdm=marker:et:ts=4:sw=4:sts=4
