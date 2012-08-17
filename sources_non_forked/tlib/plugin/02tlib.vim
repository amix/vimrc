" tlib.vim -- Some utility functions
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-04-10.
" @Last Change: 2011-04-28.
" @Revision:    672
" GetLatestVimScripts: 1863 1 tlib.vim

if &cp || exists("loaded_tlib")
    finish
endif
if v:version < 700 "{{{2
    echoerr "tlib requires Vim >= 7"
    finish
endif
let loaded_tlib = 41

let s:save_cpo = &cpo
set cpo&vim


" Init~ {{{1
" call tlib#autocmdgroup#Init()


" Commands~ {{{1

" :display: TRequire NAME [VERSION [FILE]]
" Make a certain vim file is loaded.
"
" Conventions: If FILE isn't defined, plugin/NAME.vim is loaded. The 
" file must provide a variable loaded_{NAME} that represents the version 
" number.
command! -nargs=+ TRequire let s:require = [<f-args>]
            \ | if !exists('loaded_'. get(s:require, 0))
                \ | exec 'runtime '. get(s:require, 2, 'plugin/'. get(s:require, 0) .'.vim')
                \ | if !exists('loaded_'. get(s:require, 0)) || loaded_{get(s:require, 0)} < get(s:require, 1, loaded_{get(s:require, 0)})
                    \ | echoerr 'Require '.  get(s:require, 0) .' >= '. get(s:require, 1, 'any version will do')
                    \ | finish
                    \ | endif
                \ | endif | unlet s:require


" :display: :TLet VAR = VALUE
" Set a variable only if it doesn't already exist.
" EXAMPLES: >
"   TLet foo = 1
"   TLet foo = 2
"   echo foo
"   => 1
command! -nargs=+ TLet if !exists(matchstr(<q-args>, '^[^=[:space:]]\+')) | exec 'let '. <q-args> | endif


" Open a scratch buffer (a buffer without a file).
"   TScratch  ... use split window
"   TScratch! ... use the whole frame
" This command takes an (inner) dictionary as optional argument.
" EXAMPLES: >
"   TScratch 'scratch': '__FOO__'
"   => Open a scratch buffer named __FOO__
command! -bar -nargs=* -bang TScratch call tlib#scratch#UseScratch({'scratch_split': '<bang>' != '!', <args>})


" :display: :TVarArg VAR1, [VAR2, DEFAULT2] ...
" A convenience wrapper for |tlib#arg#Let|.
" EXAMPLES: >
"   function! Foo(...)
"       TVarArg ['a', 1], 'b'
"       echo 'a='. a
"       echo 'b='. b
"   endf
command! -nargs=+ TVarArg exec tlib#arg#Let([<args>])


" :display: :TKeyArg DICT, VAR1, [VAR2, DEFAULT2] ...
" A convenience wrapper for |tlib#arg#Let|.
" EXAMPLES: >
"   function! Foo(keyargs)
"       TKeyArg a:keyargs, ['a', 1], 'b'
"       echo 'a='. a
"       echo 'b='. b
"   endf
command! -nargs=+ TKeyArg exec tlib#arg#Key([<args>])


" :display: TBrowseOutput COMMAND
" Ever wondered how to efficiently browse the output of a command 
" without redirecting it to a file? This command takes a command as 
" argument and presents the output via |tlib#input#List()| so that you 
" can easily search for a keyword (e.g. the name of a variable or 
" function) and the like.
"
" If you press enter, the selected line will be copied to the command 
" line. Press ESC to cancel browsing.
"
" EXAMPLES: >
"   TBrowseOutput 20verb TeaseTheCulprit
command! -nargs=1 -complete=command TBrowseOutput call tlib#cmd#BrowseOutput(<q-args>)

" :display: TBrowseScriptnames
" List all sourced script names (the output of ':scriptnames').
"
" When you press enter, the selected script will be opened in the current
" window. Press ESC to cancel.
"
" EXAMPLES: >
"   TBrowseScriptnames 
command! -nargs=0 -complete=command TBrowseScriptnames call
            \ tlib#cmd#BrowseOutputWithCallback("tlib#cmd#ParseScriptname", "scriptnames")

" :display: TTimeCommand CMD
" Time the execution time of CMD.
command! -nargs=1 -complete=command TTimeCommand call tlib#cmd#Time(<q-args>)



" Variables~ {{{1

" When 1, automatically select the last remaining item only if the list 
" had only one item to begin with.
" When 2, automatically select a last remaining item after applying 
" any filters.
TLet g:tlib_pick_last_item = 1

" If a list is bigger than this value, don't try to be smart when 
" selecting an item. Be slightly faster instead.
TLet g:tlib_sortprefs_threshold = 200

" Scratch window position. By default the list window is opened on the 
" bottom. Set this variable to 'topleft' or '' to change this behaviour.
TLet g:tlib_scratch_pos = 'botright'

" Size of the input list window (in percent) from the main size (of &lines).
TLet g:tlib_inputlist_pct = 50

" Size of filename columns when listing filenames.
TLet g:tlib_inputlist_width_filename = '&co / 3'
" TLet g:tlib_inputlist_width_filename = 25

" The highlight group to use for showing matches in the input list window.
TLet g:tlib_inputlist_higroup = 'IncSearch'

" If a list contains more items, don't do an incremental "live search", 
" but use |input()| to query the user for a filter. This is useful on 
" slower machines or with very long lists.
TLet g:tlib_inputlist_livesearch_threshold = 1000

" If true, show some indicators about the status of a filename (e.g. 
" buflisted(), bufloaded() etc.).
" This is disabled by default because vim checks also for the file on 
" disk when doing this.
TLet g:tlib_inputlist_filename_indicators = 0

" Can be "cnf", "cnfd", "seq", or "fuzzy". See:
"   cnf :: Match substrings
"     - |tlib#Filter_cnf#New()| (this is the default method)
"     - |tlib#Filter_cnfd#New()|
"   seq :: Match sequences of characters
"     - |tlib#Filter_seq#New()|
"   fuzzy :: Match fuzzy character sequences
"     - |tlib#Filter_fuzzy#New()|
TLet g:tlib_inputlist_match = 'cnf'

" If not null, display only a short info about the filter.
TLet g:tlib_inputlist_shortmessage = 0

" Extra tags for |tlib#tag#Retrieve()| (see there). Can also be buffer-local.
TLet g:tlib_tags_extra = ''

" Filter the tag description through |substitute()| for these filetypes. 
" This applies only if the tag cmd field (see |taglist()|) is used.
" :nodefault:
TLet g:tlib_tag_substitute = {
            \ 'java': [['\s*{\s*$', '', '']],
            \ 'ruby': [['\<\(def\|class\|module\)\>\s\+', '', '']],
            \ 'vim':  [
            \   ['^\s*com\%[mand]!\?\(\s\+-\S\+\)*\s*\u\w*\zs.*$', '', ''],
            \   ['^\s*\(let\|aug\%[roup]\|fu\%[nction]!\?\|com\%[mand]!\?\(\s\+-\S\+\)*\)\s*', '', ''],
            \   ['"\?\s*{{{\d.*$', '', ''],
            \ ],
            \ }

" " Alternative rx for keywords, in case 'iskeyword' is inadequate for 
" " the purposes of tlib but you don't want to change it's value.
" TLet g:tlib_keyword_rx = {
"             \ 'vim': '\(\w\|#\)',
"             \ }

TLet g:tlib_filename_sep = '/'
" TLet g:tlib_filename_sep = exists('+shellslash') && !&shellslash ? '\' : '/'   " {{{2

" The cache directory. If empty, use |tlib#dir#MyRuntime|.'/cache'.
" You might want to delete old files from this directory from time to 
" time with a command like: >
"   find ~/vimfiles/cache/ -atime +31 -type f -print -delete
TLet g:tlib_cache = ''

" Where to display the line when using |tlib#buffer#ViewLine|.
" For possible values for position see |scroll-cursor|.
TLet g:tlib_viewline_position = 'zz'

" :doc:
" Keys for |tlib#input#List|~

TLet g:tlib_inputlist_and = ' '
TLet g:tlib_inputlist_or  = '|'
TLet g:tlib_inputlist_not = '-'

" When editing a list with |tlib#input#List|, typing these numeric chars 
" (as returned by getchar()) will select an item based on its index, not 
" based on its name. I.e. in the default setting, typing a "4" will 
" select the fourth item, not the item called "4".
" In order to make keys 0-9 filter the items in the list and make 
" <m-[0-9]> select an item by its index, remove the keys 48 to 57 from 
" this dictionary.
" Format: [KEY] = BASE ... the number is calculated as KEY - BASE.
" :nodefault:
TLet g:tlib_numeric_chars = {
            \ 176: 176,
            \ 177: 176,
            \ 178: 176,
            \ 179: 176,
            \ 180: 176,
            \ 181: 176,
            \ 182: 176,
            \ 183: 176,
            \ 184: 176,
            \ 185: 176,
            \}
            " \ 48: 48,
            " \ 49: 48,
            " \ 50: 48,
            " \ 51: 48,
            " \ 52: 48,
            " \ 53: 48,
            " \ 54: 48,
            " \ 55: 48,
            " \ 56: 48,
            " \ 57: 48,

" :nodefault:
" The default key bindings for single-item-select list views. If you 
" want to use <c-j>, <c-k> to move the cursor up and down, add these two 
" lines to after/plugin/02tlib.vim: >
"
"   let g:tlib_keyagents_InputList_s[10] = 'tlib#agent#Down'  " <c-j>
"   let g:tlib_keyagents_InputList_s[11] = 'tlib#agent#Up'    " <c-k>
TLet g:tlib_keyagents_InputList_s = {
            \ "\<PageUp>":   'tlib#agent#PageUp',
            \ "\<PageDown>": 'tlib#agent#PageDown',
            \ "\<Up>":       'tlib#agent#Up',
            \ "\<Down>":     'tlib#agent#Down',
            \ "\<c-Up>":     'tlib#agent#UpN',
            \ "\<c-Down>":   'tlib#agent#DownN',
            \ "\<Left>":     'tlib#agent#ShiftLeft',
            \ "\<Right>":    'tlib#agent#ShiftRight',
            \ 18:            'tlib#agent#Reset',
            \ 242:           'tlib#agent#Reset',
            \ 17:            'tlib#agent#Input',
            \ 241:           'tlib#agent#Input',
            \ 27:            'tlib#agent#Exit',
            \ 26:            'tlib#agent#Suspend',
            \ 250:           'tlib#agent#Suspend',
            \ 15:            'tlib#agent#SuspendToParentWindow',  
            \ 63:            'tlib#agent#Help',
            \ "\<F1>":       'tlib#agent#Help',
            \ "\<bs>":       'tlib#agent#ReduceFilter',
            \ "\<del>":      'tlib#agent#ReduceFilter',
            \ "\<c-bs>":     'tlib#agent#PopFilter',
            \ "\<m-bs>":     'tlib#agent#PopFilter',
            \ "\<c-del>":    'tlib#agent#PopFilter',
            \ "\<m-del>":    'tlib#agent#PopFilter',
            \ "\<s-space>":  'tlib#agent#Wildcard',
            \ 191:           'tlib#agent#Debug',
            \ char2nr(g:tlib_inputlist_or):  'tlib#agent#OR',
            \ char2nr(g:tlib_inputlist_and): 'tlib#agent#AND',
            \ }

" Number of items to move when pressing <c-up/down> in the input list window.
TLet g:tlib_scroll_lines = 10

" :nodefault:
TLet g:tlib_keyagents_InputList_m = {
            \ 35:          'tlib#agent#Select',
            \ "\<s-up>":   'tlib#agent#SelectUp',
            \ "\<s-down>": 'tlib#agent#SelectDown',
            \ 1:           'tlib#agent#SelectAll',
            \ 225:         'tlib#agent#SelectAll',
            \ }
" "\<c-space>": 'tlib#agent#Select'

" :nodefault:
TLet g:tlib_handlers_EditList = [
            \ {'key': 5,  'agent': 'tlib#agent#EditItem',    'key_name': '<c-e>', 'help': 'Edit item'},
            \ {'key': 4,  'agent': 'tlib#agent#DeleteItems', 'key_name': '<c-d>', 'help': 'Delete item(s)'},
            \ {'key': 14, 'agent': 'tlib#agent#NewItem',     'key_name': '<c-n>', 'help': 'New item'},
            \ {'key': 24, 'agent': 'tlib#agent#Cut',         'key_name': '<c-x>', 'help': 'Cut item(s)'},
            \ {'key':  3, 'agent': 'tlib#agent#Copy',        'key_name': '<c-c>', 'help': 'Copy item(s)'},
            \ {'key': 22, 'agent': 'tlib#agent#Paste',       'key_name': '<c-v>', 'help': 'Paste item(s)'},
            \ {'pick_last_item': 0},
            \ {'return_agent': 'tlib#agent#EditReturnValue'},
            \ {'help_extra': [
            \      'Submit changes by pressing ENTER or <c-s> or <c-w><cr>',
            \      'Cancel editing by pressing <c-w>c'
            \ ]},
            \ ]



" " TEST:
" TRequire tselectbuffer 6
" echo loaded_tselectbuffer



let &cpo = s:save_cpo
unlet s:save_cpo
