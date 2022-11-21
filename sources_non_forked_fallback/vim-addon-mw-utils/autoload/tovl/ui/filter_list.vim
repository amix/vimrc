" OLD CODE !
" I should contribute the multiple filter feature to tlib

" filter list displays a list of items
" you can white / black filter them by regular expressions (similar to the
" tlib TToC command
" However you can edit the filters afterwards and select the cols which should
" be shown

fun! tovl#ui#filter_list#ListTest()
  call tovl#ui#filter_list#ListView({
	\ 'aligned' : 1,
	\ 'Continuation' : funcref#Function('echo string(ARGS[0])'),
	\ 'items' : [ {"aa" : "a\nAAAAAAAAAAA", 'bb' : "bbbbbbbbbbbbb\nB" }, 
		   \  {"aa" : "2a\n2AAAAAAAAAAAA", "bb" : "2 bbbbbbbbbbbbb\n2B"},
		   \  {"aa" : "XXX", "bb" : "YY"} ],
	\ })

endfun

fun! s:Intersection(a, b)
  return filter(copy(a:a), 'index(a:b, v:val) >= 0')
endf

fun! tovl#ui#filter_list#ListTestGotoLineCurrentBuf()
  let nr=1
  let lines = []
  for l in getline(0,line('$'))
    call add(lines, {'nr': nr, 'line' :l})
    let nr = nr +1
  endfor
  call tovl#ui#filter_list#ListView({
	\ 'aligned' : 0,
	\ 'keys' : ['nr','line'],
	\ 'number' : 1,
	\ 'selectByIdOrFilter' : 1,
	\ 'Continuation' : funcref#Function('exec ARGS[0]["nr"]'),
	\ 'items' : lines,
	\ })
endfun

" opens a new filtered list
" keys of opts parameters:
" Continuation: This function will be called with the selected items
" items: { key : (string or dict) }
"        items willl be modified. use copy(youritems) as argument to prevent
"        this. An item is either a string or a dict 
"        (eg {'file' : .., 'line': ... , 'msg' : .. )
" keys: list of keys to be shown (optional)
" filter: list of inital filters which must be applied
" contains [ { filter: .. , keep : .. }, ] see FilterItems() below
" aligned: default 0
" sp_cmd: the command to be used to create the new buffer (default ':e')
" init : 0 / 1 (default 1): wether to show the view right now
" number: 0 /1 (default 1): number items ?
" selectByIdOrFilter: 1: start in getchar() loop so that the user can select
"                        the item even faster
"                     auto: only do this if all items fit on screen
"                     (recommend)
" cmds: extra cmds to be run
" cursorAt : at which item to put the cursor?
"
" If you don't like the default view you can override UpdateDisplay
"
" Usage examples of this list control:
" - db results
" - replacement of the quickfix window
" - select a buffer etc
fun! tovl#ui#filter_list#ListView(opts)
  " ActivateAddons theonevimlib
  let d = {}
  let d.items = a:opts.items
  let d.cursorAt = get(a:opts, 'cursorAt', 0)
  let d.aligned = get(a:opts, 'aligned', 0)
  let d.sep = '  '
  let d.filter = get(a:opts, 'filter', [])
  " using sp because of bd! (FIXME)
  let d.sp_cmd = get(a:opts, 'sp_cmd', 'sp')
  let d.allKeys = {}
  let d.closeOnContinuation = get(a:opts,'closeOnContinuation',1)
  " don't recommend OnSingleMatch, use OnSingleMatchCR instead
  let d.continueOnSingleMatch = get(a:opts, 'continueOnSingleMatch',0)
  let d.continueOnSingleMatchCR = get(a:opts, 'continueOnSingleMatchCR',1)
  let d.selectByIdOrFilter = get(a:opts, 'selectByIdOrFilter', 0)
  let d.linesToItems = {}
  let d.number = get(a:opts, 'number', 1)
  let d.cmds = get(a:opts, 'cmds', [])
  let d.syn_cmds = get(a:opts, 'syn_cmds', [])

  if has_key(a:opts,'keys') | let d.keys = a:opts.keys | endif
  if has_key(a:opts,'Continuation') | let d.Continuation = a:opts.Continuation | endif

  " cache already filtered items in case we want to view really long results
  " contains [ { filter : { regex: .. , keep : .. } , items : .. , cursorAt :}, 
  "            { filter : { ... } , items: .. , cursorAt : }
  let d.cached = []
  " id of buffer
  let d.buffer = -1
  let d.modeText = ''

  fun d.HelpText()
    return [ "you've entered the the help of the powerful filtered view buffer",
	   \ "",
	   \ "type f to start filtering items by regex",
	   \ "type F to start dropping items by regex",
	   \ "k / K will ask you for the key to apply the filter to first",
	   \ "apply the filter by <cr> and press <cr> again to select item",
	   \ "",
	   \ "use :ShowAppliedFilters to list active filters",
	   \ "use :ToggleAlignment to toggle alignment",
	   \ "",
	   \ "TODO: Implement sorting, implement interface to change keys (displayed columns)"
	   \ ]
  endfun

  " create new scratch buffer
  " preprocess items calculating line count and maxwidth for all items
  fun d.NewBufferAndInit()
    let self.bufferId = bufnr(bufname('%'))
    for idx in range(0,len(self.items)-1)
      if type(self.items[idx]) != 4
	" no dict yet, make it one
	let self.items[idx] = {'string_line' : self.items[idx]}
      endif
      let new = {}
      for [k,v] in items(self.items[idx])
	let lines = split(v,"\n")
	let self.items[idx][k] = { 'text' : v, 'rows' : len(lines), 'cols' : max(map(copy(lines),'len(v:val)')), 'lines' : lines }
	let self.allKeys[k] = 1
        unlet k v
      endfor
    endfor
    call tovl#scratch_buffer#ScratchBuffer({
	  \ 'help' : funcref#Function(self.HelpText,{ 'self' : self }),
	  \ 'sp_cmd' : self.sp_cmd,
	  \ 'cmds' : self.cmds
	  \ })
    " I assume we have some kind of formatting anyway. Thus breaking lines is bad!
    set nowrap
    setlocal cursorline
    let b:filtered_view = self
    command! -buffer -nargs=0 ToggleAlignment call b:filtered_view.ToggleAlignment()
    command! -buffer -nargs=0 ShowAppliedFilters call b:filtered_view.ShowAppliedFilters()
    command! -buffer -nargs=0 RemoveFilters call b:filtered_view.RemoveFilters()
    noremap <buffer> f :call b:filtered_view.FilterFromKeyboard(1,'')<cr>
    " noremap <buffer> f :call b:filtered_view.FilterFromKeyboard(1)<cr>
    noremap <buffer> F :call b:filtered_view.FilterFromKeyboard(0,'')<cr>
    if has_key(self,'Continuation')
      nnoremap <buffer> <cr> :call b:filtered_view.Continue()<cr>
    endif
    "noremap <buffer> k
    "noremap <buffer> K

    let [items, cursorAt] = self.FilteredItems()
    " len(items) is an approximation because one item can have multiple
    " lines.. However adding the lines first to check takes too much time
    if self.selectByIdOrFilter == 1 || (self.selectByIdOrFilter == 'auto' && winheight('%') > len(items) )
      call self.SelectByIdOrFilter()
    else
      " user should choose how to proceed
      call self.UpdateDisplay()
    endif
  endfun

  " user interface
  fun d.ToggleAlignment()
    let self.aligned = !self.aligned
    call self.UpdateDisplay()
  endfun
  fun d.ShowAppliedFilters()
    for i in self.filter | echo string(i) | endfor
  endfun
  fun d.RemoveFilters()
    let self.filter = []
    call self.UpdateDisplay()
  endfun
  fun d.Continue()
    let item = self.CurrentItem()
    call self.DoContinue(item)
  endfun
  fun d.DoContinue(v)
    if self.closeOnContinuation | bw! | endif
    call funcref#Call(self.Continuation,[a:v])
  endfun

  fun d.MapToOriginal(v)
    if has_key(a:v, 'string_line')
      return a:v.string_line.text
    else
      let d = {}
      for [k,v] in items(a:v)
	let d[k] = v.text
	unlet k v
      endfor
      return d
    endif
  endfun

  fun d.CurrentItem()
    let idx=line('.')-len(self.headerLines)
    while idx >= 0
      if has_key(self.linesToItems, idx)
	return self.MapToOriginal(self.FilteredItems()[0][self.linesToItems[idx]])
      else
	let idx = idx -1
      endif
    endwhile
    throw "internal error, couldn't determine selected item!"
  endfun

  " updates the filter cache and returns the final filtered items
  fun d.FilteredItems()
    " update cache
    let idx = 0
    let [items, cursorAt] = [self.items, self.cursorAt]
    for idx in range(0, len(self.filter)-1)
      if idx +1 > len(self.cached) || self.cached[idx]['filter'] != self.filter[idx]
	let self.cached = self.cached[:idx-1]
	let [items, cursorAt] = self.FilterItem(copy(items), self.filter[idx], cursorAt)
	call add(self.cached, { 'cursorAt' : cursorAt, 'items' : items, 'filter' : self.filter[idx]})
      else
        let ci = self.cached[idx]
	let [items, cursorAt] = [ci['items'], ci['cursorAt']]
      endif
    endfor
    return [items, cursorAt]
  endfun

  " calling this will return a set of lines which are expected to be the new
  " buffer contents. The self.linesToItems dict is updated
  fun d.UpdateDisplay()

    if empty(self.filter)
      let self.statusline= 'no filter applied, :Help for help'
    else
      let self.statusline = len(self.filter).' '.string(self.filter[-1])
    endif

    let self.linesToItems = {}
    let [items, cursorAt] = self.FilteredItems()
    "let num_width = printf('%.0f', trunc(log10(len(items))+1))
    let num_width = 4
    if self.aligned
      " get column width.. (probably will not work with unicde characters.. I
      " don't have a better solution)
      let maxlens={}
      for i in items
	for [k,v] in items(i)
	  if get(maxlens,k,0) < v.cols
	    let maxlens[k] = v.cols
	  endif
	endfor
      endfor
    endif

    " format lines
    let self.headerLines = [self.modeText]
    let lines = copy(self.headerLines)
    let lines_count = 0
    if self.number
      let fmt_startA = '%'.num_width.'s)'
      let fmt_startB = '%'.num_width.'s'
    else
      let fmt_startA = '' | let fmt_startB = ''
    endif
    let cursorAtLine = 1 " sane default
    for idx in range(0,len(items)-1)
      let self.linesToItems[lines_count + 1] = idx
      let i = items[idx]
      let keys = has_key(self,'keys')
	    \ ? s:Intersection(self.keys, keys(i))
	    \ : keys(i)
      let fmt = ''
      let args =  [i]
      let cols = []
      for k in keys
	let fmt .= self.sep.'%-'.(self.aligned ? maxlens[k] : i[k]['cols']).'s'
	call add(cols, i[k])
      endfor
      for row in range(0, max([1] + map(copy(cols),'v:val["rows"]'))-1)
	let fmt_args = row == 0 ? [fmt_startA.fmt] :  [fmt_startB.fmt]
	if self.number
	  call add(fmt_args, row == 0 ? idx : '')
	endif
	for c in cols
	  call add(fmt_args, c.rows <= row ? '' : c.lines[row])
	endfor
	call add(lines, call('printf', fmt_args))
	let lines_count += 1
      endfor
      if idx == cursorAt
        let cursorAtLine = lines_count
      endif
    endfor
    " update stauts line to show last applied filter
    " disabled cause it causes trouble on :wincmd w
    " setlocal statusline=%!b:filtered_view.statusline

    " syntax
    syn clear
    for s in self.syn_cmds | exec s | endfor
    let id = 0
    " highlight filter regex in buffer as well
    let syn_ids = [ 'Underlined', 'Todo', 'Error', 'Type', 'Statement' ]
    for f in self.filter
      if !f.keep || !has_key(f, 'regex') | continue | endif
      if f.regex != ''
        try
	exec 'syn match '.syn_ids[id % len(syn_ids)].' '.string(f.regex)
        catch /.*/
          " ignore errors such as \ without following characters. Thus just
          " ignore and wait for the next character
        endtry
      endif
      let id = id +1
    endfor
    if len(lines) > winheight('%')
      call extend(lines, self.headerLines)
    endif
    normal ggdG
    call append(0, lines)
    " place cursor
    exec (cursorAtLine+1)
    " move cursor into the middle of the window
    normal zz
  endf

  " filter = keys :
  "  filter = string to be executed containing Val
  "  keep = 1  keep on match 
  "       = 0  drop on match
  "  key (optional)
  "  cursorAt: at which item to put the cursor
  "            if that item is deleted it will be placed at the item above
  " optional: key of dict if dict
  fun d.FilterItem(items, filter, cursorAt)
    let filter = 'Val =~ '.string(a:filter.regex)
    let keep = a:filter.keep
    let cursorAt = a:cursorAt

    for idx in reverse(range(0, len(a:items)-1))
      let i = a:items[idx]
      if has_key(a:filter,'key')
	let key = a:filter.key
	if has_key(i, key)
	  " key given, only filter by this column
	  let Val = i[key]['text']
	  exec 'let any = '.filter
	else
	  let any = 0
	endif
      else
	let any = 0
	" no key given, try all
	for x in values(i)
	  let Val = x['text']
	  exec 'let any =  '.filter
	  if any | break | endif
	endfor
      endif
      if any != keep
	call remove(a:items, idx)
        if idx <= cursorAt
          let cursorAt = cursorAt -1
        endif
      endif
    endfor
    return [a:items, cursorAt]
  endfun

  " if the user enters a number select by index else start filtering..
  fun d.SelectByIdOrFilter()
    let idx=''
    let items = self.FilteredItems()[0]
    try
      let self.modeText = '[0-9]* : select by index| <esc>: escape getchar() loop, any char: start filtering'
      call self.UpdateDisplay() | redraw
      while 1
	let c=getchar()
	if index([13,10],c) >= 0
	  return self.DoContinue(self.MapToOriginal(items[idx]))
	elseif index([27], c) >=0
	  " esc, abort
	  return
	else
	  if type(c) == 0
	    let c = nr2char(c)
	  endif
	  if c == "\<bs>" || index(map(range(0,10),'v:val.""'),c) >= 0
	    if c == "\<bs>"
	      let idx = idx[:-2]
	    else
	      let idx .= c
	    endif
	    if idx < len(items) && idx.'0' > len(items) || idx == 0 && len(items) < 10
	      " only match
	      return self.DoContinue(self.MapToOriginal(items[idx]))
	    endif
	  else
	    return self.FilterFromKeyboard(1,c)
	  endif
	endif
      endwhile
    finally
      let self.modeText = ''
    endtry
  endfun

  " gets a regular expresion filter by keybaord and updates the display while
  " you're typing. The regex ist shown in the statusline
  fun d.FilterFromKeyboard(keep, start, ...)
    let self.modeText = 'press ESC to exit getchar() loop'
    call self.UpdateDisplay() | redraw

    try
      let key_text = a:0 > 0 ? 'key : '.a:1 : ''
      let filter_bak = self.filter
      let filter = copy(self.filter)
      let start = a:start
      let filter_new = ''
      while 1
	if start != ''
	  " use c= last char to force updating display etc
	  let filter_new = start[:-2]
	  let c = start[-1:]
	  let start = ''
	else
	  let c=getchar()
	endif 
	if index([13,10],c) >= 0
	  " c-j or return, accept new filter
	  let items = self.FilteredItems()
	  if len(items) == 1 && has_key(self, 'Continuation') && self.continueOnSingleMatchCR
	    call self.DoContinue(self.MapToOriginal(items[0]))
	  endif
	  return
	elseif index([27], c) >=0
	  " esc, abort
	  let self.filter = filter_bak
	  call self.UpdateDisplay()
	  return
	else
	  if type(c) == 0
	    let c = nr2char(c)
	  endif
	  if c == "\<bs>"
	    let filter_new = filter_new[:-2]
	  else
	    let filter_new .= c
	  endif
	  let d = {'keep' : a:keep, 'regex' : filter_new }
	  if a:0 > 0
	    let d['key'] = a:1
	  endif
	  let self.filter = copy(filter_bak)
	  call add(self.filter, d)
	  let items = self.FilteredItems()
	  if len(items) == 1 && has_key(self, 'Continuation') && self.continueOnSingleMatch
	    call self.DoContinue(self.MapToOriginal(items[0]))
	    return
	  endif
	  call self.UpdateDisplay() | redraw
	endif
      endwhile
    finally
      let self.modeText = ''
    endtry
  endfun

  if get(a:opts,'init',1)
    call d.NewBufferAndInit()
  endif
endfun
