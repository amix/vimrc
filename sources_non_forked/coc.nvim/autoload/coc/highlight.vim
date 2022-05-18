scriptencoding utf-8
let s:is_vim = !has('nvim')
let s:clear_match_by_window = has('nvim-0.6.0') || has('patch-8.1.1084')
let s:set_extmark = has('nvim') && exists('*nvim_buf_set_extmark')
let s:del_extmark = has('nvim') && exists('*nvim_buf_del_extmark')
let s:prop_offset = get(g:, 'coc_text_prop_offset', 1000)
let s:namespace_map = {}
let s:ns_id = 1
let s:diagnostic_hlgroups = ['CocErrorHighlight', 'CocWarningHighlight', 'CocInfoHighlight', 'CocHintHighlight', 'CocDeprecatedHighlight', 'CocUnusedHighlight']
" Maximum count to highlight each time.
let g:coc_highlight_maximum_count = get(g:, 'coc_highlight_maximum_count', 100)

if has('nvim-0.5.0') && s:clear_match_by_window == 0
  try
    call getmatches(0)
    let s:clear_match_by_window = 1
  catch /^Vim\%((\a\+)\)\=:E118/
    " ignored
  endtry
endif

" Update buffer region by region.
function! coc#highlight#buffer_update(bufnr, key, highlights, ...) abort
  if !bufloaded(a:bufnr)
    return
  endif
  if empty(a:highlights)
    call coc#highlight#clear_highlight(a:bufnr, a:key, 0, -1)
    return
  endif
  let priority = get(a:, 1, v:null)
  let changedtick = getbufvar(a:bufnr, 'changedtick', 0)
  if type(get(a:, 2, v:null)) == 0 && changedtick > a:2
    return
  endif
  let hls = map(copy(a:highlights), "{'hlGroup':v:val[0],'lnum':v:val[1],'colStart':v:val[2],'colEnd':v:val[3],'combine':get(v:val,4,1),'start_incl':get(v:val,5,0),'end_incl':get(v:val,6,0)}")
  if len(hls) <= g:coc_highlight_maximum_count || get(g:, 'coc_node_env', '') ==# 'test'
    call coc#highlight#update_highlights(a:bufnr, a:key, hls, 0, -1, priority)
    return
  endif
  let linecount = coc#compat#buf_line_count(a:bufnr)
  let groups = s:group_hls(hls, linecount)
  call s:update_highlights_timer(a:bufnr, changedtick, a:key, priority, groups, 0)
endfunction

" Update highlights by check exists highlights.
" 0 based, end exclusive start and end
function! coc#highlight#update_highlights(bufnr, key, highlights, ...) abort
  let bufnr = a:bufnr == 0 ? bufnr('%') : a:bufnr
  if !bufloaded(bufnr)
    return
  endif
  let start = get(a:, 1, 0)
  let end = get(a:, 2, -1)
  if end == 0
    return
  endif
  let linecount = coc#compat#buf_line_count(a:bufnr)
  if end >= linecount
    let end = -1
  endif
  if empty(a:highlights)
    call coc#highlight#clear_highlight(bufnr, a:key, start, end)
    return
  endif
  let priority = get(a:, 3, v:null)
  let total = len(a:highlights)
  " index list that exists with current highlights
  let exists = []
  let ns = coc#highlight#create_namespace(a:key)
  if has('nvim-0.5.0') || exists('*prop_list')
    let endLnum = end < 0 ? linecount - 1 : end - 1
    let firstLnum = a:highlights[0]['lnum']
    if firstLnum > start
      call coc#highlight#clear_highlight(bufnr, a:key, start, firstLnum)
      let start = firstLnum
    endif
    let lastLnum = a:highlights[total - 1]['lnum']
    if lastLnum < endLnum
      call coc#highlight#clear_highlight(bufnr, a:key, lastLnum + 1, endLnum + 1)
      let endLnum = lastLnum
    endif
    let current = coc#highlight#get_highlights(bufnr, a:key, start, endLnum)
    let currIndex = 0
    let clearLnums = []
    if !empty(current)
      for [lnum, items] in s:to_group(current)
        let indexes = []
        let currIndexes = range(0, len(items) - 1)
        "call coc#rpc#notify('Log', ['items:', lnum, items])
        while currIndex != total
          let hi = a:highlights[currIndex]
          if hi['lnum'] == lnum
            for idx in currIndexes
              let item = items[idx]
              if hi['hlGroup'] ==# item[0] && hi['colStart'] == item[2] && hi['colEnd'] == item[3]
                call add(indexes, currIndex)
                call filter(currIndexes, 'v:val != '.idx)
                break
              elseif item[2] > hi['colStart']
                break
              endif
            endfor
          elseif hi['lnum'] > lnum
            break
          endif
          let currIndex = currIndex + 1
        endwhile
        if !empty(currIndexes)
          if s:del_extmark
            for idx in currIndexes
              call nvim_buf_del_extmark(bufnr, ns, items[idx][4])
            endfor
            call extend(exists, indexes)
          else
            call add(clearLnums, lnum)
          endif
        else
          " all highlights of current line exists, not clear.
          call extend(exists, indexes)
        endif
      endfor
    endif
    call coc#highlight#clear(bufnr, a:key, clearLnums)
  else
    call coc#highlight#clear_highlight(bufnr, a:key, start, end)
  endif
  let indexes = range(0, total - 1)
  if !empty(exists)
    let indexes = filter(indexes, 'index(exists, v:val) == -1')
  endif
  for idx in indexes
    let hi = a:highlights[idx]
    let opts = {}
    if type(priority) == 0
      let opts['priority'] = s:get_priority(a:key, hi['hlGroup'], priority)
    endif
    for key in ['combine', 'start_incl', 'end_incl']
      if has_key(hi, key)
        let opts[key] = hi[key]
      endif
    endfor
    call coc#highlight#add_highlight(bufnr, ns, hi['hlGroup'], hi['lnum'], hi['colStart'], hi['colEnd'], opts)
  endfor
endfunction

" 0 based line, start_col and end_col
" 0 based start & end line, end inclusive.
function! coc#highlight#get_highlights(bufnr, key, ...) abort
  if !bufloaded(a:bufnr)
    return v:null
  endif
  if !has_key(s:namespace_map, a:key)
    return []
  endif
  let start = get(a:, 1, 0)
  let end = get(a:, 2, -1)
  let res = []
  let ns = s:namespace_map[a:key]
  if exists('*prop_list')
    " Could filter by end_lnum and ids
    if has('patch-8.2.3652')
      let endLnum = end == -1 ? -1 : end + 1
      for prop in prop_list(start + 1, {'bufnr': a:bufnr, 'ids': [s:prop_offset + ns], 'end_lnum': endLnum})
        if prop['start'] == 0 || prop['end'] == 0
          " multi line textprop are not supported, simply ignore it
          continue
        endif
        let startCol = prop['col'] - 1
        let endCol = startCol + prop['length']
        call add(res, [s:prop_type_hlgroup(prop['type']), prop['lnum'] - 1, startCol, endCol])
      endfor
    else
      if end == -1
        let end = coc#compat#buf_line_count(a:bufnr)
      else
        let end = end + 1
      endif
      let id = s:prop_offset + ns
      for line in range(start + 1, end)
        for prop in prop_list(line, {'bufnr': a:bufnr})
          if prop['id'] != id || prop['start'] == 0 || prop['end'] == 0
            " multi line textprop are not supported, simply ignore it
            continue
          endif
          let startCol = prop['col'] - 1
          let endCol = startCol + prop['length']
          call add(res, [s:prop_type_hlgroup(prop['type']), line - 1, startCol, endCol])
        endfor
      endfor
    endif
  elseif has('nvim-0.5.0')
    let start = [start, 0]
    let maximum = end == -1 ? nvim_buf_line_count(a:bufnr) : end + 1
    let end = end == -1 ? -1 : [end + 1, 0]
    let markers = nvim_buf_get_extmarks(a:bufnr, ns, start, -1, {'details': v:true})
    for [marker_id, line, start_col, details] in markers
      if line >= maximum
        " Could be markers exceed end of line
        continue
      endif
      let delta = details['end_row'] - line
      if delta > 1 || (delta == 1 && details['end_col'] != 0)
        " can't handle, single line only
        continue
      endif
      let endCol = details['end_col']
      if endCol == start_col
        call nvim_buf_del_extmark(a:bufnr, ns, marker_id)
        continue
      endif
      if delta == 1
        let text = get(nvim_buf_get_lines(a:bufnr, line, line + 1, 0), 0, '')
        let endCol = strlen(text)
      endif
      call add(res, [details['hl_group'], line, start_col, endCol, marker_id])
    endfor
  else
    throw 'Get highlights requires neovim 0.5.0 or vim support prop_list'
  endif
  return res
endfunction

" Add multiple highlights to buffer.
" type HighlightItem = [hlGroup, lnum, colStart, colEnd, combine?, start_incl?, end_incl?]
function! coc#highlight#set(bufnr, key, highlights, priority) abort
  if !bufloaded(a:bufnr)
    return
  endif
    let ns = coc#highlight#create_namespace(a:key)
    if len(a:highlights) > g:coc_highlight_maximum_count
      call s:add_highlights_timer(a:bufnr, ns, a:highlights, a:priority)
    else
      call s:add_highlights(a:bufnr, ns, a:highlights, a:priority)
    endif
endfunction

" Clear highlights by 0 based line numbers.
function! coc#highlight#clear(bufnr, key, lnums) abort
  if !bufloaded(a:bufnr)
    return
  endif
  let ns = coc#highlight#create_namespace(a:key)
  for lnum in a:lnums
    if has('nvim')
      call nvim_buf_clear_namespace(a:bufnr, ns, lnum, lnum + 1)
    else
      call coc#api#call('buf_clear_namespace', [a:bufnr, ns, lnum, lnum + 1])
    endif
  endfor
  " clear highlights in invalid line.
  if has('nvim')
    let linecount = nvim_buf_line_count(a:bufnr)
    call nvim_buf_clear_namespace(a:bufnr, ns, linecount, -1)
  endif
endfunction

function! coc#highlight#del_markers(bufnr, key, ids) abort
  if !bufloaded(a:bufnr)
    return
  endif
  let ns = coc#highlight#create_namespace(a:key)
  for id in a:ids
    call nvim_buf_del_extmark(a:bufnr, ns, id)
  endfor
endfunction

" highlight LSP range,
function! coc#highlight#ranges(bufnr, key, hlGroup, ranges, ...) abort
  let bufnr = a:bufnr == 0 ? bufnr('%') : a:bufnr
  if !bufloaded(bufnr) || !exists('*getbufline')
    return
  endif
  let opts = get(a:, 1, {})
  let synmaxcol = getbufvar(a:bufnr, '&synmaxcol', 1000)
  if synmaxcol == 0
    let synmaxcol = 1000
  endif
  let synmaxcol = min([synmaxcol, 1000])
  let srcId = coc#highlight#create_namespace(a:key)
  for range in a:ranges
    let start = range['start']
    let end = range['end']
    for lnum in range(start['line'] + 1, end['line'] + 1)
      let arr = getbufline(bufnr, lnum)
      let line = empty(arr) ? '' : arr[0]
      if empty(line)
        continue
      endif
      if start['character'] > synmaxcol || end['character'] > synmaxcol
        continue
      endif
      " TODO don't know how to count UTF16 code point, should work most cases.
      let colStart = lnum == start['line'] + 1 ? strlen(strcharpart(line, 0, start['character'])) : 0
      let colEnd = lnum == end['line'] + 1 ? strlen(strcharpart(line, 0, end['character'])) : strlen(line)
      if colStart == colEnd
        continue
      endif
      call coc#highlight#add_highlight(bufnr, srcId, a:hlGroup, lnum - 1, colStart, colEnd, opts)
    endfor
  endfor
endfunction

function! coc#highlight#add_highlight(bufnr, src_id, hl_group, line, col_start, col_end, ...) abort
  let opts = get(a:, 1, {})
  let priority = get(opts, 'priority', v:null)
  if has('nvim')
    if s:set_extmark && a:src_id != -1
      " get(opts, 'start_incl', 0) ? v:true : v:false,
      try
        call nvim_buf_set_extmark(a:bufnr, a:src_id, a:line, a:col_start, {
              \ 'end_col': a:col_end,
              \ 'hl_group': a:hl_group,
              \ 'hl_mode': get(opts, 'combine', 1) ? 'combine' : 'replace',
              \ 'right_gravity': v:true,
              \ 'end_right_gravity': v:false,
              \ 'priority': type(priority) == 0 ?  min([priority, 4096]) : 4096,
              \ })
      catch /^Vim\%((\a\+)\)\=:E5555/
        " the end_col could be invalid, ignore this error
      endtry
    else
      call nvim_buf_add_highlight(a:bufnr, a:src_id, a:hl_group, a:line, a:col_start, a:col_end)
    endif
  else
    call coc#api#call('buf_add_highlight', [a:bufnr, a:src_id, a:hl_group, a:line, a:col_start, a:col_end, opts])
  endif
endfunction

function! coc#highlight#clear_highlight(bufnr, key, start_line, end_line) abort
  let bufnr = a:bufnr == 0 ? bufnr('%') : a:bufnr
  if !bufloaded(bufnr)
    return
  endif
  let src_id = coc#highlight#create_namespace(a:key)
  if has('nvim')
    call nvim_buf_clear_namespace(a:bufnr, src_id, a:start_line, a:end_line)
  else
    call coc#api#call('buf_clear_namespace', [a:bufnr, src_id, a:start_line, a:end_line])
  endif
endfunction

" highlight buffer in winid with CodeBlock &HighlightItems
" export interface HighlightItem {
"   lnum: number // 0 based
"   hlGroup: string
"   colStart: number // 0 based
"   colEnd: number
" }
" export interface CodeBlock {
"   filetype?: string
"   hlGroup?: string
"   startLine: number // 0 based
"   endLine: number
" }
function! coc#highlight#add_highlights(winid, codes, highlights) abort
  " clear highlights
  call coc#compat#execute(a:winid, 'syntax clear')
  let bufnr = winbufnr(a:winid)
  call coc#highlight#clear_highlight(bufnr, -1, 0, -1)
  if !empty(a:codes)
    call coc#highlight#highlight_lines(a:winid, a:codes)
  endif
  if !empty(a:highlights)
    for item in a:highlights
      call coc#highlight#add_highlight(bufnr, -1, item['hlGroup'], item['lnum'], item['colStart'], item['colEnd'])
    endfor
  endif
endfunction


" Add highlights to line groups of winid, support hlGroup and filetype
" config should have startLine, endLine (0 based, end excluded) and filetype or hlGroup
" endLine should > startLine and endLine is excluded
"
" export interface CodeBlock {
"   filetype?: string
"   hlGroup?: string
"   startLine: number // 0 based
"   endLine: number
" }
function! coc#highlight#highlight_lines(winid, blocks) abort
  let region_id = 1
  let defined = []
  let cmds = []
  for config in a:blocks
    let start = config['startLine'] + 1
    let end = config['endLine'] == -1 ? len(getbufline(winbufnr(a:winid), 1, '$')) + 1 : config['endLine'] + 1
    let filetype = get(config, 'filetype', '')
    let hlGroup = get(config, 'hlGroup', '')
    if !empty(hlGroup)
      call add(cmds, 'syntax region '.hlGroup.' start=/\%'.start.'l/ end=/\%'.end.'l/')
    else
      let filetype = matchstr(filetype, '\v^\w+')
      if empty(filetype) || filetype == 'txt' || index(get(g:, 'coc_markdown_disabled_languages', []), filetype) != -1
        continue
      endif
      if index(defined, filetype) == -1
        call add(cmds, 'syntax include @'.toupper(filetype).' syntax/'.filetype.'.vim')
        call add(cmds, 'unlet! b:current_syntax')
        call add(defined, filetype)
      endif
      call add(cmds, 'syntax region CodeBlock'.region_id.' start=/\%'.start.'l/ end=/\%'.end.'l/ contains=@'.toupper(filetype).' keepend')
      let region_id = region_id + 1
    endif
  endfor
  if !empty(cmds)
    call coc#compat#execute(a:winid, cmds, 'silent!')
  endif
endfunction

" Compose hlGroups with foreground and background colors.
function! coc#highlight#compose_hlgroup(fgGroup, bgGroup) abort
  let hlGroup = 'Fg'.a:fgGroup.'Bg'.a:bgGroup
  if a:fgGroup ==# a:bgGroup
    return a:fgGroup
  endif
  if hlexists(hlGroup)
    return hlGroup
  endif
  let fgId = synIDtrans(hlID(a:fgGroup))
  let bgId = synIDtrans(hlID(a:bgGroup))
  let isGuiReversed = synIDattr(fgId, 'reverse', 'gui') !=# '1' || synIDattr(bgId, 'reverse', 'gui') !=# '1'
  let guifg = isGuiReversed ? synIDattr(fgId, 'fg', 'gui') : synIDattr(fgId, 'bg', 'gui')
  let guibg = isGuiReversed ? synIDattr(bgId, 'bg', 'gui') : synIDattr(bgId, 'fg', 'gui')
  let isCtermReversed = synIDattr(fgId, 'reverse', 'cterm') !=# '1' || synIDattr(bgId, 'reverse', 'cterm') !=# '1'
  let ctermfg = isCtermReversed ? synIDattr(fgId, 'fg', 'cterm') : synIDattr(fgId, 'bg', 'cterm')
  let ctermbg = isCtermReversed ? synIDattr(bgId, 'bg', 'cterm') : synIDattr(bgId, 'fg', 'cterm')
  let bold = synIDattr(fgId, 'bold') ==# '1'
  let italic = synIDattr(fgId, 'italic') ==# '1'
  let underline = synIDattr(fgId, 'underline') ==# '1'
  let cmd = 'silent hi ' . hlGroup
  if !empty(guifg)
    let cmd .= ' guifg=' . guifg
  endif
  if !empty(ctermfg)
    let cmd .= ' ctermfg=' . ctermfg
  elseif guifg =~# '^#'
    let cmd .= ' ctermfg=' . coc#color#rgb2term(strpart(guifg, 1))
  endif
  if !empty(guibg)
    let cmd .= ' guibg=' . guibg
  endif
  if !empty(ctermbg)
    let cmd .= ' ctermbg=' . ctermbg
  elseif guibg =~# '^#'
    let cmd .= ' ctermbg=' . coc#color#rgb2term(strpart(guibg, 1))
  endif
  if bold
    let cmd .= ' cterm=bold gui=bold'
  elseif italic
    let cmd .= ' cterm=italic gui=italic'
  elseif underline
    let cmd .= ' cterm=underline gui=underline'
  endif
  if cmd ==# 'silent hi ' . hlGroup
      return 'Normal'
  endif
  execute cmd
  return hlGroup
endfunction

" add matches for winid, use 0 for current window.
function! coc#highlight#match_ranges(winid, bufnr, ranges, hlGroup, priority) abort
  let winid = a:winid == 0 ? win_getid() : a:winid
  let bufnr = a:bufnr == 0 ? winbufnr(winid) : a:bufnr
  if empty(getwininfo(winid)) || (a:bufnr != 0 && winbufnr(a:winid) != a:bufnr)
    " not valid
    return []
  endif
  if !s:clear_match_by_window
    let curr = win_getid()
    if has('nvim')
      noa call nvim_set_current_win(winid)
    else
      noa call win_gotoid(winid)
    endif
  endif
  let ids = []
  for range in a:ranges
    let pos = []
    let start = range['start']
    let end = range['end']
    for lnum in range(start['line'] + 1, end['line'] + 1)
      let arr = getbufline(bufnr, lnum)
      let line = empty(arr) ? '' : arr[0]
      if empty(line)
        continue
      endif
      let colStart = lnum == start['line'] + 1 ? strlen(strcharpart(line, 0, start['character'])) + 1 : 1
      let colEnd = lnum == end['line'] + 1 ? strlen(strcharpart(line, 0, end['character'])) + 1 : strlen(line) + 1
      if colStart == colEnd
        continue
      endif
      call add(pos, [lnum, colStart, colEnd - colStart])
    endfor
    if !empty(pos)
      let opts = s:clear_match_by_window ? {'window': a:winid} : {}
      let i = 1
      let l = []
      for p in pos
        call add(l, p)
        if i % 8 == 0
          let id = matchaddpos(a:hlGroup, l, a:priority, -1, opts)
          call add(ids, id)
          let l = []
        endif
        let i += 1
      endfor
      if !empty(l)
        let id = matchaddpos(a:hlGroup, l, a:priority, -1, opts)
        call add(ids, id)
      endif
    endif
  endfor
  if !s:clear_match_by_window
    if has('nvim')
      noa call nvim_set_current_win(curr)
    else
      noa call win_gotoid(curr)
    endif
  endif
  return ids
endfunction

" Clear matches by hlGroup regexp.
function! coc#highlight#clear_match_group(winid, match) abort
  let winid = a:winid == 0 ? win_getid() : a:winid
  if empty(getwininfo(winid))
    " not valid
    return
  endif
  if s:clear_match_by_window
    let arr = filter(getmatches(winid), 'v:val["group"] =~# "'.a:match.'"')
    for item in arr
      call matchdelete(item['id'], winid)
    endfor
  else
    let curr = win_getid()
    let switch = exists('*nvim_set_current_win') && curr != winid
    if switch
      noa call nvim_set_current_win(a:winid)
    endif
    if win_getid() == winid
      let arr = filter(getmatches(), 'v:val["group"] =~# "'.a:match.'"')
      for item in arr
        call matchdelete(item['id'])
      endfor
    endif
    if switch
      noa call nvim_set_current_win(curr)
    endif
  endif
endfunction

" Clear matches by match ids, use 0 for current win.
function! coc#highlight#clear_matches(winid, ids)
  let winid = a:winid == 0 ? win_getid() : a:winid
  if empty(getwininfo(winid))
    " not valid
    return
  endif
  if s:clear_match_by_window
    for id in a:ids
      try
        call matchdelete(id, winid)
      catch /^Vim\%((\a\+)\)\=:E803/
        " ignore
      endtry
    endfor
  else
    let curr = win_getid()
    let switch = exists('*nvim_set_current_win') && curr != winid
    if switch
      noa call nvim_set_current_win(a:winid)
    endif
    if win_getid() == winid
      for id in a:ids
        try
          call matchdelete(id)
        catch /^Vim\%((\a\+)\)\=:E803/
          " ignore
        endtry
      endfor
    endif
    if switch
      noa call nvim_set_current_win(curr)
    endif
  endif
endfunction

function! coc#highlight#clear_all() abort
  for src_id in values(s:namespace_map)
    for bufnr in map(getbufinfo({'bufloaded': 1}), 'v:val["bufnr"]')
      if has('nvim')
        call nvim_buf_clear_namespace(bufnr, src_id, 0, -1)
      else
        call coc#api#call('buf_clear_namespace', [bufnr, src_id, 0, -1])
      endif
    endfor
  endfor
endfunction

function! coc#highlight#create_namespace(key) abort
  if type(a:key) == 0
    return a:key
  endif
  if has_key(s:namespace_map, a:key)
    return s:namespace_map[a:key]
  endif
  if has('nvim')
    let s:namespace_map[a:key] = nvim_create_namespace('coc-'.a:key)
  else
    let s:namespace_map[a:key] = s:ns_id
    let s:ns_id = s:ns_id + 1
  endif
  return s:namespace_map[a:key]
endfunction

function! coc#highlight#get_syntax_name(lnum, col)
  return synIDattr(synIDtrans(synID(a:lnum,a:col,1)),"name")
endfunction

function! s:prop_type_hlgroup(type) abort
  if strpart(a:type, 0, 12) ==# 'CocHighlight'
    return strpart(a:type, 12)
  endif
  return get(prop_type_get(a:type), 'highlight', '')
endfunction

function! s:update_highlights_timer(bufnr, changedtick, key, priority, groups, idx) abort
  if getbufvar(a:bufnr, 'changedtick', 0) != a:changedtick
    return
  endif
  let group = get(a:groups, a:idx, v:null)
  if empty(group)
    return
  endif
  if empty(group['highlights'])
    call coc#highlight#clear_highlight(a:bufnr, a:key, group['start'], group['end'])
  else
    call coc#highlight#update_highlights(a:bufnr, a:key, group['highlights'], group['start'], group['end'], a:priority)
  endif
  if a:idx < len(a:groups) - 1
    call timer_start(50, { -> s:update_highlights_timer(a:bufnr, a:changedtick, a:key, a:priority, a:groups, a:idx + 1)})
  endif
endfunction

function! s:add_highlights_timer(bufnr, ns, highlights, priority) abort
  let hls = []
  let next = []
  for i in range(0, len(a:highlights) - 1)
    if i < g:coc_highlight_maximum_count
      call add(hls, a:highlights[i])
    else
      call add(next, a:highlights[i])
    endif
  endfor
  call s:add_highlights(a:bufnr, a:ns, hls, a:priority)
  if len(next)
    call timer_start(30, {->s:add_highlights_timer(a:bufnr, a:ns, next, a:priority)})
  endif
endfunction

function! s:add_highlights(bufnr, ns, highlights, priority) abort
  for item in a:highlights
    let opts = {
          \ 'priority': a:priority,
          \ 'combine': get(item, 4, 1) ? 1 : 0,
          \ 'start_incl': get(item, 5, 0) ? 1 : 0,
          \ 'end_incl':  get(item, 6, 0) ? 1 : 0,
          \ }
    call coc#highlight#add_highlight(a:bufnr, a:ns, item[0], item[1], item[2], item[3], opts)
  endfor
endfunction

function! s:to_group(items) abort
  let res = []
  let before = v:null
  for item in a:items
    if empty(before) || before[0] != item[1]
      let before = [item[1], [item]]
      call add(res, before)
    else
      call add(before[1], item)
    endif
  endfor
  return res
endfunction

function! s:get_priority(key, hlGroup, priority) abort
  if a:hlGroup ==# 'Search'
    return 999
  endif
  if strpart(a:key, 0, 10) !=# 'diagnostic'
    return a:priority
  endif
  return a:priority - index(s:diagnostic_hlgroups, a:hlGroup)
endfunction

function! s:group_hls(hls, linecount) abort
  " start, end, highlights
  let groups = []
  if empty(a:hls)
    call add(groups, {'start': 0, 'end': a:linecount, 'highlights': []})
    return groups
  endif
  let start = 0
  let highlights = []
  let lastLnum = -1
  for item in a:hls
    let lnum = item['lnum']
    if lnum >= a:linecount
      break
    endif
    if len(highlights) < g:coc_highlight_maximum_count || lnum == lastLnum
      call add(highlights, item)
      let lastLnum = lnum
    else
      call add(groups, {'start': start, 'end': lastLnum + 1, 'highlights': highlights})
      let highlights = []
      let start = lastLnum + 1
      call add(highlights, item)
      let lastLnum = lnum
    endif
  endfor
  call add(groups, {'start': start, 'end': a:linecount, 'highlights': highlights})
  return groups
endfunction
