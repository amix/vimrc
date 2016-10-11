"==============================================================================
" region utils
"==============================================================================
" deleteContent : delete content in region
"   if region make from between '<foo>' and '</foo>'
"   --------------------
"   begin:<foo>
"   </foo>:end
"   --------------------
"   this function make the content as following
"   --------------------
"   begin::end
"   --------------------
function! emmet#util#deleteContent(region) abort
  let lines = getline(a:region[0][0], a:region[1][0])
  call setpos('.', [0, a:region[0][0], a:region[0][1], 0])
  silent! exe 'delete '.(a:region[1][0] - a:region[0][0])
  call setline(line('.'), lines[0][:a:region[0][1]-2] . lines[-1][a:region[1][1]])
endfunction

" change_content : change content in region
"   if region make from between '<foo>' and '</foo>'
"   --------------------
"   begin:<foo>
"   </foo>:end
"   --------------------
"   and content is
"   --------------------
"   foo
"   bar
"   baz
"   --------------------
"   this function make the content as following
"   --------------------
"   begin:foo
"   bar
"   baz:end
"   --------------------
function! emmet#util#setContent(region, content) abort
  let newlines = split(a:content, '\n', 1)
  let oldlines = getline(a:region[0][0], a:region[1][0])
  call setpos('.', [0, a:region[0][0], a:region[0][1], 0])
  silent! exe 'delete '.(a:region[1][0] - a:region[0][0])
  if len(newlines) == 0
    let tmp = ''
    if a:region[0][1] > 1
      let tmp = oldlines[0][:a:region[0][1]-2]
    endif
    if a:region[1][1] >= 1
      let tmp .= oldlines[-1][a:region[1][1]:]
    endif
    call setline(line('.'), tmp)
  elseif len(newlines) == 1
    if a:region[0][1] > 1
      let newlines[0] = oldlines[0][:a:region[0][1]-2] . newlines[0]
    endif
    if a:region[1][1] >= 1
      let newlines[0] .= oldlines[-1][a:region[1][1]:]
    endif
    call setline(line('.'), newlines[0])
  else
    if a:region[0][1] > 1
      let newlines[0] = oldlines[0][:a:region[0][1]-2] . newlines[0]
    endif
    if a:region[1][1] >= 1
      let newlines[-1] .= oldlines[-1][a:region[1][1]:]
    endif
    call setline(line('.'), newlines[0])
    call append(line('.'), newlines[1:])
  endif
endfunction

" select_region : select region
"   this function make a selection of region
function! emmet#util#selectRegion(region) abort
  call setpos('.', [0, a:region[1][0], a:region[1][1], 0])
  normal! v
  call setpos('.', [0, a:region[0][0], a:region[0][1], 0])
endfunction

" point_in_region : check point is in the region
"   this function return 0 or 1
function! emmet#util#pointInRegion(point, region) abort
  if !emmet#util#regionIsValid(a:region) | return 0 | endif
  if a:region[0][0] > a:point[0] | return 0 | endif
  if a:region[1][0] < a:point[0] | return 0 | endif
  if a:region[0][0] == a:point[0] && a:region[0][1] > a:point[1] | return 0 | endif
  if a:region[1][0] == a:point[0] && a:region[1][1] < a:point[1] | return 0 | endif
  return 1
endfunction

" cursor_in_region : check cursor is in the region
"   this function return 0 or 1
function! emmet#util#cursorInRegion(region) abort
  if !emmet#util#regionIsValid(a:region) | return 0 | endif
  let cur = emmet#util#getcurpos()[1:2]
  return emmet#util#pointInRegion(cur, a:region)
endfunction

" region_is_valid : check region is valid
"   this function return 0 or 1
function! emmet#util#regionIsValid(region) abort
  if a:region[0][0] == 0 || a:region[1][0] == 0 | return 0 | endif
  return 1
endfunction

" search_region : make region from pattern which is composing start/end
"   this function return array of position
function! emmet#util#searchRegion(start, end) abort
  let b = searchpairpos(a:start, '', a:end, 'bcnW')
  if b == [0, 0]
    return [searchpairpos(a:start, '', a:end, 'bnW'), searchpairpos(a:start, '\%#', a:end, 'nW')]
  else
    return [b, searchpairpos(a:start, '', a:end. '', 'nW')]
  endif
endfunction

" get_content : get content in region
"   this function return string in region
function! emmet#util#getContent(region) abort
  if !emmet#util#regionIsValid(a:region)
    return ''
  endif
  let lines = getline(a:region[0][0], a:region[1][0])
  if a:region[0][0] == a:region[1][0]
    let lines[0] = lines[0][a:region[0][1]-1:a:region[1][1]-1]
  else
    let lines[0] = lines[0][a:region[0][1]-1:]
    let lines[-1] = lines[-1][:a:region[1][1]-1]
  endif
  return join(lines, "\n")
endfunction

" region_in_region : check region is in the region
"   this function return 0 or 1
function! emmet#util#regionInRegion(outer, inner) abort
  if !emmet#util#regionIsValid(a:inner) || !emmet#util#regionIsValid(a:outer)
    return 0
  endif
  return emmet#util#pointInRegion(a:inner[0], a:outer) && emmet#util#pointInRegion(a:inner[1], a:outer)
endfunction

" get_visualblock : get region of visual block
"   this function return region of visual block
function! emmet#util#getVisualBlock() abort
  return [[line("'<"), col("'<")], [line("'>"), col("'>")]]
endfunction

"==============================================================================
" html utils
"==============================================================================
function! emmet#util#getContentFromURL(url) abort
  let res = system(printf('%s -i %s', g:emmet_curl_command, shellescape(substitute(a:url, '#.*', '', ''))))
  while res =~# '^HTTP/1.\d 3' || res =~# '^HTTP/1\.\d 200 Connection established' || res =~# '^HTTP/1\.\d 100 Continue'
    let pos = stridx(res, "\r\n\r\n")
    if pos != -1
      let res = strpart(res, pos+4)
    else
      let pos = stridx(res, "\n\n")
      let res = strpart(res, pos+2)
    endif
  endwhile
  let pos = stridx(res, "\r\n\r\n")
  if pos != -1
    let content = strpart(res, pos+4)
  else
    let pos = stridx(res, "\n\n")
    let content = strpart(res, pos+2)
  endif
  let header = res[:pos-1]
  let charset = matchstr(content, '<meta[^>]\+content=["''][^;"'']\+;\s*charset=\zs[^;"'']\+\ze["''][^>]*>')
  if len(charset) == 0
    let charset = matchstr(content, '<meta\s\+charset=["'']\?\zs[^"'']\+\ze["'']\?[^>]*>')
  endif
  if len(charset) == 0
    let charset = matchstr(header, '\nContent-Type:.* charset=[''"]\?\zs[^''";\n]\+\ze')
  endif
  if len(charset) == 0
    let s1 = len(split(content, '?'))
    let utf8 = iconv(content, 'utf-8', &encoding)
    let s2 = len(split(utf8, '?'))
    return (s2 == s1 || s2 >= s1 * 2) ? utf8 : content
  endif
  return iconv(content, charset, &encoding)
endfunction

function! emmet#util#getTextFromHTML(buf) abort
  let threshold_len = 100
  let threshold_per = 0.1
  let buf = a:buf

  let buf = strpart(buf, stridx(buf, '</head>'))
  let buf = substitute(buf, '<style[^>]*>.\{-}</style>', '', 'g')
  let buf = substitute(buf, '<script[^>]*>.\{-}</script>', '', 'g')
  let res = ''
  let max = 0
  let mx = '\(<td[^>]\{-}>\)\|\(<\/td>\)\|\(<div[^>]\{-}>\)\|\(<\/div>\)'
  let m = split(buf, mx)
  for str in m
    let c = split(str, '<[^>]*?>')
    let str = substitute(str, '<[^>]\{-}>', ' ', 'g')
    let str = substitute(str, '&gt;', '>', 'g')
    let str = substitute(str, '&lt;', '<', 'g')
    let str = substitute(str, '&quot;', '"', 'g')
    let str = substitute(str, '&apos;', '''', 'g')
    let str = substitute(str, '&nbsp;', ' ', 'g')
    let str = substitute(str, '&yen;', '\&#65509;', 'g')
    let str = substitute(str, '&amp;', '\&', 'g')
    let str = substitute(str, '^\s*\(.*\)\s*$', '\1', '')
    let str = substitute(str, '\s\+', ' ', 'g')
    let l = len(str)
    if l > threshold_len
      let per = (l+0.0) / len(c)
      if max < l && per > threshold_per
        let max = l
        let res = str
      endif
    endif
  endfor
  let res = substitute(res, '^\s*\(.*\)\s*$', '\1', 'g')
  return res
endfunction

function! emmet#util#getImageSize(fn) abort
  let fn = a:fn

  if emmet#util#isImageMagickInstalled()
    return emmet#util#imageSizeWithImageMagick(fn)
  endif

  if filereadable(fn)
    let hex = substitute(system('xxd -p "'.fn.'"'), '\n', '', 'g')
  else
    if fn !~# '^\w\+://'
      let path = fnamemodify(expand('%'), ':p:gs?\\?/?')
      if has('win32') || has('win64') | 
        let path = tolower(path)
      endif
      for k in keys(g:emmet_docroot)
        let root = fnamemodify(k, ':p:gs?\\?/?')
        if has('win32') || has('win64') | 
          let root = tolower(root)
        endif
        if stridx(path, root) == 0
          let v = g:emmet_docroot[k]
          let fn = (len(v) == 0 ? k : v) . fn
          break
        endif
      endfor
    endif
    let hex = substitute(system(g:emmet_curl_command.' "'.fn.'" | xxd -p'), '\n', '', 'g')
  endif

  let [width, height] = [-1, -1]
  if hex =~# '^89504e470d0a1a0a'
    let width = eval('0x'.hex[32:39])
    let height = eval('0x'.hex[40:47])
  endif
  if hex =~# '^ffd8'
    let pos = 4
    while pos < len(hex)
      let bs = hex[pos+0:pos+3]
      let pos += 4
      if bs ==# 'ffc0' || bs ==# 'ffc2'
        let pos += 6
        let height = eval('0x'.hex[pos+0:pos+1])*256 + eval('0x'.hex[pos+2:pos+3])
        let pos += 4
        let width = eval('0x'.hex[pos+0:pos+1])*256 + eval('0x'.hex[pos+2:pos+3])
        break
      elseif bs =~# 'ffd[9a]'
        break
      elseif bs =~# 'ff\(e[0-9a-e]\|fe\|db\|dd\|c4\)'
        let pos += (eval('0x'.hex[pos+0:pos+1])*256 + eval('0x'.hex[pos+2:pos+3])) * 2
      endif
    endwhile
  endif
  if hex =~# '^47494638'
    let width = eval('0x'.hex[14:15].hex[12:13])
    let height = eval('0x'.hex[18:19].hex[16:17])
  endif

  return [width, height]
endfunction

function! emmet#util#imageSizeWithImageMagick(fn) abort
  let img_info = system('identify -format "%wx%h" "'.a:fn.'"')
  let img_size = split(substitute(img_info, '\n', '', ''), 'x')
  if len(img_size) != 2
    return [-1, -1]
  endif
  return img_size
endfunction

function! emmet#util#isImageMagickInstalled() abort
  if !get(g:, 'emmet_use_identify', 1)
    return 0
  endif
  return executable('identify')
endfunction

function! emmet#util#unique(arr) abort
  let m = {}
  let r = []
  for i in a:arr
    if !has_key(m, i)
      let m[i] = 1
      call add(r, i)
    endif
  endfor
  return r
endfunction

let s:seed = localtime()
function! emmet#util#srand(seed) abort
  let s:seed = a:seed
endfunction

function! emmet#util#rand() abort
  let s:seed = s:seed * 214013 + 2531011
  return (s:seed < 0 ? s:seed - 0x80000000 : s:seed) / 0x10000 % 0x8000
endfunction

function! emmet#util#cache(name, ...) abort
  let content = get(a:000, 0, '')
  let dir = expand('~/.emmet/cache')
  if !isdirectory(dir)
    call mkdir(dir, 'p', 0700)
  endif
  let file = dir . '/' . substitute(a:name, '\W', '_', 'g')
  if len(content) == 0
    if !filereadable(file)
      return ''
    endif
	return join(readfile(file), "\n")
  endif
  call writefile(split(content, "\n"), file)
endfunction

function! emmet#util#getcurpos() abort
  let pos = getpos('.')
  if mode(0) ==# 'i' && pos[2] > 0
    let pos[2] -=1
  endif
  return pos
endfunction

function! emmet#util#closePopup() abort
  return pumvisible() ? "\<c-e>" : ''
endfunction
