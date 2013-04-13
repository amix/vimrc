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
function! zencoding#util#deleteContent(region)
  let lines = getline(a:region[0][0], a:region[1][0])
  call setpos('.', [0, a:region[0][0], a:region[0][1], 0])
  silent! exe "delete ".(a:region[1][0] - a:region[0][0])
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
function! zencoding#util#setContent(region, content)
  let newlines = split(a:content, '\n', 1)
  let oldlines = getline(a:region[0][0], a:region[1][0])
  call setpos('.', [0, a:region[0][0], a:region[0][1], 0])
  silent! exe "delete ".(a:region[1][0] - a:region[0][0])
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
function! zencoding#util#selectRegion(region)
  call setpos('.', [0, a:region[1][0], a:region[1][1], 0])
  normal! v
  call setpos('.', [0, a:region[0][0], a:region[0][1], 0])
endfunction

" point_in_region : check point is in the region
"   this function return 0 or 1
function! zencoding#util#pointInRegion(point, region)
  if !zencoding#util#regionIsValid(a:region) | return 0 | endif
  if a:region[0][0] > a:point[0] | return 0 | endif
  if a:region[1][0] < a:point[0] | return 0 | endif
  if a:region[0][0] == a:point[0] && a:region[0][1] > a:point[1] | return 0 | endif
  if a:region[1][0] == a:point[0] && a:region[1][1] < a:point[1] | return 0 | endif
  return 1
endfunction

" cursor_in_region : check cursor is in the region
"   this function return 0 or 1
function! zencoding#util#cursorInRegion(region)
  if !zencoding#util#regionIsValid(a:region) | return 0 | endif
  let cur = getpos('.')[1:2]
  return zencoding#util#pointInRegion(cur, a:region)
endfunction

" region_is_valid : check region is valid
"   this function return 0 or 1
function! zencoding#util#regionIsValid(region)
  if a:region[0][0] == 0 || a:region[1][0] == 0 | return 0 | endif
  return 1
endfunction

" search_region : make region from pattern which is composing start/end
"   this function return array of position
function! zencoding#util#searchRegion(start, end)
  return [searchpairpos(a:start, '', a:end, 'bcnW'), searchpairpos(a:start, '\%#', a:end, 'nW')]
endfunction

" get_content : get content in region
"   this function return string in region
function! zencoding#util#getContent(region)
  if !zencoding#util#regionIsValid(a:region)
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
function! zencoding#util#regionInRegion(outer, inner)
  if !zencoding#util#regionIsValid(a:inner) || !zencoding#util#regionIsValid(a:outer)
    return 0
  endif
  return zencoding#util#pointInRegion(a:inner[0], a:outer) && zencoding#util#pointInRegion(a:inner[1], a:outer)
endfunction

" get_visualblock : get region of visual block
"   this function return region of visual block
function! zencoding#util#getVisualBlock()
  return [[line("'<"), col("'<")], [line("'>"), col("'>")]]
endfunction

"==============================================================================
" html utils
"==============================================================================
function! zencoding#util#getContentFromURL(url)
  let res = system(printf("%s %s", g:zencoding_curl_command, shellescape(substitute(a:url, '#.*', '', ''))))
  let s1 = len(split(res, '?'))
  let utf8 = iconv(res, 'utf-8', &encoding)
  let s2 = len(split(utf8, '?'))
  return (s2 == s1 || s2 >= s1 * 2) ? utf8 : res
endfunction

function! zencoding#util#getTextFromHTML(buf)
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
    let str = substitute(str, '&apos;', "'", 'g')
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

function! zencoding#util#getImageSize(fn)
  let fn = a:fn

  if zencoding#util#isImageMagickInstalled()
    return zencoding#util#imageSizeWithImageMagick(fn)
  endif

  if filereadable(fn)
    let hex = substitute(system('xxd -p "'.fn.'"'), '\n', '', 'g')
  else
    let hex = substitute(system(g:zencoding_curl_command.' "'.fn.'" | xxd -p'), '\n', '', 'g')
  endif

  let [width, height] = [-1, -1]
  if hex =~ '^89504e470d0a1a0a'
    let width = eval('0x'.hex[32:39])
    let height = eval('0x'.hex[40:47])
  endif
  if hex =~ '^ffd8'
    let pos = 4
    while pos < len(hex)
      let bs = hex[pos+0:pos+3]
      let pos += 4
      if bs == 'ffc0' || bs == 'ffc2'
        let pos += 6
        let height = eval('0x'.hex[pos+0:pos+1])*256 + eval('0x'.hex[pos+2:pos+3])
        let pos += 4
        let width = eval('0x'.hex[pos+0:pos+1])*256 + eval('0x'.hex[pos+2:pos+3])
        break
      elseif bs =~ 'ffd[9a]'
        break
      elseif bs =~ 'ff\(e[0-9a-e]\|fe\|db\|dd\|c4\)'
        let pos += (eval('0x'.hex[pos+0:pos+1])*256 + eval('0x'.hex[pos+2:pos+3])) * 2
      endif
    endwhile
  endif
  if hex =~ '^47494638'
    let width = eval('0x'.hex[14:15].hex[12:13])
    let height = eval('0x'.hex[18:19].hex[16:17])
  endif

  return [width, height]
endfunction

function! zencoding#util#imageSizeWithImageMagick(fn)
  let img_info = system('identify -format "%wx%h" "'.a:fn.'"')
  let img_size = split(substitute(img_info, '\n', '', ''), 'x')
  let width = img_size[0]
  let height = img_size[1]
  return [width, height]
endfunction

function! zencoding#util#isImageMagickInstalled()
  if !get(s:, 'zencoding_use_identify', 1)
    return 0
  endif
  return executable('identify')
endfunction
