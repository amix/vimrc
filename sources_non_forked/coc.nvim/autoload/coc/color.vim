scriptencoding utf-8

let s:activate = ""
let s:quit = ""
if has("gui_macvim") && has('gui_running')
  let s:app = "MacVim"
elseif $TERM_PROGRAM ==# "Apple_Terminal"
  let s:app = "Terminal"
elseif $TERM_PROGRAM ==# "iTerm.app"
  let s:app = "iTerm2"
elseif has('mac')
  let s:app = "System Events"
  let s:quit = "quit"
  let s:activate = 'activate'
endif

" Returns an approximate grey index for the given grey level
fun! s:grey_number(x)
  if &t_Co == 88
    if a:x < 23
      return 0
    elseif a:x < 69
      return 1
    elseif a:x < 103
      return 2
    elseif a:x < 127
      return 3
    elseif a:x < 150
      return 4
    elseif a:x < 173
      return 5
    elseif a:x < 196
      return 6
    elseif a:x < 219
      return 7
    elseif a:x < 243
      return 8
    else
      return 9
    endif
  else
    if a:x < 14
      return 0
    else
      let l:n = (a:x - 8) / 10
      let l:m = (a:x - 8) % 10
      if l:m < 5
        return l:n
      else
        return l:n + 1
      endif
    endif
  endif
endfun

" Returns the actual grey level represented by the grey index
fun! s:grey_level(n)
  if &t_Co == 88
    if a:n == 0
      return 0
    elseif a:n == 1
      return 46
    elseif a:n == 2
      return 92
    elseif a:n == 3
      return 115
    elseif a:n == 4
      return 139
    elseif a:n == 5
      return 162
    elseif a:n == 6
      return 185
    elseif a:n == 7
      return 208
    elseif a:n == 8
      return 231
    else
      return 255
    endif
  else
    if a:n == 0
      return 0
    else
      return 8 + (a:n * 10)
    endif
  endif
endfun

" Returns the palette index for the given grey index
fun! s:grey_colour(n)
  if &t_Co == 88
    if a:n == 0
      return 16
    elseif a:n == 9
      return 79
    else
      return 79 + a:n
    endif
  else
    if a:n == 0
      return 16
    elseif a:n == 25
      return 231
    else
      return 231 + a:n
    endif
  endif
endfun

" Returns an approximate colour index for the given colour level
fun! s:rgb_number(x)
  if &t_Co == 88
    if a:x < 69
      return 0
    elseif a:x < 172
      return 1
    elseif a:x < 230
      return 2
    else
      return 3
    endif
  else
    if a:x < 75
      return 0
    else
      let l:n = (a:x - 55) / 40
      let l:m = (a:x - 55) % 40
      if l:m < 20
        return l:n
      else
        return l:n + 1
      endif
    endif
  endif
endfun

" Returns the palette index for the given R/G/B colour indices
fun! s:rgb_colour(x, y, z)
  if &t_Co == 88
    return 16 + (a:x * 16) + (a:y * 4) + a:z
  else
    return 16 + (a:x * 36) + (a:y * 6) + a:z
  endif
endfun

" Returns the actual colour level for the given colour index
fun! s:rgb_level(n)
  if &t_Co == 88
    if a:n == 0
      return 0
    elseif a:n == 1
      return 139
    elseif a:n == 2
      return 205
    else
      return 255
    endif
  else
    if a:n == 0
      return 0
    else
      return 55 + (a:n * 40)
    endif
  endif
endfun

" Returns the palette index to approximate the given R/G/B colour levels
fun! s:colour(r, g, b)
  " Get the closest grey
  let l:gx = s:grey_number(a:r)
  let l:gy = s:grey_number(a:g)
  let l:gz = s:grey_number(a:b)

  " Get the closest colour
  let l:x = s:rgb_number(a:r)
  let l:y = s:rgb_number(a:g)
  let l:z = s:rgb_number(a:b)

  if l:gx == l:gy && l:gy == l:gz
    " There are two possibilities
    let l:dgr = s:grey_level(l:gx) - a:r
    let l:dgg = s:grey_level(l:gy) - a:g
    let l:dgb = s:grey_level(l:gz) - a:b
    let l:dgrey = (l:dgr * l:dgr) + (l:dgg * l:dgg) + (l:dgb * l:dgb)
    let l:dr = s:rgb_level(l:gx) - a:r
    let l:dg = s:rgb_level(l:gy) - a:g
    let l:db = s:rgb_level(l:gz) - a:b
    let l:drgb = (l:dr * l:dr) + (l:dg * l:dg) + (l:db * l:db)
    if l:dgrey < l:drgb
      " Use the grey
      return s:grey_colour(l:gx)
    else
      " Use the colour
      return s:rgb_colour(l:x, l:y, l:z)
    endif
  else
    " Only one possibility
    return s:rgb_colour(l:x, l:y, l:z)
  endif
endfun

function! coc#color#rgb2term(rgb)
  let l:r = ("0x" . strpart(a:rgb, 0, 2)) + 0
  let l:g = ("0x" . strpart(a:rgb, 2, 2)) + 0
  let l:b = ("0x" . strpart(a:rgb, 4, 2)) + 0
  return s:colour(l:r, l:g, l:b)
endfun

" [r, g, b] ['255', '255', '255']
" return ['65535', '65535', '65535'] or return v:false to cancel
function! coc#color#pick_color(default_color)
  if has('mac')
    let default_color = map(a:default_color, {idx, val -> str2nr(val) * 65535 / 255 })
    " This is the AppleScript magic:
    let ascrpt = ['-e "tell application \"' . s:app . '\""',
          \ '-e "' . s:activate . '"',
          \ "-e \"set AppleScript's text item delimiters to {\\\",\\\"}\"",
          \ '-e "set theColor to (choose color default color {' . default_color[0] . ", " . default_color[1] . ", " . default_color[2] . '}) as text"',
          \ '-e "' . s:quit . '"',
          \ '-e "end tell"',
          \ '-e "return theColor"']
    let res = trim(system("osascript " . join(ascrpt, ' ') . " 2>/dev/null"))
    if empty(res)
      return v:false
    else
      return split(trim(res), ',')
    endif
  endif

  let hex_color = printf('#%02x%02x%02x', a:default_color[0], a:default_color[1], a:default_color[2])

  if has('unix')
    if executable('zenity')
      let res = trim(system('zenity --title="Select a color" --color-selection --color="' . hex_color . '" 2> /dev/null'))
      if empty(res)
        return v:false
      else
        " res format is rgb(255,255,255)
        return map(split(res[4:-2], ','), {idx, val -> string(str2nr(trim(val)) * 65535 / 255)})
      endif
    endif
  endif

  let rgb = v:false
  if !has('python')
    echohl Error | echom 'python support required, checkout :echo has(''python'')' | echohl None
    return
  endif
  try
    execute 'py import gtk'
  catch /.*/
    echohl Error | echom 'python gtk module not found' | echohl None
    return
  endtry
python << endpython

import vim
import gtk, sys

# message strings
wnd_title_insert = "Insert a color"

csd = gtk.ColorSelectionDialog(wnd_title_insert)
cs = csd.colorsel

cs.set_current_color(gtk.gdk.color_parse(vim.eval("hex_color")))

cs.set_current_alpha(65535)
cs.set_has_opacity_control(False)
# cs.set_has_palette(int(vim.eval("s:display_palette")))

if csd.run()==gtk.RESPONSE_OK:
    c = cs.get_current_color()
    s = [str(int(c.red)),',',str(int(c.green)),',',str(int(c.blue))]
    thecolor = ''.join(s)
    vim.command(":let rgb = split('%s',',')" % thecolor)

csd.destroy()

endpython
  return rgb
endfunction

