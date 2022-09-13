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

let s:patterns = {}
let s:patterns['hex']      = '\v#?(\x{2})(\x{2})(\x{2})'
let s:patterns['shortHex'] = '\v#(\x{1})(\x{1})(\x{1})'

let s:xterm_colors = {
    \ '0':   '#000000', '1':   '#800000', '2':   '#008000', '3':   '#808000', '4':   '#000080',
    \ '5':   '#800080', '6':   '#008080', '7':   '#c0c0c0', '8':   '#808080', '9':   '#ff0000',
    \ '10':  '#00ff00', '11':  '#ffff00', '12':  '#0000ff', '13':  '#ff00ff', '14':  '#00ffff',
    \ '15':  '#ffffff', '16':  '#000000', '17':  '#00005f', '18':  '#000087', '19':  '#0000af',
    \ '20':  '#0000df', '21':  '#0000ff', '22':  '#005f00', '23':  '#005f5f', '24':  '#005f87',
    \ '25':  '#005faf', '26':  '#005fdf', '27':  '#005fff', '28':  '#008700', '29':  '#00875f',
    \ '30':  '#008787', '31':  '#0087af', '32':  '#0087df', '33':  '#0087ff', '34':  '#00af00',
    \ '35':  '#00af5f', '36':  '#00af87', '37':  '#00afaf', '38':  '#00afdf', '39':  '#00afff',
    \ '40':  '#00df00', '41':  '#00df5f', '42':  '#00df87', '43':  '#00dfaf', '44':  '#00dfdf',
    \ '45':  '#00dfff', '46':  '#00ff00', '47':  '#00ff5f', '48':  '#00ff87', '49':  '#00ffaf',
    \ '50':  '#00ffdf', '51':  '#00ffff', '52':  '#5f0000', '53':  '#5f005f', '54':  '#5f0087',
    \ '55':  '#5f00af', '56':  '#5f00df', '57':  '#5f00ff', '58':  '#5f5f00', '59':  '#5f5f5f',
    \ '60':  '#5f5f87', '61':  '#5f5faf', '62':  '#5f5fdf', '63':  '#5f5fff', '64':  '#5f8700',
    \ '65':  '#5f875f', '66':  '#5f8787', '67':  '#5f87af', '68':  '#5f87df', '69':  '#5f87ff',
    \ '70':  '#5faf00', '71':  '#5faf5f', '72':  '#5faf87', '73':  '#5fafaf', '74':  '#5fafdf',
    \ '75':  '#5fafff', '76':  '#5fdf00', '77':  '#5fdf5f', '78':  '#5fdf87', '79':  '#5fdfaf',
    \ '80':  '#5fdfdf', '81':  '#5fdfff', '82':  '#5fff00', '83':  '#5fff5f', '84':  '#5fff87',
    \ '85':  '#5fffaf', '86':  '#5fffdf', '87':  '#5fffff', '88':  '#870000', '89':  '#87005f',
    \ '90':  '#870087', '91':  '#8700af', '92':  '#8700df', '93':  '#8700ff', '94':  '#875f00',
    \ '95':  '#875f5f', '96':  '#875f87', '97':  '#875faf', '98':  '#875fdf', '99':  '#875fff',
    \ '100': '#878700', '101': '#87875f', '102': '#878787', '103': '#8787af', '104': '#8787df',
    \ '105': '#8787ff', '106': '#87af00', '107': '#87af5f', '108': '#87af87', '109': '#87afaf',
    \ '110': '#87afdf', '111': '#87afff', '112': '#87df00', '113': '#87df5f', '114': '#87df87',
    \ '115': '#87dfaf', '116': '#87dfdf', '117': '#87dfff', '118': '#87ff00', '119': '#87ff5f',
    \ '120': '#87ff87', '121': '#87ffaf', '122': '#87ffdf', '123': '#87ffff', '124': '#af0000',
    \ '125': '#af005f', '126': '#af0087', '127': '#af00af', '128': '#af00df', '129': '#af00ff',
    \ '130': '#af5f00', '131': '#af5f5f', '132': '#af5f87', '133': '#af5faf', '134': '#af5fdf',
    \ '135': '#af5fff', '136': '#af8700', '137': '#af875f', '138': '#af8787', '139': '#af87af',
    \ '140': '#af87df', '141': '#af87ff', '142': '#afaf00', '143': '#afaf5f', '144': '#afaf87',
    \ '145': '#afafaf', '146': '#afafdf', '147': '#afafff', '148': '#afdf00', '149': '#afdf5f',
    \ '150': '#afdf87', '151': '#afdfaf', '152': '#afdfdf', '153': '#afdfff', '154': '#afff00',
    \ '155': '#afff5f', '156': '#afff87', '157': '#afffaf', '158': '#afffdf', '159': '#afffff',
    \ '160': '#df0000', '161': '#df005f', '162': '#df0087', '163': '#df00af', '164': '#df00df',
    \ '165': '#df00ff', '166': '#df5f00', '167': '#df5f5f', '168': '#df5f87', '169': '#df5faf',
    \ '170': '#df5fdf', '171': '#df5fff', '172': '#df8700', '173': '#df875f', '174': '#df8787',
    \ '175': '#df87af', '176': '#df87df', '177': '#df87ff', '178': '#dfaf00', '179': '#dfaf5f',
    \ '180': '#dfaf87', '181': '#dfafaf', '182': '#dfafdf', '183': '#dfafff', '184': '#dfdf00',
    \ '185': '#dfdf5f', '186': '#dfdf87', '187': '#dfdfaf', '188': '#dfdfdf', '189': '#dfdfff',
    \ '190': '#dfff00', '191': '#dfff5f', '192': '#dfff87', '193': '#dfffaf', '194': '#dfffdf',
    \ '195': '#dfffff', '196': '#ff0000', '197': '#ff005f', '198': '#ff0087', '199': '#ff00af',
    \ '200': '#ff00df', '201': '#ff00ff', '202': '#ff5f00', '203': '#ff5f5f', '204': '#ff5f87',
    \ '205': '#ff5faf', '206': '#ff5fdf', '207': '#ff5fff', '208': '#ff8700', '209': '#ff875f',
    \ '210': '#ff8787', '211': '#ff87af', '212': '#ff87df', '213': '#ff87ff', '214': '#ffaf00',
    \ '215': '#ffaf5f', '216': '#ffaf87', '217': '#ffafaf', '218': '#ffafdf', '219': '#ffafff',
    \ '220': '#ffdf00', '221': '#ffdf5f', '222': '#ffdf87', '223': '#ffdfaf', '224': '#ffdfdf',
    \ '225': '#ffdfff', '226': '#ffff00', '227': '#ffff5f', '228': '#ffff87', '229': '#ffffaf',
    \ '230': '#ffffdf', '231': '#ffffff', '232': '#080808', '233': '#121212', '234': '#1c1c1c',
    \ '235': '#262626', '236': '#303030', '237': '#3a3a3a', '238': '#444444', '239': '#4e4e4e',
    \ '240': '#585858', '241': '#606060', '242': '#666666', '243': '#767676', '244': '#808080',
    \ '245': '#8a8a8a', '246': '#949494', '247': '#9e9e9e', '248': '#a8a8a8', '249': '#b2b2b2',
    \ '250': '#bcbcbc', '251': '#c6c6c6', '252': '#d0d0d0', '253': '#dadada', '254': '#e4e4e4',
    \ '255': '#eeeeee'}

let s:xterm_16colors = {
\ 'black':          '#000000',
\ 'darkblue':       '#00008B',
\ 'darkgreen':      '#00CD00',
\ 'darkcyan':       '#00CDCD',
\ 'darkred':        '#CD0000',
\ 'darkmagenta':    '#8B008B',
\ 'brown':          '#CDCD00',
\ 'darkyellow':     '#CDCD00',
\ 'lightgrey':      '#E5E5E5',
\ 'lightgray':      '#E5E5E5',
\ 'gray':           '#E5E5E5',
\ 'grey':           '#E5E5E5',
\ 'darkgrey':       '#7F7F7F',
\ 'darkgray':       '#7F7F7F',
\ 'blue':           '#5C5CFF',
\ 'lightblue':      '#5C5CFF',
\ 'green':          '#00FF00',
\ 'lightgreen':     '#00FF00',
\ 'cyan':           '#00FFFF',
\ 'lightcyan':      '#00FFFF',
\ 'red':            '#FF0000',
\ 'lightred':       '#FF0000',
\ 'magenta':        '#FF00FF',
\ 'lightmagenta':   '#FF00FF',
\ 'yellow':         '#FFFF00',
\ 'lightyellow':    '#FFFF00',
\ 'white':          '#FFFFFF',
\ }

let s:w3c_color_names = {
\ 'aliceblue': '#F0F8FF',
\ 'antiquewhite': '#FAEBD7',
\ 'aqua': '#00FFFF',
\ 'aquamarine': '#7FFFD4',
\ 'azure': '#F0FFFF',
\ 'beige': '#F5F5DC',
\ 'bisque': '#FFE4C4',
\ 'black': '#000000',
\ 'blanchedalmond': '#FFEBCD',
\ 'blue': '#0000FF',
\ 'blueviolet': '#8A2BE2',
\ 'brown': '#A52A2A',
\ 'burlywood': '#DEB887',
\ 'cadetblue': '#5F9EA0',
\ 'chartreuse': '#7FFF00',
\ 'chocolate': '#D2691E',
\ 'coral': '#FF7F50',
\ 'cornflowerblue': '#6495ED',
\ 'cornsilk': '#FFF8DC',
\ 'crimson': '#DC143C',
\ 'cyan': '#00FFFF',
\ 'darkblue': '#00008B',
\ 'darkcyan': '#008B8B',
\ 'darkgoldenrod': '#B8860B',
\ 'darkgray': '#A9A9A9',
\ 'darkgreen': '#006400',
\ 'darkkhaki': '#BDB76B',
\ 'darkmagenta': '#8B008B',
\ 'darkolivegreen': '#556B2F',
\ 'darkorange': '#FF8C00',
\ 'darkorchid': '#9932CC',
\ 'darkred': '#8B0000',
\ 'darksalmon': '#E9967A',
\ 'darkseagreen': '#8FBC8F',
\ 'darkslateblue': '#483D8B',
\ 'darkslategray': '#2F4F4F',
\ 'darkturquoise': '#00CED1',
\ 'darkviolet': '#9400D3',
\ 'deeppink': '#FF1493',
\ 'deepskyblue': '#00BFFF',
\ 'dimgray': '#696969',
\ 'dodgerblue': '#1E90FF',
\ 'firebrick': '#B22222',
\ 'floralwhite': '#FFFAF0',
\ 'forestgreen': '#228B22',
\ 'fuchsia': '#FF00FF',
\ 'gainsboro': '#DCDCDC',
\ 'ghostwhite': '#F8F8FF',
\ 'gold': '#FFD700',
\ 'goldenrod': '#DAA520',
\ 'gray': '#808080',
\ 'green': '#008000',
\ 'greenyellow': '#ADFF2F',
\ 'honeydew': '#F0FFF0',
\ 'hotpink': '#FF69B4',
\ 'indianred': '#CD5C5C',
\ 'indigo': '#4B0082',
\ 'ivory': '#FFFFF0',
\ 'khaki': '#F0E68C',
\ 'lavender': '#E6E6FA',
\ 'lavenderblush': '#FFF0F5',
\ 'lawngreen': '#7CFC00',
\ 'lemonchiffon': '#FFFACD',
\ 'lightblue': '#ADD8E6',
\ 'lightcoral': '#F08080',
\ 'lightcyan': '#E0FFFF',
\ 'lightgoldenrodyellow': '#FAFAD2',
\ 'lightgray': '#D3D3D3',
\ 'lightgreen': '#90EE90',
\ 'lightpink': '#FFB6C1',
\ 'lightsalmon': '#FFA07A',
\ 'lightseagreen': '#20B2AA',
\ 'lightskyblue': '#87CEFA',
\ 'lightslategray': '#778899',
\ 'lightsteelblue': '#B0C4DE',
\ 'lightyellow': '#FFFFE0',
\ 'lime': '#00FF00',
\ 'limegreen': '#32CD32',
\ 'linen': '#FAF0E6',
\ 'magenta': '#FF00FF',
\ 'maroon': '#800000',
\ 'mediumaquamarine': '#66CDAA',
\ 'mediumblue': '#0000CD',
\ 'mediumorchid': '#BA55D3',
\ 'mediumpurple': '#9370D8',
\ 'mediumseagreen': '#3CB371',
\ 'mediumslateblue': '#7B68EE',
\ 'mediumspringgreen': '#00FA9A',
\ 'mediumturquoise': '#48D1CC',
\ 'mediumvioletred': '#C71585',
\ 'midnightblue': '#191970',
\ 'mintcream': '#F5FFFA',
\ 'mistyrose': '#FFE4E1',
\ 'moccasin': '#FFE4B5',
\ 'navajowhite': '#FFDEAD',
\ 'navy': '#000080',
\ 'oldlace': '#FDF5E6',
\ 'olive': '#808000',
\ 'olivedrab': '#6B8E23',
\ 'orange': '#FFA500',
\ 'orangered': '#FF4500',
\ 'orchid': '#DA70D6',
\ 'palegoldenrod': '#EEE8AA',
\ 'palegreen': '#98FB98',
\ 'paleturquoise': '#AFEEEE',
\ 'palevioletred': '#D87093',
\ 'papayawhip': '#FFEFD5',
\ 'peachpuff': '#FFDAB9',
\ 'peru': '#CD853F',
\ 'pink': '#FFC0CB',
\ 'plum': '#DDA0DD',
\ 'powderblue': '#B0E0E6',
\ 'purple': '#800080',
\ 'red': '#FF0000',
\ 'rosybrown': '#BC8F8F',
\ 'royalblue': '#4169E1',
\ 'saddlebrown': '#8B4513',
\ 'salmon': '#FA8072',
\ 'sandybrown': '#F4A460',
\ 'seagreen': '#2E8B57',
\ 'seashell': '#FFF5EE',
\ 'sienna': '#A0522D',
\ 'silver': '#C0C0C0',
\ 'skyblue': '#87CEEB',
\ 'slateblue': '#6A5ACD',
\ 'slategray': '#708090',
\ 'snow': '#FFFAFA',
\ 'springgreen': '#00FF7F',
\ 'steelblue': '#4682B4',
\ 'tan': '#D2B48C',
\ 'teal': '#008080',
\ 'thistle': '#D8BFD8',
\ 'tomato': '#FF6347',
\ 'turquoise': '#40E0D0',
\ 'violet': '#EE82EE',
\ 'wheat': '#F5DEB3',
\ 'white': '#FFFFFF',
\ 'whitesmoke': '#F5F5F5',
\ 'yellow': '#FFFF00',
\ 'yellowgreen': '#9ACD32'
\ }

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

function! coc#color#term2rgb(term) abort
  if a:term < 0 || a:term > 255
    return '#000000'
  endif
  return s:xterm_colors[a:term]
endfunction

function! coc#color#rgb2term(rgb)
  let l:r = ("0x" . strpart(a:rgb, 0, 2)) + 0
  let l:g = ("0x" . strpart(a:rgb, 2, 2)) + 0
  let l:b = ("0x" . strpart(a:rgb, 4, 2)) + 0
  return s:colour(l:r, l:g, l:b)
endfunction

function! coc#color#rgbToHex(...)
  let [r, g, b] = ( a:0==1 ? a:1 : a:000 )
  let num = printf('%02x', float2nr(r)) . ''
        \ . printf('%02x', float2nr(g)) . ''
        \ . printf('%02x', float2nr(b)) . ''
  return '#' . num
endfunction

function! coc#color#hexToRgb(color)
  if type(a:color) == 2
    let color = printf('%x', a:color)
  else
    let color = a:color
  end
  let matches = matchlist(color, s:patterns['hex'])
  let factor  = 0x1
  if empty(matches)
    let matches = matchlist(color, s:patterns['shortHex'])
    let factor  = 0x10
    end
    if len(matches) < 4
      echohl Error
      echom 'Couldnt parse ' . string(color) . ' ' .  string(matches)
      echohl None
      return
    end
    let r = str2nr(matches[1], 16) * factor
    let g = str2nr(matches[2], 16) * factor
  let b = str2nr(matches[3], 16) * factor
  return [r, g, b]
endfunction

" @params String                 color      The color
" @params {Number|String|Float} [amount=5]  The percentage of light
function! coc#color#lighten(color, ...)
  let amount = a:0 ?
        \(type(a:1) < 2 ?
        \str2float(a:1) : a:1 )
        \: 5.0
  if(amount < 1.0)
    let amount = 1.0 + amount
  else
    let amount = 1.0 + (amount / 100.0)
  end
  let rgb = coc#color#hexToRgb(a:color)
  let rgb = map(rgb, 'v:val * amount')
  let rgb = map(rgb, 'v:val > 255.0 ? 255.0 : v:val')
  let rgb = map(rgb, 'float2nr(v:val)')
  let hex = coc#color#rgbToHex(rgb)
  return hex
endfunction

" @params String                 color      The color
" @params {Number|String|Float} [amount=5]  The percentage of darkness
function! coc#color#darken(color, ...)
  let amount = a:0 ?
        \(type(a:1) < 2 ?
        \str2float(a:1) : a:1 )
        \: 5.0
  if(amount < 1.0)
    let amount = 1.0 - amount
  else
    let amount = 1.0 - (amount / 100.0)
  end
  if(amount < 0.0)
    let amount = 0.0 | end
  let rgb = coc#color#hexToRgb(a:color)
  let rgb = map(rgb, 'v:val * amount')
  let rgb = map(rgb, 'v:val > 255.0 ? 255.0 : v:val')
  let rgb = map(rgb, 'float2nr(v:val)')
  let hex = coc#color#rgbToHex(rgb)
  return hex
endfu

function! coc#color#luminance(rgb) abort
  let vals = []
  for val in a:rgb
    let val = (val + 0.0)/255
    if val <= 0.03928
      call add(vals, val/12.92)
    else
      call add(vals, pow((val + 0.055)/1.055, 2.4))
    endif
  endfor
  return vals[0] * 0.2126 + vals[1] * 0.7152 + vals[2] * 0.0722
endfunction

function! coc#color#contrast(rgb1, rgb2) abort
  let lnum1 = coc#color#luminance(a:rgb1)
  let lnum2 = coc#color#luminance(a:rgb2)
  let brightest = lnum1 > lnum2 ? lnum1 : lnum2
  let darkest = lnum1 < lnum2 ? lnum1 : lnum2
  return (brightest + 0.05) / (darkest + 0.05)
endfunction

function! coc#color#hex_contrast(hex1, hex2) abort
  return  coc#color#contrast(coc#color#hexToRgb(a:hex1), coc#color#hexToRgb(a:hex2))
endfunction

function! coc#color#nameToHex(name, term) abort
  if a:term
    return has_key(s:xterm_16colors, a:name) ? s:xterm_16colors[a:name] : v:null
  endif
  return has_key(s:w3c_color_names, a:name) ? s:w3c_color_names[a:name] : v:null
endfunction

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
