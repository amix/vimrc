let s:style = get(g:, 'ayucolor', 'dark')

let s:fg = {}
let s:fg.primary    = {'dark': '#E6E1CF', 'light': '#5C6773', 'mirage': '#D9D7CE'}[s:style]
let s:fg.secondary  = {'dark': '#14191F', 'light': '#F0F0F0', 'mirage': '#232838'}[s:style]
let s:fg.contrast   = {'dark': '#C2D94C', 'light': '#86B300', 'mirage': '#BAE67E'}[s:style]
let s:fg.warning    = {'dark': '#FFBD54', 'light': '#F2AE49', 'mirage': '#FFDF80'}[s:style]
let s:fg.error      = {'dark': '#FF3333', 'light': '#FFF333', 'mirage': '#FF3333'}[s:style]

let s:bg = {}
let s:bg.primary    = {'dark': '#161F2A', 'light': '#DEE8F1', 'mirage': '#2A3546'}[s:style]
let s:bg.secondary  = {'dark': '#14191F', 'light': '#F0F0F0', 'mirage': '#232838'}[s:style]
let s:bg.contrast   = {'dark': '#E6B450', 'light': '#FF9940', 'mirage': '#FFCC66'}[s:style]
let s:bg.normal     = {'dark': '#01060E', 'light': '#D3D5D7', 'mirage': '#141925'}[s:style]
let s:bg.insert     = {'dark': '#39BAE6', 'light': '#55B4D4', 'mirage': '#5CCFE6'}[s:style]
let s:bg.replace    = {'dark': '#FF8F40', 'light': '#FA8D3E', 'mirage': '#FFA759'}[s:style]
let s:bg.visual     = {'dark': '#A37ACC', 'light': '#A37ACC', 'mirage': '#D4BFFF'}[s:style]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left     = [[s:fg.primary, s:bg.normal], [s:fg.primary, s:bg.primary]]
let s:p.normal.right    = [[s:fg.primary, s:bg.primary], [s:fg.primary, s:bg.primary]]
let s:p.normal.middle   = [[s:fg.contrast, s:bg.secondary]]
let s:p.normal.error    = [[s:fg.error, s:bg.primary, 'bold']]
let s:p.normal.warning  = [[s:fg.warning, s:bg.primary, 'bold']]
let s:p.inactive.left   = [[s:fg.primary, s:bg.secondary]]
let s:p.inactive.right  = [[s:fg.primary, s:bg.secondary]]
let s:p.inactive.middle = [[s:fg.primary, s:bg.secondary]]
let s:p.insert.left     = [[s:fg.secondary, s:bg.insert], [s:fg.primary, s:bg.primary]]
let s:p.replace.left    = [[s:fg.secondary, s:bg.replace], [s:fg.primary, s:bg.primary]]
let s:p.visual.left     = [[s:fg.secondary, s:bg.visual], [s:fg.primary, s:bg.primary]]
let s:p.tabline.left    = [[s:fg.primary, s:bg.primary]]
let s:p.tabline.tabsel  = [[s:fg.secondary, s:bg.contrast]]

let g:lightline#colorscheme#ayu#palette = lightline#colorscheme#fill(s:p)
