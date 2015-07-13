" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if &cp || v:version < 702 || (exists('g:loaded_airline') && g:loaded_airline)
  finish
endif
let g:loaded_airline = 1

let s:airline_initialized = 0
let s:airline_theme_defined = 0
function! s:init()
  if s:airline_initialized
    return
  endif
  let s:airline_initialized = 1

  call airline#extensions#load()
  call airline#init#sections()

  let s:airline_theme_defined = exists('g:airline_theme')
  if s:airline_theme_defined || !airline#switch_matching_theme()
    let g:airline_theme = get(g:, 'airline_theme', 'dark')
    call airline#switch_theme(g:airline_theme)
  endif

  silent doautocmd User AirlineAfterInit
endfunction

function! s:on_window_changed()
  if pumvisible()
    return
  endif
  call s:init()
  call airline#update_statusline()
endfunction

function! s:on_colorscheme_changed()
  call s:init()
  if !s:airline_theme_defined
    if airline#switch_matching_theme()
      return
    endif
  endif

  " couldn't find a match, or theme was defined, just refresh
  call airline#load_theme()
endfunction

function airline#cmdwinenter(...)
  call airline#extensions#apply_left_override('Command Line', '')
endfunction

function! s:airline_toggle()
  if exists("#airline")
    augroup airline
      au!
    augroup END
    augroup! airline

    if exists("s:stl")
      let &stl = s:stl
    endif

    silent doautocmd User AirlineToggledOff
  else
    let s:stl = &statusline
    augroup airline
      autocmd!

      autocmd CmdwinEnter *
            \ call airline#add_statusline_func('airline#cmdwinenter')
            \ | call <sid>on_window_changed()
      autocmd CmdwinLeave * call airline#remove_statusline_func('airline#cmdwinenter')

      autocmd ColorScheme * call <sid>on_colorscheme_changed()
      autocmd VimEnter,WinEnter,BufWinEnter,FileType,BufUnload,VimResized *
            \ call <sid>on_window_changed()

      autocmd BufWritePost */autoload/airline/themes/*.vim
            \ exec 'source '.split(globpath(&rtp, 'autoload/airline/themes/'.g:airline_theme.'.vim', 1), "\n")[0]
            \ | call airline#load_theme()
    augroup END

    if s:airline_initialized
      call s:on_window_changed()
    endif

    silent doautocmd User AirlineToggledOn
  endif
endfunction

function! s:get_airline_themes(a, l, p)
  let files = split(globpath(&rtp, 'autoload/airline/themes/'.a:a.'*'), "\n")
  return map(files, 'fnamemodify(v:val, ":t:r")')
endfunction

function! s:airline_theme(...)
  if a:0
    call airline#switch_theme(a:1)
  else
    echo g:airline_theme
  endif
endfunction

command! -bar -nargs=? -complete=customlist,<sid>get_airline_themes AirlineTheme call <sid>airline_theme(<f-args>)
command! -bar AirlineToggleWhitespace call airline#extensions#whitespace#toggle()
command! -bar AirlineToggle call s:airline_toggle()
command! -bar AirlineRefresh call airline#load_theme() | call airline#update_statusline()

call airline#init#bootstrap()
call s:airline_toggle()

autocmd VimEnter * call airline#deprecation#check()

