function! copilot#handlers#window_logMessage(params, ...) abort
  call copilot#logger#Raw(get(a:params, 'type', 6), get(a:params, 'message', ''))
endfunction

function! copilot#handlers#window_showMessageRequest(params, ...) abort
  let choice = inputlist([a:params.message . "\n\nRequest Actions:"] +
        \ map(copy(get(a:params, 'actions', [])), { i, v -> (i + 1) . '. ' . v.title}))
  return choice > 0 ? get(a:params.actions, choice - 1, v:null) : v:null
endfunction

function! s:BrowserCallback(into, code) abort
  let a:into.code = a:code
endfunction

function! copilot#handlers#window_showDocument(params, ...) abort
  echo a:params.uri
  if empty(get(a:params, 'external'))
    return {'success': v:false}
  endif
  let browser = copilot#Browser()
  if empty(browser)
    return {'success': v:false}
  endif
  let status = {}
  call copilot#job#Stream(browser + [a:params.uri], v:null, v:null, function('s:BrowserCallback', [status]))
  let time = reltime()
  while empty(status) && reltimefloat(reltime(time)) < 1
    sleep 10m
  endwhile
  return {'success': get(status, 'code') ? v:false : v:true}
endfunction
