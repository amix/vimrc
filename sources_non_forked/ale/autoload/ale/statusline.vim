" Author: KabbAmine <amine.kabb@gmail.com>
" Description: Statusline related function(s)

" remove in 2.0
"
" A deprecated setting for ale#statusline#Status()
" See :help ale#statusline#Count() for getting status reports.
let g:ale_statusline_format = get(g:, 'ale_statusline_format',
\   ['%d error(s)', '%d warning(s)', 'OK']
\)

function! s:CreateCountDict() abort
    " Keys 0 and 1 are for backwards compatibility.
    " The count object used to be a List of [error_count, warning_count].
    return {
    \   '0': 0,
    \   '1': 0,
    \   'error': 0,
    \   'warning': 0,
    \   'info': 0,
    \   'style_error': 0,
    \   'style_warning': 0,
    \   'total': 0,
    \}
endfunction

" Update the buffer error/warning count with data from loclist.
function! ale#statusline#Update(buffer, loclist) abort
    if !exists('g:ale_buffer_info') || !has_key(g:ale_buffer_info, a:buffer)
        return
    endif

    let l:loclist = filter(copy(a:loclist), 'v:val.bufnr == a:buffer')
    let l:count = s:CreateCountDict()
    let l:count.total = len(l:loclist)

    for l:entry in l:loclist
        if l:entry.type is# 'W'
            if get(l:entry, 'sub_type', '') is# 'style'
                let l:count.style_warning += 1
            else
                let l:count.warning += 1
            endif
        elseif l:entry.type is# 'I'
            let l:count.info += 1
        elseif get(l:entry, 'sub_type', '') is# 'style'
            let l:count.style_error += 1
        else
            let l:count.error += 1
        endif
    endfor

    " Set keys for backwards compatibility.
    let l:count[0] = l:count.error + l:count.style_error
    let l:count[1] = l:count.total - l:count[0]

    let g:ale_buffer_info[a:buffer].count = l:count
endfunction

" Get the counts for the buffer, and update the counts if needed.
function! s:GetCounts(buffer) abort
    if !exists('g:ale_buffer_info') || !has_key(g:ale_buffer_info, a:buffer)
        return s:CreateCountDict()
    endif

    " Cache is cold, so manually ask for an update.
    if !has_key(g:ale_buffer_info[a:buffer], 'count')
        call ale#statusline#Update(a:buffer, g:ale_buffer_info[a:buffer].loclist)
    endif

    return g:ale_buffer_info[a:buffer].count
endfunction

" Returns a Dictionary with counts for use in third party integrations.
function! ale#statusline#Count(buffer) abort
    " The Dictionary is copied here before exposing it to other plugins.
    return copy(s:GetCounts(a:buffer))
endfunction

" This is the historical format setting which could be configured before.
function! s:StatusForListFormat() abort
    let [l:error_format, l:warning_format, l:no_errors] = g:ale_statusline_format
    let l:counts = s:GetCounts(bufnr(''))

    " Build strings based on user formatting preferences.
    let l:errors = l:counts[0] ? printf(l:error_format, l:counts[0]) : ''
    let l:warnings = l:counts[1] ? printf(l:warning_format, l:counts[1]) : ''

    " Different formats based on the combination of errors and warnings.
    if empty(l:errors) && empty(l:warnings)
        let l:res = l:no_errors
    elseif !empty(l:errors) && !empty(l:warnings)
        let l:res = printf('%s %s', l:errors, l:warnings)
    else
        let l:res = empty(l:errors) ? l:warnings : l:errors
    endif

    return l:res
endfunction

" remove in 2.0
"
" Returns a formatted string that can be integrated in the statusline.
"
" This function is deprecated, and should not be used. Use the airline plugin
" instead, or write your own status function with ale#statusline#Count()
function! ale#statusline#Status() abort
    if !get(g:, 'ale_deprecation_ale_statusline_status', 0)
        execute 'echom ''ale#statusline#Status() is deprecated, use ale#statusline#Count() to write your own function.'''
        let g:ale_deprecation_ale_statusline_status = 1
    endif

    if !exists('g:ale_statusline_format')
        return 'OK'
    endif

    if type(g:ale_statusline_format) == type([])
        return s:StatusForListFormat()
    endif

    return ''
endfunction
