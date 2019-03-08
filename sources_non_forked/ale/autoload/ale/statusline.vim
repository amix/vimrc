" Author: KabbAmine <amine.kabb@gmail.com>
" Additions by: petpetpetpet <chris@freelanceninjas.com>
" Description: Statusline related function(s)

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

    " Allows easy access to the first instance of each problem type.
    let l:first_problems = {}

    for l:entry in l:loclist
        if l:entry.type is# 'W'
            if get(l:entry, 'sub_type', '') is# 'style'
                let l:count.style_warning += 1

                if l:count.style_warning == 1
                    let l:first_problems.style_warning = l:entry
                endif
            else
                let l:count.warning += 1

                if l:count.warning == 1
                    let l:first_problems.warning = l:entry
                endif
            endif
        elseif l:entry.type is# 'I'
            let l:count.info += 1

            if l:count.info == 1
                let l:first_problems.info = l:entry
            endif
        elseif get(l:entry, 'sub_type', '') is# 'style'
            let l:count.style_error += 1

            if l:count.style_error == 1
                let l:first_problems.style_error = l:entry
            endif
        else
            let l:count.error += 1

            if l:count.error == 1
                let l:first_problems.error = l:entry
            endif
        endif
    endfor

    " Set keys for backwards compatibility.
    let l:count[0] = l:count.error + l:count.style_error
    let l:count[1] = l:count.total - l:count[0]

    let g:ale_buffer_info[a:buffer].count = l:count
    let g:ale_buffer_info[a:buffer].first_problems = l:first_problems
endfunction

" Get the counts for the buffer, and update the counts if needed.
function! s:UpdateCacheIfNecessary(buffer) abort
    " Cache is cold, so manually ask for an update.
    if !has_key(g:ale_buffer_info[a:buffer], 'count')
        call ale#statusline#Update(
        \   a:buffer,
        \   g:ale_buffer_info[a:buffer].loclist
        \)
    endif
endfunction

function! s:BufferCacheExists(buffer) abort
    if !exists('g:ale_buffer_info') || !has_key(g:ale_buffer_info, a:buffer)
        return 0
    endif

    return 1
endfunction

" Get the counts for the buffer, and update the counts if needed.
function! s:GetCounts(buffer) abort
    if !s:BufferCacheExists(a:buffer)
        return s:CreateCountDict()
    endif

    call s:UpdateCacheIfNecessary(a:buffer)

    return g:ale_buffer_info[a:buffer].count
endfunction

" Get the dict of first_problems, update the buffer info cache if necessary.
function! s:GetFirstProblems(buffer) abort
    if !s:BufferCacheExists(a:buffer)
        return {}
    endif

    call s:UpdateCacheIfNecessary(a:buffer)

    return g:ale_buffer_info[a:buffer].first_problems
endfunction

" Returns a Dictionary with counts for use in third party integrations.
function! ale#statusline#Count(buffer) abort
    " The Dictionary is copied here before exposing it to other plugins.
    return copy(s:GetCounts(a:buffer))
endfunction

" Returns a copy of the *first* locline instance of the specified problem
" type. (so this would allow an external integration to know all the info
" about the first style warning in the file, for example.)
function! ale#statusline#FirstProblem(buffer, type) abort
    let l:first_problems = s:GetFirstProblems(a:buffer)

    if !empty(l:first_problems) && has_key(l:first_problems, a:type)
        return copy(l:first_problems[a:type])
    endif

    return {}
endfunction
