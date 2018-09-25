" Author: w0rp <devw0rp@gmail.com>
" Description: APIs for working with asynchronous sockets, with an API
" normalised between Vim 8 and NeoVim. Socket connections only work in NeoVim
" 0.3+, and silently do nothing in earlier NeoVim versions.
"
" Important functions are described below. They are:
"
"   ale#socket#Open(address, options) -> channel_id (>= 0 if successful)
"   ale#socket#IsOpen(channel_id) -> 1 if open, 0 otherwise
"   ale#socket#Close(channel_id)
"   ale#socket#Send(channel_id, data)
"   ale#socket#GetAddress(channel_id) -> Return the address for a job

let s:channel_map = get(s:, 'channel_map', {})

function! s:VimOutputCallback(channel, data) abort
    let l:channel_id = ch_info(a:channel).id

    " Only call the callbacks for jobs which are valid.
    if l:channel_id >= 0 && has_key(s:channel_map, l:channel_id)
        call ale#util#GetFunction(s:channel_map[l:channel_id].callback)(l:channel_id, a:data)
    endif
endfunction

function! s:NeoVimOutputCallback(channel_id, data, event) abort
    let l:info = s:channel_map[a:channel_id]

    if a:event is# 'data'
        let l:info.last_line = ale#util#JoinNeovimOutput(
        \   a:channel_id,
        \   l:info.last_line,
        \   a:data,
        \   l:info.mode,
        \   ale#util#GetFunction(l:info.callback),
        \)
    endif
endfunction

" Open a socket for a given address. The following options are accepted:
"
" callback - A callback for receiving input. (required)
"
" A non-negative number representing a channel ID will be returned is the
" connection was successful. 0 is a valid channel ID in Vim, so test if the
" connection ID is >= 0.
function! ale#socket#Open(address, options) abort
    let l:mode = get(a:options, 'mode', 'raw')
    let l:Callback = a:options.callback

    let l:channel_info = {
    \   'address': a:address,
    \   'mode': l:mode,
    \   'callback': a:options.callback,
    \}

    if !has('nvim')
        " Vim
        let l:channel_options = {
        \   'mode': l:mode,
        \   'waittime': 0,
        \   'callback': function('s:VimOutputCallback'),
        \}

        " Use non-blocking writes for Vim versions that support the option.
        if has('patch-8.1.350')
            let l:channel_options.noblock = 1
        endif

        let l:channel_info.channel = ch_open(a:address, l:channel_options)
        let l:vim_info = ch_info(l:channel_info.channel)
        let l:channel_id = !empty(l:vim_info) ? l:vim_info.id : -1
    elseif exists('*chansend') && exists('*sockconnect')
        " NeoVim 0.3+
        try
            let l:channel_id = sockconnect('tcp', a:address, {
            \   'on_data': function('s:NeoVimOutputCallback'),
            \})
            let l:channel_info.last_line = ''
        catch /connection failed/
            let l:channel_id = -1
        endtry

        " 0 means the connection failed some times in NeoVim, so make the ID
        " invalid to match Vim.
        if l:channel_id is 0
            let l:channel_id = -1
        endif

        let l:channel_info.channel = l:channel_id
    else
        " Other Vim versions.
        let l:channel_id = -1
    endif

    if l:channel_id >= 0
        let s:channel_map[l:channel_id] = l:channel_info
    endif

    return l:channel_id
endfunction

" Return 1 is a channel is open, 0 otherwise.
function! ale#socket#IsOpen(channel_id) abort
    if !has_key(s:channel_map, a:channel_id)
        return 0
    endif

    if has('nvim')
        " In NeoVim, we have to check if this channel is in the global list.
        return index(map(nvim_list_chans(), 'v:val.id'), a:channel_id) >= 0
    endif

    let l:channel = s:channel_map[a:channel_id].channel

    return ch_status(l:channel) is# 'open'
endfunction

" Close a socket, if it's still open.
function! ale#socket#Close(channel_id) abort
    " IsRunning isn't called here, so we don't check nvim_list_chans()
    if !has_key(s:channel_map, a:channel_id)
        return 0
    endif

    let l:channel = remove(s:channel_map, a:channel_id).channel

    if has('nvim')
        silent! call chanclose(l:channel)
    elseif ch_status(l:channel) is# 'open'
        call ch_close(l:channel)
    endif
endfunction

" Send some data to a socket.
function! ale#socket#Send(channel_id, data) abort
    if !has_key(s:channel_map, a:channel_id)
        return
    endif

    let l:channel = s:channel_map[a:channel_id].channel

    if has('nvim')
        call chansend(l:channel, a:data)
    else
        call ch_sendraw(l:channel, a:data)
    endif
endfunction

" Get an address for a channel, or an empty string.
function! ale#socket#GetAddress(channel_id) abort
    return get(get(s:channel_map, a:channel_id, {}), 'address', '')
endfunction
