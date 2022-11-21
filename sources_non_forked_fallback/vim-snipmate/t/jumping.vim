function! Setup(snip) abort
    return snipMate#expandSnip(join(a:snip, "\n"), 1)
endfunction

function! s:to_be_file(expected) abort
    return a:expected == getline(1,'$')
endfunction

function! s:to_be_in(item, list) abort
    return !empty(filter(copy(a:list), 'v:val is a:item'))
endfunction

call vspec#customize_matcher('to_be_file', function('s:to_be_file'))
call vspec#customize_matcher('to_be_in', function('s:to_be_in'))

describe 'snippet state'

    before
        enew
        let b:snip_state = snipmate#jumping#state()
    end

    after
        bwipeout!
    end

    describe '.remove()'

        it 'removes the state object'
            Expect exists('b:snip_state') to_be_true
            call b:snip_state.remove()
            Expect exists('b:snip_state') to_be_false
        end

        it 'removes snippet related autocommands'
            function! ReadAutocmds()
                redir => autocmds
                0verbose au snipmate_changes * <buffer>
                redir END
                return split(autocmds, "\n")
            endfunction
            aug snipmate_changes
                au CursorMoved,CursorMovedI <buffer> echo 'event'
            aug END

            Expect len(ReadAutocmds()) > 1
            call b:snip_state.remove()
            Expect len(ReadAutocmds()) == 1
        end

    end

    describe '.find_next_stop()'

        it 'increments/decrements the stop_no'
            let b:snip_state.stops = { 1 : {}, 2 : {} }
            let b:snip_state.stop_no = 1
            let b:snip_state.stop_count = 4

            call b:snip_state.find_next_stop(0)
            Expect b:snip_state.stop_no == 2
            call b:snip_state.find_next_stop(1)
            Expect b:snip_state.stop_no == 1
        end

        it 'continues iterating if the next/previous stop does not exist'
            let b:snip_state.stops = { 3 : {} }
            let b:snip_state.stop_count = 6
            let b:snip_state.stop_no = 1
            call b:snip_state.find_next_stop(0)
            Expect b:snip_state.stop_no == 3
            let b:snip_state.stop_no = 5
            call b:snip_state.find_next_stop(1)
            Expect b:snip_state.stop_no == 3
        end

        it 'does something at the ends'
            "
        end

    end

    describe '.remove_nested()'

        it 'removes nested mirrors and only nested mirrors'
            let mirror = { 'line' : 0 }
            let b:snip_state.stops = { 1 : { 'placeholder' : [[2, mirror]] },
                                    \  2 : { 'mirrors' : [mirror, {}] } }

            call b:snip_state.remove_nested(1)
            Expect len(b:snip_state.stops[2].mirrors) == 1
            Expect b:snip_state.stops[2].mirrors[0] isnot mirror
        end

        it 'removes nested stops'
            let stop = [2, 'abc']
            let b:snip_state.stops = { 1 : { 'placeholder' : [stop] },
                                    \  2 : { 'placeholder' : stop[1:1] } }

            call b:snip_state.remove_nested(1)
            Expect len(b:snip_state.stops) == 1
            Expect keys(b:snip_state.stops) == ['1']
        end

    end

    describe '.find_update_objects()'

        it 'finds mirrors/stops on the same line and after cur_stop'
            let b:snip_state.stops = {
                        \ 1 : { 'line' : 1, 'col' : 5,
                            \ 'placeholder' : ['x'] },
                        \ 2 : { 'line' : 1, 'col' : 7,
                            \ 'mirrors' : [{ 'line' : 1, 'col' : 7  }] }
                        \ }
            let stop = b:snip_state.stops[1]

            call b:snip_state.find_update_objects(stop)
            for obj in stop.update_objects
                Expect obj to_be_in [ b:snip_state.stops[2],
                            \ b:snip_state.stops[2].mirrors[0] ]
            endfor
        end

        it 'finds mirrors/stops on the same line and after cur_stop mirrors'
            let b:snip_state.stops = {
                        \ 1 : { 'line' : 1, 'col' : 5,
                            \ 'mirrors' : [{ 'line' : 2, 'col' : 5 }],
                            \ 'placeholder' : ['x'] },
                        \ 2 : { 'line' : 2, 'col' : 7,
                            \ 'mirrors' : [{ 'line' : 2, 'col' : 7  }] }
                        \ }
            let stop = b:snip_state.stops[1]

            call b:snip_state.find_update_objects(stop)
            for obj in stop.update_objects
                Expect obj to_be_in [ b:snip_state.stops[2],
                            \ b:snip_state.stops[2].mirrors[0] ]
            endfor
        end

        it 'ignores mirrors/stops on other lines'
            let b:snip_state.stops = {
                        \ 1 : { 'line' : 2, 'col' : 5,
                            \ 'placeholder' : ['x'] },
                        \ 2 : { 'line' : 1, 'col' : 7,
                            \ 'mirrors' : [{ 'line' : 1, 'col' : 7  }] },
                        \ 3 : { 'line' : 3, 'col' : 7,
                            \ 'mirrors' : [{ 'line' : 3, 'col' : 7  }] }
                        \ }
            let stop = b:snip_state.stops[1]

            call b:snip_state.find_update_objects(stop)
            Expect empty(stop.update_objects) to_be_true
        end

        it 'ignores mirrors/stops on the same line but before cur_stop/mirrors'
            let b:snip_state.stops = {
                        \ 1 : { 'line' : 1, 'col' : 5,
                            \ 'mirrors' : [{ 'line' : 2, 'col' : 5 }],
                            \ 'placeholder' : ['x'] },
                        \ 2 : { 'line' : 1, 'col' : 1,
                            \ 'mirrors' : [{ 'line' : 2, 'col' : 1  }] },
                        \ 3 : { 'line' : 2, 'col' : 3,
                            \ 'mirrors' : [{ 'line' : 1, 'col' : 3  }] },
                        \ }
            let stop = b:snip_state.stops[1]

            call b:snip_state.find_update_objects(stop)
            Expect empty(stop.update_objects) to_be_true
        end

    end

end
