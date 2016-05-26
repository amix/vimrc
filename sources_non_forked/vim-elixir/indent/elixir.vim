if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal nosmartindent

setlocal indentexpr=GetElixirIndent()
setlocal indentkeys+=0),0],0=end,0=else,0=match,0=elsif,0=catch,0=after,0=rescue

if exists("*GetElixirIndent")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

let s:no_colon_before        = ':\@<!'
let s:no_colon_after         = ':\@!'
let s:symbols_end            = '\]\|}\|)'
let s:symbols_start          = '\[\|{\|('
let s:arrow                  = '^.*->$'
let s:skip_syntax            = '\%(Comment\|String\)$'
let s:block_skip             = "synIDattr(synID(line('.'),col('.'),1),'name') =~? '".s:skip_syntax."'"
let s:block_start            = '\<\%(do\|fn\)\>'
let s:block_middle           = 'else\|match\|elsif\|catch\|after\|rescue'
let s:block_end              = 'end'
let s:starts_with_pipeline   = '^\s*|>.*$'
let s:ending_with_assignment = '=\s*$'

let s:indent_keywords   = '\<'.s:no_colon_before.'\%('.s:block_start.'\|'.s:block_middle.'\)$'.'\|'.s:arrow
let s:deindent_keywords = '^\s*\<\%('.s:block_end.'\|'.s:block_middle.'\)\>'.'\|'.s:arrow

let s:pair_start  = '\<\%('.s:no_colon_before.s:block_start.'\)\>'.s:no_colon_after
let s:pair_middle = '\<\%('.s:block_middle.'\)\>'.s:no_colon_after.'\zs'
let s:pair_end    = '\<\%('.s:no_colon_before.s:block_end.'\)\>\zs'

let s:inside_block = 0

function! GetElixirIndent()
  let lnum = prevnonblank(v:lnum - 1)

  " At the start of the file use zero indent.
  if lnum == 0
    return 0
  endif

  let opened_symbol = 0
  let current_line  = getline(v:lnum)
  let last_line     = getline(lnum)
  let ind           = indent(lnum)

  " TODO: Remove these 2 lines
  " I don't know why, but for the test on spec/indent/lists_spec.rb:24.
  " Vim is making some mess on parsing the syntax of 'end', it is being
  " recognized as 'elixirString' when should be recognized as 'elixirBlock'.
  " This forces vim to sync the syntax.
  call synID(v:lnum, 1, 1)
  syntax sync fromstart

  if synIDattr(synID(v:lnum, 1, 1), "name") !~ s:skip_syntax

    if last_line !~ s:arrow
      let split_line = split(last_line, '\zs')
      let opened_symbol += count(split_line, '(') - count(split_line, ')')
      let opened_symbol += count(split_line, '[') - count(split_line, ']')
      let opened_symbol += count(split_line, '{') - count(split_line, '}')
    end

    " if start symbol is followed by a character, indent based on the
    " whitespace after the symbol, otherwise use the default shiftwidth
    if last_line =~ '\('.s:symbols_start.'\).'
      let opened_prefix = matchlist(last_line, '\('.s:symbols_start.'\)\s*')[0]
      let ind += (opened_symbol * strlen(opened_prefix))
    else
      let ind += (opened_symbol * &sw)
    endif

    if last_line =~ '^\s*\('.s:symbols_end.'\)' || last_line =~ s:indent_keywords
      let ind += &sw
    endif

    if current_line =~ '^\s*\('.s:symbols_end.'\)'
      let ind -= &sw
    endif

    if last_line =~ s:ending_with_assignment && opened_symbol == 0
      let b:old_ind = indent(lnum)
      let ind += &sw
    end

    " if line starts with pipeline
    " and last line ends with a pipeline,
    " align them
    if last_line =~ '|>.*$' &&
          \ current_line =~ s:starts_with_pipeline
      let ind = float2nr(match(last_line, '|>') / &sw) * &sw

    " if line starts with pipeline
    " and last line is an attribution
    " indents pipeline in same level as attribution
    elseif current_line =~ s:starts_with_pipeline &&
          \ last_line =~ '^[^=]\+=.\+$'

      if !exists('b:old_ind') || b:old_ind == 0
        let b:old_ind = indent(lnum)
      end
      let ind = float2nr(matchend(last_line, '=\s*[^ ]') / &sw) * &sw
    endif

    " if last line starts with pipeline
    " and current line doesn't start with pipeline
    " returns the indentation before the pipeline
    if last_line =~ s:starts_with_pipeline &&
          \ current_line !~ s:starts_with_pipeline
      let ind = b:old_ind
    endif

    if current_line =~ s:deindent_keywords
      let bslnum = searchpair(
            \ s:pair_start,
            \ s:pair_middle,
            \ s:pair_end,
            \ 'nbW',
            \ s:block_skip
            \ )

      let ind = indent(bslnum)
    endif

    " indent case statements '->'
    if current_line =~ s:arrow
      let ind += &sw
    endif
  endif

  return ind
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save
