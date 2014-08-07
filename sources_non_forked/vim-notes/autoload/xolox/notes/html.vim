" Vim auto-load script
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 23, 2013
" URL: http://peterodding.com/code/vim/notes/

if !exists('g:notes_markdown_program')
  let g:notes_markdown_program = 'markdown'
endif

function! xolox#notes#html#view() " {{{1
  " Convert the current note to a web page and show the web page in a browser.
  " Requires [Markdown] [markdown] to be installed; you'll get a warning if it
  " isn't.
  "
  " [markdown]: http://en.wikipedia.org/wiki/Markdown
  try
    " Convert the note's text to HTML using Markdown.
    let starttime = xolox#misc#timer#start()
    let note_title = xolox#notes#current_title()
    let filename = xolox#notes#title_to_fname(note_title)
    let note_text = join(getline(1, '$'), "\n")
    let raw_html = xolox#notes#html#convert_note(note_text)
    let styled_html = xolox#notes#html#apply_template({
          \ 'encoding': &encoding,
          \ 'title': note_title,
          \ 'content': raw_html,
          \ 'version': g:xolox#notes#version,
          \ 'date': strftime('%A %B %d, %Y at %H:%M'),
          \ 'filename': fnamemodify(filename, ':~'),
          \ })
    let filename = s:create_temporary_file(note_title)
    if writefile(split(styled_html, "\n"), filename) != 0
      throw printf("Failed to write HTML file! (%s)", filename)
    endif
    " Open the generated HTML in a web browser.
    call xolox#misc#open#url('file://' . filename)
    call xolox#misc#timer#stop("notes.vim %s: Rendered HTML preview in %s.", g:xolox#notes#version, starttime)
  catch
    call xolox#misc#msg#warn("notes.vim %s: %s at %s", g:xolox#notes#version, v:exception, v:throwpoint)
  endtry
endfunction

function! xolox#notes#html#convert_note(note_text) " {{{1
  " Convert a note's text to a web page (HTML) using the [Markdown text
  " format] [markdown] as an intermediate format. This function takes the text
  " of a note (the first argument) and converts it to HTML, returning a
  " string.
  if !executable(g:notes_markdown_program)
    throw "HTML conversion requires the `markdown' program! On Debian/Ubuntu you can install it by executing `sudo apt-get install markdown'."
  endif
  let markdown = xolox#notes#markdown#convert_note(a:note_text)
  let result = xolox#misc#os#exec({'command': g:notes_markdown_program, 'stdin': markdown})
  let html = join(result['stdout'], "\n")
  return html
endfunction

function! xolox#notes#html#apply_template(variables) " {{{1
  " The vim-notes plug-in contains a web page template that's used to provide
  " a bit of styling when a note is converted to a web page and presented to
  " the user. This function takes the original HTML produced by [Markdown]
  " [markdown] (the first argument) and wraps it in the configured template,
  " returning the final HTML as a string.
  let filename = expand(g:notes_html_template)
  call xolox#misc#msg#debug("notes.vim %s: Reading web page template from %s ..", g:xolox#notes#version, filename)
  let template = join(readfile(filename), "\n")
  let output = substitute(template, '{{\(.\{-}\)}}', '\= s:template_callback(a:variables)', 'g')
  return output
endfunction

function! s:template_callback(variables) " {{{1
  " Callback for xolox#notes#html#apply_template().
  let key = xolox#misc#str#trim(submatch(1))
  return get(a:variables, key, '')
endfunction

function! s:create_temporary_file(note_title) " {{{1
  " Create a temporary filename for a note converted to an HTML document,
  " based on the title of the note.
  if !exists('s:temporary_directory')
    let s:temporary_directory = xolox#misc#path#tempdir()
  endif
  let filename = xolox#misc#str#slug(a:note_title) . '.html'
  return xolox#misc#path#merge(s:temporary_directory, filename)
endfunction
