" File:         sketch.vim
" Version:      0.3.2
" Date:         2004-01-18
" Author:       Antony Scriven  <ads@metawire.org>
" Maintainer:   Antony Scriven  <ads@metawire.org>
" Description:  Line drawing, drag and drop, tables, arrows, boxes, fills,
"               using the mouse.
"
" Tutorial version: 2003-10-02
"
" Usage and settings
" ------------------
" See the file sketch.tut for a tutorial. There isn't a help file yet.
"
"
" Change or uncomment this line so you can start or stop Sketch.
" noremap <F1> :call ToggleSketch()<CR>
"
" Don't have a meta/alt key? Use this mapping to switch between square corners
" and rounded corners for boxes/lines
" nnoremap <buffer> <silent> <AnyKeyYouLike> <LeftMouse>:call NextCornerStyle()<CR>
"
" Release notes
" -------------
"       0.3.2   - Tabs still caused garbled screen when dragging transparent
"                 objects. Fixed.
"       0.3.1   - cursor() can cause problems in a file with tabs when using
"                 virutal edit mode. Changed to use G and |. Many tutorial
"                 errors fixed.
"       0.3.0   - Flood fill with arbitrary character added.
"       0.2.3   - Bug 6 fixed.
"       0.2.2   - Error in the tutorial that I thought I'd fixed is now really
"                 fixed. Thanks to pinkie..
"               - Tutorial updated.
"       0.2.1   - Bug 9 fixed.
"               - Tutorial updated.
"               - Keys rearranged again, sorry. Most important are the line
"                 drawing actions. These are all accessible via shift and
"                 control keys. Meta is used for painting/fat erase. This
"                 shouldn't be much of a loss if your terminal/keyboard
"                 doesn't have or doesn't like the meta/alt key.
"       0.2.0   - Bug 3 fixed.
"               - Bug 1 fixed.
"               - Bug 7 fixed.
"               - Bug 5 fixed.
"               - Corrected tutorial errors.
"                 Thanks to Tijs Van Den Bogaard.
"                 Added a quick reference card.
"               - Changed some commands around slightly after
"                 encountering some usability problems:
"                       Shift+LeftMouse is now paint.
"                       Control+LeftMouse is brush select.
"                       Shift+RightMouse is erase.
"                       Control+RightMouse is fat erase.
"               - Vis select then line/box/fill sets the arrowhead.
"               - Drag/drop opaque & transparent.
"               - Lines/boxes can have round corners: <M-LeftMouse>
"       0.1.1   Bug 2 fixed.
"       0.1.0   Initial beta release
"
"
" Please don't tell me my code is horrible, I know it is. It started as a
" quick test and I've hacked on all sorts of features in a nasty nasty way.
" Using virtualedit seems buggy, or at least how it works isn't entirely
" obvious in certain situations. So I've hacked in some workarounds.
" I'll tidy it up later (honest).
"
" To do, in order of priority (notes and examples at the end of the file)
" -----------------------------------------------------------------------
" - make a better behaved plugin
"   <SID> etc
"   uses registers in a few places withouth documenting this. Need a scheme to
"   avoid this.
" - use the new setreg features if ver = vim6.2
" - vim help file with command reference and link to tutorial
" - Rewrite the code nicely :-)
" >>Version 1
" - diagonals
" - coarser drawing to make straight lines and diagonals easier
" - keyboard support
" ...
" - painting with colours! (horrendously slow last I checked)
"
" Known bugs
" ----------
" 10.   Transparent drag doesn't quite work for dragging to column 1.
" 4.    Try drawing from below the bottom of the file. I don't think this is
"       fixable. But it may be possible to make it react a little more sanely.
" 8.    When dragging an object, doesn't quite work right when you are one
"       column to the right of your starting point.
"       Has this gone away?
"
" Fixed bugs
" ----------
" 9.    If mousemodel=popup then rightrelease gives an annoying menu.
"       --Solution: save &mousemodel; let mousemodel=''
" 5.    Select a line, then left-click to draw it. Now if you
"       double-click the arrow is in the correct direction. However, if
"       you select a line, then double-click to draw a line+arrow, the
"       arrowhead is not in the correct direction.
"       -- Fixed by getting rightrelease to handle this.
" 6.    Right vertical line of a visual box is drawn one
"       column to the right. Happens if the last cursor position of the visual
"       selection is on an empty line, and is to the right of the starting
"       position.
" 7.    Drawing through, e.g., letters produces '+'s. This should be lines.
" 3.    Visual selection, then click in another window: the click is
"       interpreted with the buffer-local map of the window you are
"       drawing in and you get a lot of error messages. This should be
"       fixable. (However not for erase :-()
" 1.    filling a box: rarely the leftmost, uppermost `-' isn't drawn.
"       Hopefully fixed by fixing bug 2!
" 2.    very rarely the left vertical line of a visual box is drawn one
"       column to the right ?? (virtualedit problem or my strlen()
"       problem). Happens if the last cursor position of the visual
"       selection is on an empty line, and is to the right of the
"       starting position.
"
"
" ----------------------------------------------------------------------
" To follow: poorly named, poorly commented, poorly written functions in
" a random order. Sorry.

fun! Cursor(line,col)
   exe 'silent norm! ' . a:line . 'G' . a:col . '|'
endfun

fun! ToggleSketch()
   if !exists("b:Sketch_loaded")
      let b:Sketch_loaded = 1
      call Sketch()
      echo '[Sketch on]'
      norm! zRz.
   else
      unlet b:Sketch_loaded
      call NoSketch()
      echo '[Sketch off]'
   endif
endfun

fun! NextBrush()
   let b:brushes = strpart(b:brushes,1,strlen(b:brushes)-1).b:brushes[0]
   echo '['.b:brushes[0].']['.((b:roundcorners==1)?'Round':'Square').']'
endfun

fun! NextCornerStyle()
   if b:roundcorners == 1
      let b:roundcorners = 0
   else
      let b:roundcorners = 1
   endif
   echo '['.b:brushes[0].']['.((b:roundcorners==1)?'Round':'Square').']'
endfun

fun! MarkVisStart()
   call SketchSavePos('auto')
   let b:Vis_start_line = line(".")
   let b:Vis_start_col = virtcol(".")
   let g:Sketch_winnr = winnr()
endfun

fun! MarkVisEnd()
   "Fixes bug 3. Clicking outside of the window where you first made the
   "selection.
   if winnr() != g:Sketch_winnr
      return 'abort'
   endif
   normal! gv
   let b:Vis_end_line = line(".")
   let b:Vis_end_col = virtcol(".")
   normal! 
   return ''
endfun

fun! Sketch()
   let b:brushes = '.:#'
   let b:Sketch_dir2 = 0
   let b:Sketch_dir = 'r'
   let b:Sketch_line = 0
   let b:Sketch_col = 0
   let b:Sketch_savedguiopts = &go
   let b:Sketch_savedmousemodel = &mousemodel
   set mousemodel=
   let b:Vis_start_line = 0
   let b:Vis_start_col = 0
   let b:Vis_end_line = 0
   let b:Vis_end_col = 0
   let b:Sketch_erasesize = 1
   let g:Sketch_winnr = winnr()
   let b:roundcorners = 0
   set go+=rb
   setl ve=all


   "Lines
   nnoremap <buffer> <silent> <LeftMouse> <LeftMouse>:call SketchClick('-')<CR>
   nnoremap <buffer> <silent> <LeftDrag> <LeftMouse>:call SketchDrag()<CR>

   "Arrow
   nnoremap <buffer> <silent> <2-LeftMouse> :call SketchClick('>')<CR>
   "Single vert bar
   nnoremap <buffer> <silent> <3-LeftMouse> :call SketchClick('\|')<CR>

   "Painting
   nnoremap <buffer> <silent> <C-LeftMouse> <LeftMouse>:call SketchPaint()<CR>
   nnoremap <buffer> <silent> <C-LeftDrag> <LeftMouse>:call SketchPaint()<CR>
   nnoremap <buffer> <silent> <M-LeftMouse> <LeftMouse>:call NextBrush()<CR>
   nnoremap <buffer> <silent> <M-2-LeftMouse> <LeftMouse>:call NextBrush()<CR>
   nnoremap <buffer> <silent> <M-3-LeftMouse> <LeftMouse>:call NextBrush()<CR>
   nnoremap <buffer> <silent> <M-4-LeftMouse> <LeftMouse>:call NextBrush()<CR>

   "Selection
   nnoremap <buffer> <silent> <RightMouse> <LeftMouse>:call MarkVisStart()<CR><4-LeftMouse>
   vnoremap <buffer> <silent> <RightDrag> <LeftDrag>
   vnoremap <buffer> <silent> <RightRelease> <LeftMouse>:call SketchSavePos('auto')<CR>gv
   "The gv"ap at the end prevents flicker when M-RightDrag does the undo.
   vnoremap <buffer> <silent> <S-RightMouse> <LeftMouse>:call VisSaveDims()<CR>gv"pygv"pp:let b:above = @p<CR>
   nnoremap <buffer> <silent> <S-RightDrag> u<LeftMouse>:call VisWithSavedDims()<CR>"pp
   nnoremap <buffer> <silent> <RightDrag> u<LeftMouse>:call VisWithSavedDims()<CR>:call PasteTransparent()<CR>:call VisWithSavedDims()<CR>"pp

   "Erase
   nnoremap <buffer> <silent> <C-RightMouse> <LeftMouse>:call SketchErase('small')<CR>
   nnoremap <buffer> <silent> <C-RightDrag> <LeftMouse>:call SketchErase('small')<CR>
   nnoremap <buffer> <silent> <M-RightMouse> <LeftMouse>:call SketchErase('big')<CR>
   nnoremap <buffer> <silent> <M-RightDrag> <LeftMouse>:call SketchErase('big')<CR>
   vnoremap <buffer> <silent> <RightMouse> <LeftMouse>:call VisSaveDims()<CR>gv"pygv"pp:let b:above = @p<CR>gv:call SketchErase('vblock')<CR><LeftMouse>
"    vnoremap <buffer> <silent> <RightMouse> <LeftMouse>:call VisSaveDims()<CR>gv"py:let b:above = @p<CR>gv:call SketchErase('vblock')<CR><LeftMouse>
   vnoremap <buffer> <silent> <2-RightMouse> <LeftMouse>:call VisSaveDims()<CR>gv"pygv"pp:let b:above = @p<CR>gv:call SketchErase('vblock')<CR><LeftMouse>
   "vnoremap <buffer> <silent> <RightMouse> :call SketchErase('vblock')<CR><LeftMouse>

   "Fill
   vnoremap <buffer> <silent> <LeftMouse> <LeftMouse>:call SketchFillBox('transparent')<CR>
   vnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>:call SketchFillBox('opaque')<CR>
   vnoremap <buffer> <silent> <C-LeftMouse> <LeftMouse>:call SketchFillBrush('opaque')<CR>
   nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>:call NextCornerStyle()<CR>
   nnoremap <buffer> <silent> <S-2-LeftMouse> <LeftMouse>:call NextCornerStyle()<CR>
   nnoremap <buffer> <silent> <S-3-LeftMouse> <LeftMouse>:call NextCornerStyle()<CR>
   nnoremap <buffer> <silent> <S-4-LeftMouse> <LeftMouse>:call NextCornerStyle()<CR>
   nnoremap <buffer> <leader>f :call <SID>Fill()<CR>
endfun

" Replace tabs in a:str with the correct number of spaces.
" Need to supply the tabstop value and the offset from the left edge of the
" screen.
" This implements the fix for version 0.3.2.
fun! s:EscapeAll(str)
   return escape(a:str, '\.*^$[]')
endfun
let s:spc = '                                 '
fun! s:Detab(str, ts, offset)
   let i = 0
   let n = 0
   let str = a:str
   while 1
      let i = match(str, '\t\|\n', i)
      if i == -1 | break | endif
      if str[i] == "\n"
         let n = i + 1
      else
         let numspaces = a:ts - ((i - n + a:offset) % a:ts)
         let str = substitute(str, '\t', strpart(s:spc, 0, numspaces ), '')
      endif
      let i = i + 1
   endwhile
   return str
endfun

fun! PasteTransparent() range
   normal! gv"oy
   let above = b:above
   "
   "s:Detab() has a noticeable impact on speed. So make sure there
   "really are tabs before removing them!
   if @o =~ '\t'
      let below = s:Detab(@o,&ts,virtcol('.')-1)
   else
      let below = @o
   endif

   "Need this if..endif, because you can't write .\{0} in a regexp.
   if above[0] == ' '
      let above = substitute(above, ' ', escape(below[0],'&'), '')
   endif
   let i = 1
   while i < strlen(above)
      if above[i] == ' '
         let above = substitute(above, '\(.\{'.i.'}\).', '\1'.escape(below[i],'&'), '')
      endif
      let i = i + 1
   endwhile
   let @p = above
   "normal! ms
   let pos = SavePos(0)
   $ put p
   "Need the zero. If @p has leading spaces, cursor will be on first non-blank
   "of the line.
   normal! 0G$h"pydG
   exe pos
endfun

fun! Move()
   if exists("b:vis_just_saved")
      "TRAILING SPACE
      normal! gvr 
      let b:vis_just_saved = 0
   endif
endfun

fun! VisSaveDims() range
   normal! gv
   if virtcol(".") > b:Vis_start_col
      let b:viswidth = virtcol(".") - b:Vis_start_col + 1
   else
      let b:viswidth = b:Vis_start_col - virtcol(".") + 1
   endif
   if line(".") > b:Vis_start_line
      let b:visheight = line(".") - b:Vis_start_line + 1
   else
      let b:visheight = b:Vis_start_line - line(".") + 1
   endif
   let b:vis_just_saved = 1
   let b:erase_or_drag = 'drag'
endfun

fun! VisWithSavedDims()
   if b:viswidth == 1 && b:visheight == 1
      return
   elseif b:viswidth == 1
      exe 'normal! '.(b:visheight-1).'jo'
   elseif b:visheight == 1
      exe 'normal! '.(b:viswidth-1).'lo'
   else
      exe 'normal! '.(b:viswidth-1).'l'.(b:visheight-1).'jo'
   endif
endfun

fun! NoSketch()
   let &go = b:Sketch_savedguiopts
   let &mousemodel = b:Sketch_savedmousemodel
   setl ve=
   nunmap <buffer> <LeftMouse>
   nunmap <buffer> <LeftDrag>
   "Arrow
   nunmap <buffer> <2-LeftMouse>
   "Single vert bar
   nunmap <buffer> <3-LeftMouse>
   "Painting
   nunmap <buffer> <C-LeftMouse>
   nunmap <buffer> <C-LeftDrag>
   nunmap <buffer> <M-LeftMouse>
   nunmap <buffer> <M-2-LeftMouse>
   nunmap <buffer> <M-3-LeftMouse>
   nunmap <buffer> <M-4-LeftMouse>
   "Selection
   nunmap <buffer> <RightMouse>
   vunmap <buffer> <RightDrag>
   vunmap <buffer> <RightRelease>
   vunmap <buffer> <S-RightMouse>
   nunmap <buffer> <S-RightDrag>
   nunmap <buffer> <RightDrag>
   "Erase
   nunmap <buffer> <C-RightMouse>
   nunmap <buffer> <C-RightDrag>
   nunmap <buffer> <M-RightMouse>
   nunmap <buffer> <M-RightDrag>
   vunmap <buffer> <RightMouse>
   vunmap <buffer> <2-RightMouse>
   "Fill
   vunmap <buffer> <LeftMouse>
   vunmap <buffer> <S-LeftMouse>
   vunmap <buffer> <C-LeftMouse>
   nunmap <buffer> <S-LeftMouse>
   nunmap <buffer> <S-2-LeftMouse>
   nunmap <buffer> <S-3-LeftMouse>
   nunmap <buffer> <S-4-LeftMouse>
   nunmap <buffer> <leader>f
endfun

fun! SketchFillBrush(style) range
   if MarkVisEnd() == 'abort'
      return
   endif
   if a:style == 'opaque'
      exe "normal! gvr".b:brushes[0]
   endif
endfun

fun! SketchFillBox(style) range
   "Fixes bug 3.
   if MarkVisEnd() == 'abort'
      return
   endif
   if a:style == 'opaque'
      "TRAILING SPACE
      normal! gvr 
   endif

   if b:Vis_start_line < b:Vis_end_line
      let top = b:Vis_start_line
      let bot = b:Vis_end_line
   else
      let top = b:Vis_end_line
      let bot = b:Vis_start_line
   endif
   if b:Vis_start_col < b:Vis_end_col
      let left = b:Vis_start_col
      let right = b:Vis_end_col
   else
      let left = b:Vis_end_col
      let right = b:Vis_start_col
   endif

   "Lots of bugginess if we are in virtual columns, so lets pad it first.
   let lnum = top
   while lnum <= bot
      let line = getline(lnum)
      "Handle lines with tabs
      "X:let len = strlen(line)
      exe "norm! " . lnum . "G"
      let len = virtcol("$") - 1
      if (len) < right "pad the line.
         let i = 1
         while i < (right - len)
            let line = line . ' '
            let i = i + 1
         endwhile
         call setline(lnum, line)
      endif
      let lnum = lnum + 1
   endwhile

   "Don't draw corners. If we are just drawing a line then we want to check
   "for a join first. Need to test for greater than 1, not 0 because we are
   "not including corners.
   if (bot-top) > 1
      "Wake up vim. Also fixes bug 2, bug 6.
      normal! lh
      call Cursor(top,left)
      let n = (bot-top-2)==0 ? '' : (bot-top-2).'j'
      normal! lh
      exe 'normal! jlh'.n.'r|'
      call Cursor(top,right)
      normal! lh
      "Seems to be a bug. If I don't do lh after visual block selction, it goes
      "wrong.
      exe 'normal! jlh'.n.'r|'
   endif
   if (right-left) > 1
      "Also bugginess if we don't wake vim up with a cursor movement before
      "calling cursor()
      normal! l
      call Cursor(top,left)
      let n = (right-left-2)==0 ? '' : (right-left-2).'l'
      exe 'normal! llh'.n.'r-'
      normal! l
      call Cursor(bot,left)
      exe 'normal! llh'.n.'r-'
   endif

   "Want to draw the '-' last, because we have triple click to insert a single
   "'|'. So now we can rightclick leftclick to insert a single '-' over a
   "character.
   if top == bot
      "Horizontal line.
      call Cursor(top,left)
      normal! lh
      if GetChar() == '|'
         normal! r+
      else
         normal! r-
      endif
      call Cursor(top,right)
      normal! lh
      if GetChar() == '|'
         normal! r+
      else
         normal! r-
      endif
   elseif left == right
      "Vertical line.
      call Cursor(top,left)
      normal! lh
      if GetChar() == '-'
         normal! r+
      else
         normal! r|
      endif
      call Cursor(bot,left)
      if GetChar() == '-'
         normal! r+
      else
         normal! r|
      endif
   else
      if b:roundcorners == 1
         call Cursor(top,left)
         normal! r.
         call Cursor(top,right)
         normal! r.
         call Cursor(bot,left)
         normal! r'
         call Cursor(bot,right)
         normal! r'
      else
         call Cursor(top,left)
         normal! r+
         call Cursor(top,right)
         normal! r+
         call Cursor(bot,left)
         normal! r+
         call Cursor(bot,right)
         normal! r+
      endif
   endif

   call Cursor(b:Vis_end_line, b:Vis_end_col)
   " Determine shape of box to set the direction.
   "/2 because letters are narrower than they are tall.
   if (right-left)/2 >= (bot-top)
      "Short fat box.
      if b:Vis_end_col > b:Vis_start_col
         call SketchSavePos('r')
      else
         call SketchSavePos('l')
      endif
   else
      "Tall thin box.
      if b:Vis_end_line > b:Vis_start_line
         call SketchSavePos('d')
      else
         call SketchSavePos('u')
      endif
   endif
   echo '['.b:brushes[0].']['.((b:roundcorners==1)?'Round':'Square').']'
endfun

fun! MoreLines(where)
   if a:where == 'below'
      let pos = SavePos(0)
      normal! G12o
      exe pos
   else
      let pos = SavePos(12)
      normal! 1G12O
      exe pos
   endif
endfun

fun! SavePos(offset)
   let c = virtcol(".")
   let l1 = line(".")
   normal! H
   let l2 = line(".")
   return 'call Cursor('.(l2 + a:offset).',1)| exe "normal! zt"|call Cursor('.(l1 + a:offset).',1)|exe "normal! '.(c-1).'l"'
endfun

fun! SketchPaint()
   call SketchSavePos('auto')
   if b:brushes[0] == '.'
      normal! r.
   elseif b:brushes[0] == ':'
      normal! r:lr:h
   elseif b:brushes[0] == '#'
      normal! hr#lr#lr#hhjr#lr#lr#hk
   endif
   silent call AddLineIfAtBoundary()
endfun

fun! SketchErase(size) range
   if winnr() != g:Sketch_winnr
      return
   endif
   if a:size == 'vblock'
      if MarkVisEnd() == 'abort'
         return
      endif
      "TRAILING SPACE
      normal! gvr 
   else
      if a:size == ''
         let size = b:Sketch_erasesize
      else
         let size = a:size
      endif
      if size == 'small'
         "TRAILING SPACE
         normal! r 
         let b:Sketch_erasesize = size
      elseif size == 'big'
         normal! hkR   hhjR   hhjR   hk
         let b:Sketch_erasesize = size
      endif
      call SketchSavePos('auto')
      silent call AddLineIfAtBoundary()
   endif
   echo '['.b:brushes[0].']['.((b:roundcorners==1)?'Round':'Square').']'
endfun


fun! SketchClick(char)
   if a:char == '>'
      call SketchArrow()
   elseif a:char == '|'
      call SketchBar()
   else
      "This fixes bug 7.
      if IsBlankChar() || (GetChar() != '-' && GetChar() != '|')
         exe "normal! r" . a:char
         let b:Sketch_firstclick = 1
      else
         normal! r+
         let b:Sketch_firstclick = 0
      endif
      if a:char == '-'
         call SketchSavePos('r')
      else
         call SketchSavePos('d')
      endif
   endif
   silent call AddLineIfAtBoundary()
endfun

fun! AddLineIfAtBoundary()
   "Tried adding line at top of file as well, but I didn't think it
   "worked very well.
   if line(".") == line("$")
      $ put _
   endif
   call Cursor(b:Sketch_line,b:Sketch_col)
endfun

fun! SketchArrow()
   if b:Sketch_dir2 == 'r'
      normal! r>
   elseif b:Sketch_dir2 == 'l'
      normal! r<
   elseif b:Sketch_dir2 == 'u'
      normal! r^
   elseif b:Sketch_dir2 == 'd'
      normal! rv
   endif
   call SketchSavePos(b:Sketch_dir2)
   echo '['.b:brushes[0].']['.((b:roundcorners==1)?'Round':'Square').']'
endfun

fun! SketchBar()
   normal! r|
   call SketchSavePos(b:Sketch_dir)
endfun

fun! Debug()
   echo "Line: ".b:Sketch_line."  Col: ".b:Sketch_col."  Dir: ".b:Sketch_dir
endfun

fun! SketchDrag()
   "Moving vertically (current col == last saved col)
   if virtcol(".") == b:Sketch_col
      "This fixes bug 7.
      if IsBlankChar() || (GetChar() !~ '-\|+')
         normal! r|
      else
         normal! r+
      endif
      if b:Sketch_dir =~ 'r\|l'
         if b:Sketch_firstclick
            call SketchAtSavedPos('|')
         else
            if b:roundcorners == 1
               "Previously moved horizontally, now moving up
               if line(".") < b:Sketch_line
                  call SketchAtSavedPos("'")
               "Moving down
               else
                  call SketchAtSavedPos('.')
               endif
            else
               call SketchAtSavedPos('+')
            endif
         endif
      endif
      "[TODO] I think this is redundant, as rightrelease now sets this.
      if line(".") > b:Sketch_line
         call SketchSavePos('d')
      else
         call SketchSavePos('u')
      endif
   "Otherwise, must be moving horizontally
   else
      "This fixes bug 7.
      if IsBlankChar() || (GetChar() !~ '|\|+')
         normal! r-
      else
         normal! r+
      endif
      if b:Sketch_dir =~ 'd\|u'
         if b:roundcorners == 1
            "Was moving up
            if b:Sketch_dir == 'u'
               call SketchAtSavedPos(".")
            "Moving down
            else
               call SketchAtSavedPos("'")
            endif
         else
            call SketchAtSavedPos('+')
         endif
      endif
      "I think this is redundant, as rightrelease now sets this.
      if virtcol(".") > b:Sketch_col
         call SketchSavePos('r')
      else
         call SketchSavePos('l')
      endif
   endif
   let b:Sketch_firstclick = 0
   silent call AddLineIfAtBoundary()
   echo '['.b:brushes[0].']['.((b:roundcorners==1)?'Round':'Square').']'
endfun

fun! IsBlankChar()
   let char = GetChar()
   if (char == ' ') || (char == '')
      return 1
   else
      return 0
   endif
endfun

fun! GetChar()
   return strpart(getline("."),virtcol(".")-1,1)
endfun

fun! SketchSavePos(dir)
   let b:Sketch_dir2 = b:Sketch_dir
   if a:dir == 'auto'
      if (line(".") > b:Sketch_line)
         let b:Sketch_dir = 'd'
      elseif (line(".") < b:Sketch_line)
         let b:Sketch_dir = 'u'
      elseif (virtcol(".") > b:Sketch_col)
         let b:Sketch_dir = 'r'
      elseif (virtcol(".") < b:Sketch_col)
         let b:Sketch_dir = 'l'
      endif
   else
      let b:Sketch_dir = a:dir
   endif
   let b:Sketch_line = line(".")
   let b:Sketch_col = virtcol(".")
endfun

fun! SketchAtSavedPos(char)
   let col = virtcol(".")
   let line = line(".")
   call Cursor(b:Sketch_line,b:Sketch_col)
   exe "normal! r".a:char
   "You can have fun if you reverse line and col
   call Cursor(line,col)
endfun

"
"-- Flood fill functions -----------------------------------
"
fun! s:Field(string, num, delim)
   " Return the field specified by integer a:num from a:string where fields are
   " delimited by the character a:delim.
   if a:num < 1
      return ''
   elseif a:num == 1
      return matchstr(a:string, '[^' . a:delim . ']*')
   else
      return matchstr(a:string, '\([^' . a:delim . ']*' . a:delim . '\)\{' . (a:num - 1) . '}\zs[^' . a:delim . ']*')
   endif
endfun

fun! s:Unlet()
   let i = 1
   while i <= s:TOS
      exe 'unlet s:fillinfo' . i
      let i = i + 1
   endwhile
endfun

" The fill algorithm might as well be recursive. However Vim has reasonably
" strict limits on the depth of recursion. It is quite hard to break this
" algorithm if done recursively, but it is possible with a large complicated
" diagram. Just in case this is a problem for someone, I've implemented the
" algorithm imperatively, using a couple of explicit stacks. Happily, this
" also seems to speed things up.
"
" The fill starts off horizontally. When a line has been filled as far as it
" can, the line, the leftmost cell and the rightmost cell are pushed onto a
" stack. Information from a stack is popped, and this is used to check each
" cell above and below those just filled. If any of these are not a boundary
" character then go back to the start of this paragraph.
"
" If I also encode the vertical direction in which the fill is progressing,
" then I can make a simple optimization to reduce the number of cells that
" need checking.

fun! s:PushFillInfo(line, L, R, dir) "dir can be 'u' up or 'd' down
   let s:TOS = s:TOS + 1
   exe 'let s:fillinfo' . s:TOS . '="' . a:line . ',' . a:L . ',' . a:R . ',' . a:dir . '"'
endfun

fun! s:PopFillInfo()
   exe 'let fillinfo = s:fillinfo' . s:TOS
   exe 'unlet s:fillinfo' . s:TOS
   let s:TOS = s:TOS - 1
   let line = s:Field(fillinfo, 1, ',')
   let L = s:Field(fillinfo, 2, ',')
   let R = s:Field(fillinfo, 3, ',')
   let dir = s:Field(fillinfo, 4, ',')
   return 'let line=' . line . '|let L=' . L . '|let R=' . R . '|let dir="' . dir . '"'
endfun

fun! s:IsBoundary()
   return @" ==# s:fillchar || @" == '|' || @" == '-' || @" == '' || @" == '+' || @" == '\' || @" == '/' || @" == '<' || @" == '>'
endfun

" Returns a string that you can execute to store the leftmost column in L and
" the rightmost column in R. E.g. :exe Fill_line()
fun! s:Fill_line()
   let line = line('.') | let col = col('.')
   "Fill current char
   let R = col | let L = col
   silent norm! yl
   if !s:IsBoundary()
      exe 'silent norm! r' . s:fillchar
   endif
   "Fill to the left
   let i = col
   let L = 1
   while i > 1
      silent norm! hyl
      let i = i - 1
      if s:IsBoundary()
         let L = i + 1
         break
      else
         exe 'silent norm! r' . s:fillchar
      endif
   endwhile
   call Cursor(line, col)
   "Fill to the right
   let i = col
   let R = col('$') - 1
   while i < (col('$') - 1)
      silent norm! lyl
      let i = i + 1
      if s:IsBoundary()
         let R = i - 1
         break
      else
         exe 'silent norm! r' . s:fillchar
      endif
   endwhile
   return 'let L=' . L . '|let R=' . R
endfun

fun! <SID>Fill()
   " The variables used for the stack get unlet as the function progresses.
   " However if the user hits <C-C> before filling has finished, we could
   " potentially be left with many variables in memory. So each time Fill() is
   " run, first check that nothing is left over from the previous Fill(). If
   " it is then remove the variables before prompting the user for the fill
   " character. This way the user won't notice the delay.
   if exists("s:fillinfo1")
      call s:Unlet()
   endif
   let s:TOS = 0
   "Get input from user. Use only the first character supplied.
   call inputsave()
   let s:fillchar = input("Enter the fill character: ")[0]
   call inputrestore()
   if s:fillchar == ''
      redraw!
      echo 'Fill aborted.'
      return
   endif
   echo 'Working, please wait.'
   let saved_ve = &ve
   let &ve = ''
   let lastline = line('$')
   silent norm! yl
   if s:IsBoundary()
      redraw!
      echo 'On a boundary.'
      return
   endif
   let line = line('.') | let col = col('.')
   let restorecursor = "call Cursor(" . line . "," . col . ")"
   silent exe s:Fill_line()
   call s:PushFillInfo(line, L, R, 'u')
   call s:PushFillInfo(line, L, R, 'd')
   while s:TOS > 0
      exe s:PopFillInfo()
      let l = L | let r = R
      if dir == 'u'
         if line > 1
            exe 'silent norm! ' . (line - 1). 'G' . L . '|'
         else
            continue
         endif
         let i = l
         while i <= r
            silent norm! yl
            if !s:IsBoundary()
               silent exe s:Fill_line()
               call s:PushFillInfo(line - 1, L, R, 'u')
               " Eliminate some (but not all) of the cells already checked.
               " This is actually slightly faster than eliminating all the
               " cells already checked! (shown underneath). Perhaps because of
               " the extra stack accesses being more expensive than looping
               " over a few already checked cells?
               if R > r || L < l
                  call s:PushFillInfo(line - 1, L, R, 'd')
               endif
               "if L < l
               "   call s:PushFillInfo(line - 1, L, l - 1, 'd')
               "endif
               "if R > r
               "   call s:PushFillInfo(line - 1, l + 1, R, 'd')
               "endif
               " Finish early if possible.
               if R >= r
                  break
               else
                  let i = R + 1
               endif
            else
               let i = i + 1
            endif
            exe 'silent norm! ' . i . '|'
         endwhile
      else "assume dir == 'd'
         if line < lastline
            exe 'silent norm! ' . (line + 1). 'G' . L . '|'
         else
            continue
         endif
         let i = l
         while i <= r
            silent norm! yl
            if !s:IsBoundary()
               silent exe s:Fill_line()
               call s:PushFillInfo(line + 1, L, R, 'd')
               " Eliminate some (but not all) of the cells already checked.
               if R > r || L < l
                  call s:PushFillInfo(line + 1, L, R, 'u')
               endif
               "if L < l
               "   call s:PushFillInfo(line + 1, L, l - 1, 'u')
               "endif
               "if R > r
               "   call s:PushFillInfo(line + 1, l + 1, R, 'u')
               "endif
               " Finish early if possible.
               if R >= r
                  break
               else
                  let i = R + 1
               endif
            else
               let i = i + 1
            endif
            exe 'silent norm! ' . i . '|'
         endwhile
      endif
   endwhile
   redraw!
   let &ve = saved_ve
   exe restorecursor
   echo 'Fill finished.'
endfun


"
"Implement diagonals like this:
"                                             /
"                                           \/
"                                           /\    _ 
"                                          /  \   /|
"  _      _              .----------------'    \ /
"  /|    |\   \     /   /                       X
" /        \   \| |/   /                       / \
"              ¯   ¯                          /   \ 
"                                                   
"
"Implement painting with colours like this:
"syn match y /y/
"syn match x /x/
"hi y guifg=#cc7788 guibg=#cc7788
"hi x guifg=#8877cc guibg=#8877cc
"map \x :let brush = 'x'<CR>
"map \y :let brush = 'y'<CR>
"map <rightdrag> <leftmouse>:exe 'normal! r'.brush <CR>
"map <rightmouse> <leftmouse>:exe 'normal! r'.brush <CR>
"
"     Even though Sketch doesn't have diagonals yet,
"     this was easy thanks to the copy+drag actions.
"     +-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
"     |\...........\...........\...........\...........\...........\...........\...........\...........\
"     |:\...........\...........\...........\...........\...........\...........\...........\...........\
"     |::+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
"     |::|\...........\...........\       |::|        |::|        |::|        |::|\...........\...........\
"     |::|:\...........\...........\      |::|        |::|        |::|        |::|:\...........\...........\
"     +::|::+-----------+-----------+-----+::|--------+::|--------+::|--------+::|::+-----------+-----------+
"     |\:|::|\...........\...........\.....\:|.........\:|.........\:|.........\:|::|\...........\...........\
"     |:\|::|:\...........\...........\.....\|..........\|..........\|..........\|::|:\...........\...........\
"     |::+::|::+-----------+-----------+-----+-----------+-----------+-----------+::|::+-----------+-----------+
"     |::|\:|::|\...........\...........\ |::|        |::|        |::|        |::|\:|::|\...........\...........\
"     |::|:\|::|:\...........\...........\|::|        |::|        |::|        |::|:\|::|:\...........\...........\
"     +::|::+::|::+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
"      \:|::|\:|::|\...........\...........\...........\...........\...........\...........\...........\...........\
"       \|::|:\|::|:\...........\...........\...........\...........\...........\...........\...........\...........\
"        +::|::|::|::+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
"         \:|::|\:|::|......\:|::|......\:|::|        |::|        |::|        |::|\:|::|\:|::|......\:|::|......\:|::|
"          \|::|:\|::|.......\|::|.......\|::|        |::|        |::|        |::| \|::|:\|::|.......\|::|.......\|::|
"           +::|::+::|--------+::|--------+::|--------+::|--------+::|--------+::|--------+::|--------+::|--------+::|
"            \:|::|\:|.........\:|.........\.|.........\:|.........\:|.........\:|.........\:|.........\:|.........\:|
"             \|::|:\|..........\|..........\|..........\|..........\|..........\|..........\|..........\|..........\|
"              +::|::+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
"               \:|::|......\:|::|......\:|::|        |::|        |::|        |::|      \:|::|......\:|::|......\:|::|
"                \|::|.......\|::|.......\|::|        |::|        |::|        |::|       \|::|.......\|::|.......\|::|
"                 +::|--------+::|--------+::|--------+::|--------+::|--------+::|--------+::|--------+::|--------+::|
"                  \:|.........\:|.........\:|.........\:|.........\:|.........\:|.........\:|.........\:|.........\:|
"                   \|..........\|..........\|..........\|..........\|..........\|..........\|..........\|..........\|
"                    +-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
"
"
"         +--------------------+       +--------------------+       +--------------------+       +--------------------+
"         |                    |       |                    |       |                    |       |                    |
"     +---|----------------+   |   +---|----------------+   |   +---|----------------+   |   +---|----------------+   |
"     |   |                |   |   |   |                |   |   |   |                |   |   |   |                |   |
"     |   |   +--------------------+   |   +--------------------|   |   +--------------------+   |   +--------------------+
"     |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |
"     |   +---|----------------+   |   +---|----------------+   |   +---|----------------+   |   +---|----------------+   |
"     |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |
"     |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |
"     |   |---|----------------|   |   |---|----------------|   |   |---|----------------|   |   |---|----------------|   |
"     |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |
"     +---|---|------------+   |   |---|---|------------+   |   +---|---|------------+   |   |---|---|------------+   |   |
"         |---|----------------|   |   |---|----------------|   |   |---|----------------|   |   |---|----------------|   |
"         |   +--------------------+   |   +--------------------+   |   +--------------------+   |   +--------------------+
"     +---|----------------+   |   +---|----------------+   |   +---|----------------+   |   +---|----------------+   |
"     |   +--------------------+   |   +--------------------+   |   +--------------------+   |   +--------------------+
"     |   |   +--------------------+   |   +--------------------|   |   +--------------------+   |   +--------------------+
"     |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |
"     |   +---|----------------+   |   +---|----------------+   |   +---|----------------+   |   +---|----------------+   |
"     |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |
"     |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |
"     |   |---|----------------|   |   |---|----------------|   |   |---|----------------|   |   |---|----------------|   |
"     |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |   |   |            |   |   |
"     +---|---|------------+   |   |---|---|------------+   |   +---|---|------------+   |   |---|---|------------+   |   |
"         |   |                |   |   |   |                |   |   |   |                |   |   |   |                |   |
"         |   +--------------------+   |   +--------------------+   |   +--------------------+   |   +--------------------+
"         |                    |       |                    |       |                    |       |                    |
"         +--------------------+       +--------------------+       +--------------------+       +--------------------+
"
"
"
" Not that you'd ever want to draw anything like this, but it is easy. Big
" screen helps :-)
