"--------------------------------------------------------------------------------
"
"  Copyright (c) 2010 Michael Smith <msmith@msmith.id.au>
"
"  http://github.com/michaeljsmith/vim-indent-object
"
"  Permission is hereby granted, free of charge, to any person obtaining a copy
"  of this software and associated documentation files (the "Software"), to
"  deal in the Software without restriction, including without limitation the
"  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
"  sell copies of the Software, and to permit persons to whom the Software is
"  furnished to do so, subject to the following conditions:
"
"  The above copyright notice and this permission notice shall be included in
"  all copies or substantial portions of the Software.
"
"  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
"  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
"  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
"  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
"  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
"  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
"  IN THE SOFTWARE.
"
"--------------------------------------------------------------------------------

" Mappings excluding line below.
onoremap <silent>ai :<C-u>cal <Sid>HandleTextObjectMapping(0, 0, 0, [line("."), line("."), col("."), col(".")])<CR>
onoremap <silent>ii :<C-u>cal <Sid>HandleTextObjectMapping(1, 0, 0, [line("."), line("."), col("."), col(".")])<CR>
vnoremap <silent>ai :<C-u>cal <Sid>HandleTextObjectMapping(0, 0, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv
vnoremap <silent>ii :<C-u>cal <Sid>HandleTextObjectMapping(1, 0, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv

" Mappings including line below.
onoremap <silent>aI :<C-u>cal <Sid>HandleTextObjectMapping(0, 1, 0, [line("."), line("."), col("."), col(".")])<CR>
onoremap <silent>iI :<C-u>cal <Sid>HandleTextObjectMapping(1, 1, 0, [line("."), line("."), col("."), col(".")])<CR>
vnoremap <silent>aI :<C-u>cal <Sid>HandleTextObjectMapping(0, 1, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv
vnoremap <silent>iI :<C-u>cal <Sid>HandleTextObjectMapping(1, 1, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv

let s:l0 = -1
let s:l1 = -1
let s:c0 = -1
let s:c1 = -1

if !exists("g:indent_object_except_first_level")
	let g:indent_object_except_first_level = 1
endif

function! <Sid>TextObject(inner, incbelow, vis, range, count)

	" Record the current state of the visual region.
	let vismode = "V"

	" Detect if this is a completely new visual selction session.
	let new_vis = 0
	let new_vis = new_vis || s:l0 != a:range[0]
	let new_vis = new_vis || s:l1 != a:range[1]
	let new_vis = new_vis || s:c0 != a:range[2]
	let new_vis = new_vis || s:c1 != a:range[3]

	let s:l0 = a:range[0]
	let s:l1 = a:range[1]
	let s:c0 = a:range[2]
	let s:c1 = a:range[3]

	" Repeatedly increase the scope of the selection.
	let itr_cnt = 0
	let cnt = a:count
	while cnt > 0

		" Look for the minimum indentation in the current visual region.
		let l = s:l0
		let idnt_invalid = 1000
		let idnt = idnt_invalid
		while l <= s:l1
			if !(getline(l) =~ "^\\s*$")
				let idnt = min([idnt, indent(l)])
			endif
			let l += 1
		endwhile

		" Keep track of where the range should be expanded to.
		let l_1 = s:l0
		let l_1o = l_1
		let l2 = s:l1
		let l2o = l2

		" If we are highlighting only blank lines, we may not have found a
		" valid indent. In this case we need to look for the next and previous
		" non blank lines and check which of those has the largest indent.
		if idnt == idnt_invalid
			let idnt = 0
			let pnb = prevnonblank(s:l0)
			if pnb
				let idnt = max([idnt, indent(pnb)])
				let l_1 = pnb
			endif
			let nnb = nextnonblank(s:l0)
			if nnb
				let idnt = max([idnt, indent(nnb)])
			endif

			" If we are in whitespace at the beginning of a block, skip over
			" it when we are selecting the range. Similarly, if we are in
			" whitespace at the end, ignore it.
			if idnt > indent(pnb)
				let l_1 = nnb
			endif
			if idnt > indent(nnb)
				let l2 = pnb
			endif
		endif

		" Search backward for the first line with less indent than the target
		" indent (skipping blank lines).
		let blnk = getline(l_1) =~ "^\\s*$"
		while l_1 > 0 && (blnk || indent(l_1) >= idnt)
			if g:indent_object_except_first_level && idnt == 0 && blnk
				break
			endif
			if !blnk || !a:inner
				let l_1o = l_1
			endif
			let l_1 -= 1
			let blnk = getline(l_1) =~ "^\\s*$"
		endwhile

		" Search forward for the first line with more indent than the target
		" indent (skipping blank lines).
		let line_cnt = line("$")
		let blnk = getline(l2) =~ "^\\s*$"
		while l2 <= line_cnt && (blnk || indent(l2) >= idnt)
			if g:indent_object_except_first_level && idnt == 0 && blnk
				break
			endif
			if !blnk || !a:inner
				let l2o = l2
			endif
			let l2 += 1
			let blnk = getline(l2) =~ "^\\s*$"
		endwhile

		" Determine which of these extensions to include. Include neither if
		" we are selecting an 'inner' object. Exclude the bottom unless are
		" told to include it.
		let idnt2 = max([indent(l_1), indent(l2)])
		if indent(l_1) < idnt2 || a:inner
			let l_1 = l_1o
		endif
		if indent(l2) < idnt2 || a:inner || !a:incbelow
			let l2 = l2o
		endif
		let l_1 = max([l_1, 1])
		let l2 = min([l2, line("$")])

		" Extend the columns to the start and end.
		" If inner is selected, set the final cursor pos to the start
		" of the text in the line.
		let c_1 = 1
		if a:inner
			let c_1 = match(getline(l_1), "\\c\\S") + 1
		endif
		let c2 = len(getline(l2))
		if !a:inner
			let c2 += 1
		endif

		" Make sure there's no change if we haven't really made a
		" significant change in linewise mode - this makes sure that
		" we can iteratively increase selection in linewise mode.
		if itr_cnt == 0 && vismode ==# 'V' && s:l0 == l_1 && s:l1 == l2
			let c_1 = s:c0
			let c2 = s:c1
		endif

		" Check whether the visual region has changed.
		let chg = 0
		let chg = chg || s:l0 != l_1
		let chg = chg || s:l1 != l2
		let chg = chg || s:c0 != c_1
		let chg = chg || s:c1 != c2

		if vismode ==# 'V' && new_vis
			let chg = 1
		endif

		" Update the vars.
		let s:l0 = l_1
		let s:l1 = l2
		let s:c0 = c_1
		let s:c1 = c2

		" If there was no change, then don't decrement the count (it didn't
		" count because it didn't do anything).
		if chg
			let cnt = cnt - 1
		else
			" Since this didn't work, push the selection back one char. This
			" will have the effect of getting the enclosing block. Do it at
			" the beginning rather than the end - the beginning is very likely
			" to be only one indentation level different.
			if s:l0 == 0
				return
			endif
			let s:l0 -= 1
			let s:c0 = len(getline(s:l0))
		endif

		let itr_cnt += 1

	endwhile

	" Apply the range we have found. Make sure to use the current visual mode.
	call cursor(s:l0, s:c0)
	exe "normal! " . vismode
	call cursor(s:l1, s:c1)
	normal! o

	" Update these static variables - we need to keep these up-to-date between
	" invocations because it's the only way we can detect whether it's a new
	" visual mode. We need to know if it's a new visual mode because otherwise
	" if there's a single line block in visual line mode and we select it with
	" "V", we can't tell whether it's already been selected using Vii.
	exe "normal! \<Esc>"
	let s:l0 = line("'<")
	let s:l1 = line("'>")
	let s:c0 = col("'<")
	let s:c1 = col("'>")
	normal gv0o0

endfunction

function! <Sid>HandleTextObjectMapping(inner, incbelow, vis, range)
	call <Sid>TextObject(a:inner, a:incbelow, a:vis, a:range, v:count1)
endfunction
