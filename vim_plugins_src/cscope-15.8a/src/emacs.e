/ ========================================================================= /
/ Copyright (c) 1998-2000, The Santa Cruz Operation /
/ All rights reserved./
/ /
/ Redistribution and use in source and binary forms, with or without/
/ modification, are permitted provided that the following conditions are met:/
//
/ *Redistributions of source code must retain the above copyright notice,/
/ this list of conditions and the following disclaimer./
//
/ *Redistributions in binary form must reproduce the above copyright notice,/
/ this list of conditions and the following disclaimer in the documentation/
/ and/or other materials provided with the distribution./
//
/ *Neither name of The Santa Cruz Operation nor the names of its contributors/
/ may be used to endorse or promote products derived from this software/
/ without specific prior written permission. /
//
/ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS/
/ IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,/
/ THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR/
/ PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE/
/ LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR/
/ CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF/
/ SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS/
/ INTERRUPTION)/
/ HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT/
/ LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY/
/ OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH/
/ DAMAGE. /
/ ========================================================================= /

/ $Id: emacs.e,v 1.1 2000/04/27 16:33:47 petr Exp $ /


/ emacs menu for cscope /
((X) cscope (find current word [MACRO])
	(extern symbol-character)

	/ if this character is not part of a symbol /
	(cond ((not symbol-character)
		
		/ if the previous character is not part of a symbol, go to
		  the next word /
		back
		(cond ((not symbol-character) forward-word back-word))
	))
	/ get the current symbol (leave cursor at beginning of symbol) /
	(while symbol-character forward)	/ go past last symbol character /
	mark					/ mark this point /
	back					/ back to last symbol character /
	(while (cond (symbol-character (return back))))	/ back fails at BOF /
	(cond ((not symbol-character) forward))		/ forward if not at BOF /
	pickup-region				/ get the symbol /
	(local-string symbol)
	symbol=

	/ if arg > 0 then display the menu /
	(cond ((> arg 0) (display-menu
		(format symbol "5  Find functions calling %l()")
		(format symbol "4  Find functions called by %l()")
		(format symbol "3  Find global definition of %l")
		(format symbol "2  Find symbol %l")
		"1  Call cscope"
		5)
	))
	/ get the selection /
	(local selection)
	(selection= (read-character "Selection?"))

	/ if the selection is in range /
	(cond ((&& (>= selection '1') (<= selection '5'))
	
		/ if the selection requests finding the symbol /
		(local-string findsymbol)
		(findsymbol= "")
		(cond ((>= selection '2')
			(findsymbol= (format (char-to-string (- selection 2)) symbol "-%l '%l'"))))
		
		/ if arg > 1 or < 0 then don't update the cross-reference database /
		(local-string doption)
		(doption= "")
		(cond ((|| (> arg 1) (< arg 0)) (doption= "-d")))
		
		/ call cscope with usilent mode off /
		(local oldmode)				/ save old usilent mode /
		(oldmode= (set-mode "usilent" 0))	/ turn off usilent mode /
		(run-command (format doption findsymbol "cscope %l %l"))
		(set-mode "usilent" oldmode)		/ restore usilent mode /
	))
)
/ see if the current character is part of a symbol /
(symbol-character ()
	(local c)
	(c= current-character)
	(return (cond	((&& (>= c 'a') (<= c 'z')))
			((&& (>= c 'A') (<= c 'Z')))
			((&& (>= c '0') (<= c '9')))
			((== c '_'))
		)
	)
)
