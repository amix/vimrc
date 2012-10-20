	; ===========================================================================
	; Copyright (c) 1998-2000, The Santa Cruz Operation 
	; All rights reserved.
 
	; Redistribution and use in source and binary forms, with or without
	; modification, are permitted provided that the following conditions are met:

	; *Redistributions of source code must retain the above copyright notice,
	; this list of conditions and the following disclaimer.

	; *Redistributions in binary form must reproduce the above copyright notice,
	; this list of conditions and the following disclaimer in the documentation
	; and/or other materials provided with the distribution.

	; *Neither name of The Santa Cruz Operation nor the names of its contributors
	; may be used to endorse or promote products derived from this software
	; without specific prior written permission. 

	; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
	; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
	; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
	; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
	; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
	; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
	; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
	; INTERRUPTION)
	; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
	; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
	; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
	; DAMAGE. 
	; =========================================================================
	; $Id: gmacs.ml,v 1.1 2000/04/27 16:33:47 petr Exp $ 


	; cscope.ml (s.cscope.ml) - 1.4 (2/21/84 14:53:58)
	;
	; Macro to handle invocation of gmacs by cscope from the
	; experimental tools.  Cscope invokes gmacs with two arguments:
	;
	;	gmacs +line file
	;
	; This macro gobbles the line number, visits the specified file,
	; and moves to the specified line number.

(progn
	args
	pluses
	(setq pluses 0)
	(setq args (argc))
	(if (> args 1)
		(progn
			(if (= (string-to-char "+") (string-to-char (argv 1)))
				(setq pluses 1)
			)
			(setq args (- args 1))
			(while (> args pluses)
				(visit-file (argv args))
				(setq args (- args 1))
			)
			(if (= (> (argc) 2) (> pluses 0))
				(goto-line (argv 1))
			)
		)
	)
)
