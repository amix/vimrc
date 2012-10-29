/*===========================================================================
 Copyright (c) 1998-2000, The Santa Cruz Operation 
 All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 *Redistributions of source code must retain the above copyright notice,
 this list of conditions and the following disclaimer.

 *Redistributions in binary form must reproduce the above copyright notice,
 this list of conditions and the following disclaimer in the documentation
 and/or other materials provided with the distribution.

 *Neither name of The Santa Cruz Operation nor the names of its contributors
 may be used to endorse or promote products derived from this software
 without specific prior written permission. 

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
 IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION)
 HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 DAMAGE. 
 =========================================================================*/

/*	cscope - interactive C symbol cross-reference
 *
 *	display help
 *
 */

#include "global.h"
#if defined(USE_NCURSES) && !defined(RENAMED_NCURSES)
#include <ncurses.h>
#else
#include <curses.h>
#endif
/*
	max num of lines of help screen -
	this number needs to be increased if more than n help items are needed
*/
#define MAXHELP	50	/* maximum number of help strings */

static char const rcsid[] = "$Id: help.c,v 1.6 2005/04/29 18:44:31 broeker Exp $";

void
help(void)
{
	char	**ep, *s, **tp, *text[MAXHELP];	
	int	ln;

	tp = text;
	if (changing == NO) {
		if (mouse) {
			*tp++ = "Point with the mouse and click button 1 to move to the desired input field,\n";
			*tp++ = "type the pattern to search for, and then press the RETURN key.  For the first 4\n";
			*tp++ = "and last 2 input fields, the pattern can be a regcomp(3) regular expression.\n";
			*tp++ = "If the search is successful, you can edit the file containing a displayed line\n";
			*tp++ = "by pointing with the mouse and clicking button 1.\n";
			*tp++ = "\nYou can either use the button 2 menu or these single-character commands:\n\n";
		} else {
			*tp++ = "Press the RETURN key repeatedly to move to the desired input field, type the\n";
			*tp++ = "pattern to search for, and then press the RETURN key.  For the first 4 and\n";
			*tp++ = "last 2 input fields, the pattern can be a regcomp(3) regular expression.\n";
			*tp++ = "If the search is successful, you can use these single-character commands:\n\n";
			*tp++ = "0-9a-zA-Z\tEdit the file containing the displayed line.\n";
		}
		*tp++ = "space bar\tDisplay next set of matching lines.\n";
		*tp++ = "+\t\tDisplay next set of matching lines.\n";
		*tp++ = "^V\t\tDisplay next set of matching lines.\n";
		*tp++ = "-\t\tDisplay previous set of matching lines.\n";
		*tp++ = "^E\t\tEdit all lines.\n";
		*tp++ = ">\t\tWrite the list of lines being displayed to a file.\n";
		*tp++ = ">>\t\tAppend the list of lines being displayed to a file.\n";
		*tp++ = "<\t\tRead lines from a file.\n";
		*tp++ = "^\t\tFilter all lines through a shell command.\n";
		*tp++ = "|\t\tPipe all lines to a shell command.\n";
		if (!mouse) {
			*tp++ = "\nAt any time you can use these single-character commands:\n\n";
			*tp++ = "TAB\t\tSwap positions between input and output areas.\n";
			*tp++ = "RETURN\t\tMove to the next input field.\n";
			*tp++ = "^N\t\tMove to the next input field.\n";
			*tp++ = "^P\t\tMove to the previous input field.\n";
		}
		*tp++ = "^Y / ^A\t\tSearch with the last pattern typed.\n";
		*tp++ = "^B\t\tRecall previous input field and search pattern.\n";
		*tp++ = "^F\t\tRecall next input field and search pattern.\n";
		if(caseless)
			*tp++ = "^C\t\tToggle ignore/use letter case when searching (IGNORE).\n";
		else
			*tp++ = "^C\t\tToggle ignore/use letter case when searching (USE).\n";
		*tp++ = "^R\t\tRebuild the cross-reference.\n";
		*tp++ = "!\t\tStart an interactive shell (type ^D to return to cscope).\n";
		*tp++ = "^L\t\tRedraw the screen.\n";
		*tp++ = "?\t\tDisplay this list of commands.\n";
		*tp++ = "^D\t\tExit cscope.\n";
		*tp++ = "\nNote: If the first character of the pattern you want to search for matches\n";
		*tp++ = "a command, type a \\ character first.\n";
		*tp++ = "Note: Some ctrl keys may be occupied by your terminal configuration.\n";
	} else {
		if (mouse) {
			*tp++ = "Point with the mouse and click button 1 to mark or unmark the line to be\n";
			*tp++ = "changed.  You can also use the button 2 menu or these single-character\n";
			*tp++ = "commands:\n\n";
		}
		else {
			*tp++ = "When changing text, you can use these single-character commands:\n\n";
			*tp++ = "0-9a-zA-Z\tMark or unmark the line to be changed.\n";
		}
		*tp++ = "*\t\tMark or unmark all displayed lines to be changed.\n";
		*tp++ = "space bar\tDisplay next set of lines.\n";
		*tp++ = "+\t\tDisplay next set of lines.\n";
		*tp++ = "-\t\tDisplay previous set of lines.\n";
		*tp++ = "^A\t\tMark or unmark all lines to be changed.\n";
		*tp++ = "^D\t\tChange the marked lines and exit.\n";
		*tp++ = "ESC\t\tExit without changing the marked lines.\n";
		*tp++ = "!\t\tStart an interactive shell (type ^D to return to cscope).\n";
		*tp++ = "^L\t\tRedraw the screen.\n";
		*tp++ = "?\t\tDisplay this list of commands.\n";
	}
	/* print help, a screen at a time */
	ep = tp;
	ln = 0;
	for (tp = text; tp < ep; ) {
		if (ln < LINES - 1) {
			for (s = *tp; *s != '\0'; ++s) {
				if (*s == '\n') {
					++ln;
				}
			}
			(void) addstr(*tp++);
		}
		else {
			(void) addstr("\n");
			askforchar();
			(void) clear();
			ln = 0;
		}
	}
	if (ln) {
		(void) addstr("\n");
		askforchar();
	}
}
