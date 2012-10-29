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

/*	cscope - interactive C symbol or text cross-reference
 *
 *	command history
 */

#include "global.h"

#include "alloc.h"

static char const rcsid[] = "$Id: history.c,v 1.3 2006/07/23 20:59:20 broeker Exp $";

static	struct cmd *tail, *current;

/* add a cmd to the history list */
void
addcmd(int f, char *s)		/* field number and command text */
{
	struct cmd *h;

	h = mymalloc(sizeof(struct cmd));
	if( tail) {
		tail->next = h;
		h->next = 0;
		h->prev = tail;
		tail = h;
	} else {
		tail = h;
		h->next = h->prev = 0;
	}
	h->field = f;
	h->text = my_strdup( s);
	current = 0;
}

/* return previous history item */
struct cmd *
prevcmd(void)
{
	if( current) {
		if( current->prev)	/* stay on first item */
			return current = current->prev;
		else
			return current;
	} else if( tail)
		return current = tail;
	else 
		return NULL;
}

/* return next history item */
struct cmd *
nextcmd(void)
{
	if( current) {
		if( current->next)	/* stay on first item */
			return current = current->next;
		else
			return current;
	} else 
		return NULL;
}
/* reset current to tail */
void
resetcmd(void)
{
	current = 0;
}

struct cmd *
currentcmd(void)
{
	return current;
}
