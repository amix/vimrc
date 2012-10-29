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

/*
 *	compath(pathname)
 *
 *	This compresses pathnames.  All strings of multiple slashes are
 *	changed to a single slash.  All occurrences of "./" are removed.
 *	Whenever possible, strings of "/.." are removed together with
 *	the directory names that they follow.
 *
 *	WARNING: since pathname is altered by this function, it should
 *		 be located in a temporary buffer. This avoids the problem
 *		 of accidently changing strings obtained from makefiles
 *		 and stored in global structures.
 */

static char const rcsid[] = "$Id: compath.c,v 1.3 2001/07/05 14:31:00 broeker Exp $";

#include "global.h"

#ifndef NULL
#define	NULL	0
#endif

char *
compath(char *pathname)			/*FDEF*/
{
	char	*nextchar;
	char	*lastchar;
	char	*sofar;
	char	*pnend;

	int	pnlen;

		/*
		 *	do not change the path if it has no "/"
		 */

	if (strchr(pathname, '/') == NULL)
		return(pathname);

		/*
		 *	find all strings consisting of more than one '/'
		 */

	for (lastchar = pathname + 1; *lastchar != '\0'; lastchar++)
		if ((*lastchar == '/') && (*(lastchar - 1) == '/'))
		{

			/*
			 *	find the character after the last slash
			 */

			nextchar = lastchar;
			while (*++lastchar == '/')
			{
			}

			/*
			 *	eliminate the extra slashes by copying
			 *	everything after the slashes over the slashes
			 */

			sofar = nextchar;
			while ((*nextchar++ = *lastchar++) != '\0')
				;
			lastchar = sofar;
		}

		/*
		 *	find all strings of "./"
		 */

	for (lastchar = pathname + 1; *lastchar != '\0'; lastchar++)
		if ((*lastchar == '/') && (*(lastchar - 1) == '.') &&
		    ((lastchar - 1 == pathname) || (*(lastchar - 2) == '/')))
		{

			/*
			 *	copy everything after the "./" over the "./"
			 */

			nextchar = lastchar - 1;
			sofar = nextchar;
			while ((*nextchar++ = *++lastchar) != '\0')
				;
			lastchar = sofar;
		}

		/*
		 *	find each occurrence of "/.."
		 */

	for (lastchar = pathname + 1; *lastchar != '\0'; lastchar++)
		if ((lastchar != pathname) && (*lastchar == '/') &&
		    (*(lastchar + 1) == '.') && (*(lastchar + 2) == '.') &&
		    ((*(lastchar + 3) == '/') || (*(lastchar + 3) == '\0')))
		{

			/*
			 *	find the directory name preceding the "/.."
			 */

			nextchar = lastchar - 1;
			while ((nextchar != pathname) &&
			    (*(nextchar - 1) != '/'))
				--nextchar;

			/*
			 *	make sure the preceding directory's name
			 *	is not "." or ".."
			 */

			if ((*nextchar == '.') &&
			    ((*(nextchar + 1) == '/') ||
			     ((*(nextchar + 1) == '.') && (*(nextchar + 2) == '/'))))
				/* EMPTY */;
			else
			{

				/*
				 * 	prepare to eliminate either
				 *	"dir_name/../" or "dir_name/.."
				 */

				if (*(lastchar + 3) == '/')
					lastchar += 4;
				else
					lastchar += 3;

				/*
				 *	copy everything after the "/.." to
				 *	before the preceding directory name
				 */

				sofar = nextchar - 1;
				while ((*nextchar++ = *lastchar++) != '\0');
					
				lastchar = sofar;

				/*
				 *	if the character before what was taken
				 *	out is '/', set up to check if the
				 *	slash is part of "/.."
				 */

				if ((sofar + 1 != pathname) && (*sofar == '/'))
					--lastchar;
			}
		}

	/*
 	 *	if the string is more than a character long and ends
	 *	in '/', eliminate the '/'.
	 */

	pnlen = strlen(pathname);
	pnend = strchr(pathname, '\0') - 1;

	if ((pnlen > 1) && (*pnend == '/'))
	{
		*pnend-- = '\0';
		pnlen--;
	}

	/*
	 *	if the string has more than two characters and ends in
	 *	"/.", remove the "/.".
	 */

	if ((pnlen > 2) && (*(pnend - 1) == '/') && (*pnend == '.'))
		*--pnend = '\0';

	/*
	 *	if all characters were deleted, return ".";
	 *	otherwise return pathname
	 */

	if (*pathname == '\0')
		(void) strcpy(pathname, ".");

	return(pathname);
}
