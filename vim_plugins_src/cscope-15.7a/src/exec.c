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
 *	process execution functions
 */

#include <unistd.h>
#include "global.h"
#include <stdarg.h>
#include <sys/wait.h>
#include <sys/types.h>      /* pid_t */
#ifdef __DJGPP__
#include <process.h>
#endif
#if defined(USE_NCURSES) && !defined(RENAMED_NCURSES)
#include <ncurses.h>
#else
#include <curses.h>
#endif

static char const rcsid[] = "$Id: exec.c,v 1.12 2009/04/10 13:39:23 broeker Exp $";

static	sighandler_t oldsigquit; /* old value of quit signal */
static	sighandler_t oldsighup; /* old value of hangup signal */
static	sighandler_t oldsigtstp; /* old value of SIGTSTP */

#ifndef __MSDOS__ /* none of these is needed, there */
static	int	join(pid_t p);
static	int	myexecvp(char *a, char **args);
static	pid_t	myfork(void);
#endif

/* execute forks and executes a program or shell script, waits for it to
 * finish, and returns its exit code.
 */

/*VARARGS1*/
int
execute(char *a, ...)	/* note: "exec" is already defined on u370 */
{
	va_list	ap;
	int	exitcode = -1;	/* initialize, to avoid warning */
	char	*argv[BUFSIZ];
	pid_t	p;

	/* fork and exec the program or shell script */
	endwin();	/* restore the terminal modes */
	mousecleanup();
	fflush(stdout);
	va_start(ap, a);
	for (p = 0; (argv[p] = va_arg(ap, char *)) != 0; p++)
		;
#ifdef __MSDOS__
	/* HBB 20010313: in MSDOG, everything is completely different.
	 * No fork()/exec()/wait(), but rather a single libc call: */
        exitcode = spawnvp(P_WAIT, a, argv);
#else
	if ((p = myfork()) == 0) {
		myexecvp(a, argv);	/* child */
	}
	else {
		exitcode = join(p);	/* parent */
	}
#endif /* MSDOS */
	
	/* the menu and scrollbar may be changed by the command executed */
#if UNIXPC || !TERMINFO
# ifndef __DJGPP__ /* leave CRLF handling as is */      
	nonl();
# endif
	raw();	/* endwin() turns off cbreak mode so restore it */
	noecho();
#endif
	mousemenu();
	drawscrollbar(topline, nextline);
	va_end(ap);
	return(exitcode);
}

#ifndef __MSDOS__ /* None of the following functions is used there */

/* myexecvp is an interface to the execvp system call to
 * modify argv[0] to reference the last component of its path-name.
 */
static int
myexecvp(char *a, char **args)
{
    char    msg[MSGLEN + 1];
	
    /* modify argv[0] to reference the last component of its path name */
    args[0] = mybasename(args[0]);

    /* execute the program or shell script */
    execvp(a, args);	/* returns only on failure */
    snprintf(msg, sizeof(msg), "\nCannot exec %s", a);
    perror(msg);		/* display the reason */
    askforreturn();		/* wait until the user sees the message */
    myexit(1);		/* exit the child */
    /* NOTREACHED */
}

/* myfork acts like fork but also handles signals */

static pid_t
myfork(void)
{
	pid_t	p;		/* process number */

	p = fork();
	
	/* the parent ignores the interrupt, quit, and hangup signals */
	if (p > 0) {
		oldsigquit = signal(SIGQUIT, SIG_IGN);
		oldsighup = signal(SIGHUP, SIG_IGN);
#ifdef SIGTSTP		
		oldsigtstp = signal(SIGTSTP, SIG_DFL);
#endif		
	}
	/* so they can be used to stop the child */
	else if (p == 0) {
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		signal(SIGHUP, SIG_DFL);
#ifdef SIGTSTP
		signal(SIGTSTP, SIG_DFL);
#endif			
	}
	/* check for fork failure */
	if (p == -1) {
		myperror("Cannot fork");
	}
	return p;
}

/* join is the compliment of fork */

static int
join(pid_t p) 
{
	int	status;  
	pid_t	w;

	/* wait for the correct child to exit */
	do {
		w = wait(&status);
	} while (p != -1 && w != p);

	/* restore signal handling */
	signal(SIGQUIT, oldsigquit);
	signal(SIGHUP, oldsighup);
#ifdef SIGTSTP
	signal(SIGTSTP, oldsigtstp);
#endif	

	/* return the child's exit code */
	return(status >> 8);
}

#endif /* !MSDOS */
