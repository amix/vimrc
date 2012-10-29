/*
*   $Id: e_riscos.h 136 2002-03-08 22:35:19Z darren $
*
*   Copyright (c) 2002, Andrew Wingate
*
*   Author: Andrew Wingate <andy@sparse.net>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License. It is provided on an as-is basis and no
*   responsibility is accepted for its failure to perform as expected.
*
*   Configures ctags for RISC OS environment.
*
*   We currently use UnixLib. This file is subject to change if
*   we move to using SharedCLibrary and libGNU.
*/
#ifndef E_RISCOS_H
#define E_RISCOS_H

#define MACROS_USE_PATTERNS 1
#define DEFAULT_FILE_FORMAT 2
#define INTERNAL_SORT 1 /* Not all systems will have installed sort(1) */
#define TMPDIR "<Wimp$ScrapDir>"

/* Various definitions for UnixLib */
#define STDC_HEADERS 1
#define HAVE_CHMOD 1
#define HAVE_CHSIZE 1
#define HAVE_CLOCK 1
#define HAVE_FGETPOS 1
#define HAVE_FNMATCH 1
#define HAVE_FTRUNCATE 1
#define HAVE_MKSTEMP 1
#define HAVE_OPENDIR 1
#define HAVE_PUTENV 1
#define HAVE_REGCOMP 1 /* Requires RegEx library */
#define HAVE_REMOVE 1
#define HAVE_SETENV 1
#define HAVE_STRERROR 1
#define HAVE_STRICMP 1
#define HAVE_STRNICMP 1
#define HAVE_STRSTR 1
#define HAVE_TIMES 1
#define HAVE_TRUNCATE 1
#define HAVE_DIRENT_H 1
#define HAVE_FCNTL_H 1
#define HAVE_FNMATCH_H 1
#define HAVE_STAT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_DIR_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_TIME_H 1
#define HAVE_UNISTD_H 1

#endif
