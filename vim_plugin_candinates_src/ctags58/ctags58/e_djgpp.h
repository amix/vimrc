/*
*   $Id: e_djgpp.h 375 2003-10-31 04:15:35Z darren $
*
*   Copyright (c) 2002-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Configures ctags for DJGPP environment.
*/
#ifndef E_DJGPP_H
#define E_DJGPP_H

#define CASE_INSENSITIVE_FILENAMES 1
#define MSDOS_STYLE_PATH 1

#define HAVE_DIR_H 1
#define HAVE_DIRENT_H 1
#define HAVE_FCNTL_H 1
#define HAVE_FNMATCH_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_DIR_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_TIME_H 1
#define HAVE_UNISTD_H 1

#define HAVE_CLOCK 1
#define HAVE_FGETPOS 1
#define HAVE_FNMATCH 1
#define HAVE_MKSTEMP 1
#define HAVE_OPENDIR 1
#define HAVE_REGCOMP 1
#define HAVE_REMOVE 1
#define HAVE_SETENV 1
#define HAVE_STAT_ST_INO 1
#define HAVE_STRCASECMP 1
#define HAVE_STRERROR 1
#define HAVE_STRNCASECMP 1
#define HAVE_STRSTR 1
#define HAVE_TRUNCATE 1
#define NEED_PROTO_LSTAT 1
#define STDC_HEADERS 1

#endif
