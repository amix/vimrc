# $Id: mk_mvc.mak 724 2009-07-09 20:54:01Z dhiebert $
#
# Makefile for Win32 using Microsoft Visual C++ compiler

include source.mak

REGEX_DEFINES = -DHAVE_REGCOMP -D__USE_GNU -Dbool=int -Dfalse=0 -Dtrue=1 -Dstrcasecmp=stricmp
DEFINES = -DWIN32 $(REGEX_DEFINES)
INCLUDES = -I. -Ignu_regex
OPT = /O2

ctags: ctags.exe

ctags.exe: respmvc
	cl $(OPT) /Fe$@ @respmvc /link setargv.obj

readtags.exe: readtags.c
	cl /clr $(OPT) /Fe$@ $(DEFINES) -DREADTAGS_MAIN readtags.c /link setargv.obj

# Debug version
dctags.exe: respmvc
	cl /Zi -DDEBUG /Fe$@ @respmvc debug.c /link setargv.obj

regex.obj:
	cl /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) gnu_regex/regex.c

respmvc: $(SOURCES) $(REGEX_SOURCES) $(HEADERS) $(REGEX_HEADERS) mk_mvc.mak
	echo $(DEFINES) > $@
	echo $(INCLUDES) >> $@
	echo $(SOURCES) >> $@
	echo $(REGEX_SOURCES) >> $@

mostlyclean:
	- del *.obj
	- del dctags.exe
	- del respmvc
	- del tags

clean: mostlyclean
	- del ctags.exe
