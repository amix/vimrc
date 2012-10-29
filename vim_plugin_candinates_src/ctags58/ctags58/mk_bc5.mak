# $Id: mk_bc5.mak 623 2007-09-10 02:52:22Z dhiebert $
#
# Makefile for Win32 using Borland C++ compiler, version 5.5 (free version)

!include source.mak

REGEX_DEFINE = -DHAVE_REGCOMP -DREGEX_MALLOC -DSTDC_HEADERS=1
DEFINES = -DWIN32 $(REGEX_DEFINE)
INCLUDES = -I. -Ignu_regex
WARNINGS = -w-aus -w-par -w-pia -w-pro -w-sus
CFLAGS = -d -DSTRICT -lTpe -lap
BCC = bcc32

# Optimizations if your platform supports all of them.
OPT = -O2 -OS -lGt

# Allows multithreading
#MT_OPT = -tWM -lcw32mt

ctags: ctags.exe

ctags.exe: respbc5
	$(BCC) $(OPT) $(MT_OPT) -e$@ $(LDFLAGS) @respbc5

readtags.exe: readtags.c
	$(BCC) $(CFLAGS) $(OPT) $(MT_OPT) -e$@ $(DEFINES) -DREADTAGS_MAIN readtags.c $(LDFLAGS)

# Debug version
dctags.exe: respbc5
	$(BCC) -DDEBUG -e$@ $(LDFLAGS) @respbc5 debug.c

regex.obj:
	$(BCC) -c -o$@ -w- $(DEFINES) -Dconst= $(INCLUDES)

respbc5: $(SOURCES) $(REGEX_SOURCES) $(HEADERS) $(REGEX_HEADERS) mk_bc5.mak
	echo $(DEFINES) $(INCLUDES) > $@
	echo $(WARNINGS) >> $@
	echo $(CFLAGS) >> $@
	echo $(SOURCES) $(REGEX_SOURCES) >> $@

mostlyclean:
	- del *.obj
	- del *.tds
	- del dctags.exe
	- del respbc5
	- del tags

clean: mostlyclean
	- del ctags.exe
