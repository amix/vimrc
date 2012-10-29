# $Id: mk_bc3.mak 278 2003-02-24 02:27:53Z darren $
#
# Simple makefile for Borland C++ 3.1

!include source.mak

# Adjust the paths to your location of the borland C files
BCCLOC  = c:\borlandc
CC	= $(BCCLOC)\bin\bcc
INC	= -I$(BCCLOC)\include
LIB	= -L$(BCCLOC)\lib

# Add this file for wildcard expansion (does NOT work with 4.0!)
#EXTRA   =   $(BCCLOC)\lib\wildargs.obj

# The following compile options can be changed for better machines.
#	replace -1- with -2 to produce code for a 80286 or higher
#	replace -1- with -3 to produce code for a 80386 or higher
#	add -v for source debugging
OPTIMIZE= -1- -O1

CFLAGS	= -DMSDOS -ml -d -w-ccc -w-par -w-pia -w-rch -w-sus $(INC)
LFLAGS	= $(LIB) $(EXTRA)
EXTRA_LIBS =

ctags.exe: $(SOURCES) respbc3
	$(CC) $(OPTIMIZE) -e$@ @respbc3

debug: dctags.exe

dctags.exe: $(SOURCES) respbc3 debug.c
	$(CC) -DDEBUG -v -e$@ @respbc3 debug.c

respbc3: mk_bc3.mak
	copy &&|
$(CFLAGS)
$(LFLAGS)
$(SOURCES)
$(EXTRA_LIBS)
| $@

clean:
	del *.exe
	del *.obj
	del respbc3
	del tags
