# $Id: mk_manx.mak 264 2003-02-13 02:59:30Z darren $
#
# Makefile for ctags on the Amiga, using Aztec/Manx C 5.0 or later

OBJEXT = o

OBJECTS = \
	args.$(OBJEXT) \
	asm.$(OBJEXT) \
	asp.$(OBJEXT) \
	awk.$(OBJEXT) \
	beta.$(OBJEXT) \
	c.$(OBJEXT) \
	cobol.$(OBJEXT) \
	eiffel.$(OBJEXT) \
	entry.$(OBJEXT) \
	erlang.$(OBJEXT) \
	fortran.$(OBJEXT) \
	get.$(OBJEXT) \
	keyword.$(OBJEXT) \
	lisp.$(OBJEXT) \
	lregex.$(OBJEXT) \
	lua.$(OBJEXT) \
	main.$(OBJEXT) \
	make.$(OBJEXT) \
	options.$(OBJEXT) \
	parse.$(OBJEXT) \
	pascal.$(OBJEXT) \
	perl.$(OBJEXT) \
	php.$(OBJEXT) \
	python.$(OBJEXT) \
	read.$(OBJEXT) \
	rexx.$(OBJEXT) \
	routines.$(OBJEXT) \
	ruby.$(OBJEXT) \
	scheme.$(OBJEXT) \
	sh.$(OBJEXT) \
	slang.$(OBJEXT) \
	sort.$(OBJEXT) \
	sml.$(OBJEXT) \
	sql.$(OBJEXT) \
	strlist.$(OBJEXT) \
	tcl.$(OBJEXT) \
	verilog.$(OBJEXT) \
	vim.$(OBJEXT) \
	vstring.$(OBJEXT) \
	yacc.$(OBJEXT)

CC = cc

#>>>>> choose between debugging (-bs) or optimizing (-so)
OPTIONS = -so
#OPTIONS = -bs

#>>>>>> choose -g for debugging
LN_DEBUG =
#LN_DEBUG = -g

CFLAGS = $(OPTIONS) -wapruq -ps -qf -DAMIGA -Dconst=

Ctags: $(OBJECTS)
	ln +q -m $(LN_DEBUG) -o Ctags $(OBJECTS) -lc16 -lm16

.c.o:
	$(CC) $(CFLAGS) -o $*.o $*.c
