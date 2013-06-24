# $Id: mk_sas.mak 264 2003-02-13 02:59:30Z darren $
#
# Makefile for SAS/C Amiga Compiler
# Submitted by Stefan Haubenthal <polluks@freeshell.org>

CFLAGS= def AMIGA opt parm r sint

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

ctags: $(OBJECTS)
	sc link to $@ $(OBJECTS) math s sint

.c.o:
	$(CC) $(CFLAGS) -o $*.o $*.c

clean:
	-delete $(OBJECTS) ctags.lnk

archive: clean
	@-delete force RAM:ctags.lha
	lha -r a RAM:ctags // ctags
