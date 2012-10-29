# $Id: source.mak 720 2009-07-07 03:55:23Z dhiebert $
#
# Shared macros

HEADERS = \
	args.h ctags.h debug.h entry.h general.h get.h keyword.h \
	main.h options.h parse.h parsers.h read.h routines.h sort.h \
	strlist.h vstring.h

SOURCES = \
	args.c \
	ant.c \
	asm.c \
	asp.c \
	awk.c \
	basic.c \
	beta.c \
	c.c \
	cobol.c \
	dosbatch.c \
	eiffel.c \
	entry.c \
	erlang.c \
	flex.c \
	fortran.c \
	get.c \
	html.c \
	jscript.c \
	keyword.c \
	lisp.c \
	lregex.c \
	lua.c \
	main.c \
	make.c \
	matlab.c \
	ocaml.c \
	options.c \
	parse.c \
	pascal.c \
	perl.c \
	php.c \
	python.c \
	read.c \
	rexx.c \
	routines.c \
	ruby.c \
	scheme.c \
	sh.c \
	slang.c \
	sml.c \
	sort.c \
	sql.c \
	strlist.c \
	tcl.c \
	tex.c \
	verilog.c \
	vhdl.c \
	vim.c \
	yacc.c \
	vstring.c

ENVIRONMENT_HEADERS = \
    e_amiga.h e_djgpp.h e_mac.h e_msoft.h e_os2.h e_qdos.h e_riscos.h e_vms.h

ENVIRONMENT_SOURCES = \
    argproc.c mac.c qdos.c

REGEX_SOURCES = gnu_regex/regex.c

REGEX_HEADERS = gnu_regex/regex.h

OBJECTS = \
	args.$(OBJEXT) \
	ant.$(OBJEXT) \
	asm.$(OBJEXT) \
	asp.$(OBJEXT) \
	awk.$(OBJEXT) \
	basic.$(OBJEXT) \
	beta.$(OBJEXT) \
	c.$(OBJEXT) \
	cobol.$(OBJEXT) \
	dosbatch.$(OBJEXT) \
	eiffel.$(OBJEXT) \
	entry.$(OBJEXT) \
	erlang.$(OBJEXT) \
	flex.$(OBJEXT) \
	fortran.$(OBJEXT) \
	get.$(OBJEXT) \
	html.$(OBJEXT) \
	jscript.$(OBJEXT) \
	keyword.$(OBJEXT) \
	lisp.$(OBJEXT) \
	lregex.$(OBJEXT) \
	lua.$(OBJEXT) \
	main.$(OBJEXT) \
	make.$(OBJEXT) \
	matlab.$(OBJEXT) \
	ocaml.$(OBJEXT) \
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
	sml.$(OBJEXT) \
	sort.$(OBJEXT) \
	sql.$(OBJEXT) \
	strlist.$(OBJEXT) \
	tcl.$(OBJEXT) \
	tex.$(OBJEXT) \
	verilog.$(OBJEXT) \
	vhdl.$(OBJEXT) \
	vim.$(OBJEXT) \
	yacc.$(OBJEXT) \
	vstring.$(OBJEXT)
