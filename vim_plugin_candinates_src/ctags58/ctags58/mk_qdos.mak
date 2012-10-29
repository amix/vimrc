# $Id: mk_qdos.mak 264 2003-02-13 02:59:30Z darren $
#
# Makefile for ctags on QDOS/SMS systems and C68 v4.24
# Submitted by Thierry Godefroy <godefroy@imaginet.fr>

# Directories:

T = ram1_
P = drv1_C68_

# Programs name:

CC  = $(P)cc
AS  = $(P)as68
ASM = $(P)qmac
LD  = $(P)ld

# Programs flags:

CCFLAGS  = -tmp$(T) -v -Y$(P) -I$(P)include_ -O
ASFLAGS  = -V
ASMFLAGS = -nolist
LDFLAGS  = -v -L$(P)lib_ -bufp150K\

# Target name:

EXEC = ctags

# Additional libraries:

LIBS =

# Target dependencies:

OBJEXT = o

HEADERS = e_qdos.h \
	args.h ctags.h debug.h entry.h general.h get.h keyword.h \
	main.h options.h parse.h parsers.h read.h routines.h sort.h \
	strlist.h vstring.h

OBJECTS = qdos.$(OBJEXT) \
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

$(EXEC) : $(OBJECTS)
    $(LD) -o$(EXEC) $(LDFLAGS) $(OBJECTS) $(LIBS)

$(OBJECTS): $(HEADERS)

# Construction rules:

_c_o :
    $(CC) -c $(CCFLAGS) $<

_s_o :
    $(AS) $(ASFLAGS) $< $@

_asm_rel :
    $(ASM) $< $(ASMFLAGS)

#end
