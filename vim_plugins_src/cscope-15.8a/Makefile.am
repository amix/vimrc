## Process this file with automake to produce Makefile.in

SUBDIRS = doc src contrib

EXTRA_DIST = INSTALL.gnu packages

dist-hook:
	@for dir in `find $(distdir) -type d -name CVS -print`; do \
	  rm -rf $$dir; done
