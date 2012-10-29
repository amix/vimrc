Summary: cscope is an interactive, screen-oriented tool that allows the user to browse through C source files for specified elements of code.
Name: cscope
Version: 16.0a
Release: 1
Epoch: 1
License: BSD
Group: Development/Tools
Source: cscope-16.0a.tar.gz
URL: http://cscope.sourceforge.net
Buildroot: %{_tmppath}/%{name}-root

%description
cscope is an interactive, screen-oriented tool that allows the user to browse through C source files for specified elements of code.

%prep
%setup -q

%build
%configure
make

%install
rm -rf %{buildroot}
%makeinstall

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
%doc TODO COPYING ChangeLog AUTHORS README NEWS INSTALL
%{_bindir}/*
%{_mandir}/man1/*

%changelog
* Sun Mar 21 2004 Adam Monsen <adamm@wazamatta.com>
- updated packages/cscope.spec to use more RPM macros and shell globs.
  Should be more generic/robust/up-to-date/etc.
* Mon Jul 2 2001 Cscope development team <cscope-devel@lists.sourceforge.net>
- Version 15.3 release
- New flex scanner
- XEmacs support improvements
- Vim support improvements
- 64 bit fixes
- MSDOS support
- More editing keys
- Webcscope added to contrib
* Wed Nov 20 2000 Cscope development team <cscope-devel@lists.sourceforge.net>
- Version 15.1 release
- New menu and line matching interface
- Support for up to 62 (up from 9) matching lines on screen
- Numerous fixes
- Updated documentation
* Tue May 15 2000 Cscope development team  <cscope-devel@lists.sourceforge.net>
- Version 15.0bl2 (build 2) pre-alpha release
- Fixes and enhancements
- Updated documentation
- Autoconf/automake support
- directory restructuring
* Sun Apr 16 2000 Petr Sorfa <petrs@sco.com>
- Initial Open Source release
- Ported to GNU environment
- Created rpm package
