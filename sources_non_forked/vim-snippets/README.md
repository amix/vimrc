IMPORTANT: comment on: [What about merging whith Ultisnip using its engine](https://github.com/garbas/vim-snipmate/issues/114)

Snipmate & UltiSnip Snippets
============================

This repository contains snippets files for various programming languages.

It is community-maintained and many people have contributed snippet files and
other improvements already.

Contents
========

    snippets/*: snippets using snipmate format
    UltiSnips/*: snippets using UltiSnips format

Snippet engines
===============

There are different forks of snippet engines which allow the user to insert
sippets by typing the name of a snippet hitting the expansion mapping.

    garbas/vim-snipmate [4]:
      VimL, snipmate-snippets, engine sometimes behaves strange, supports
      rewriting snippets on the fly (eg adding a second version with folding
      markers)

    MarcWeber/UltiSnips [6]:
      python, snipmate-snippets and UltiSnips-snippets

    SirVer/ultisnips [7]:
      python, UltiSnips-snippets

    github.com/Shougo/neosnippet [5]:
      viml, has a compatible mode allowing to reuse most snipmate snippets ?

    XPTemplate:
      totally different syntax, does not read snippets contained in this file,
      but it is also very powerful

    ... there are some more, but they have less features which is why I don't
    mention them here

UltiSnips engine has additional features such as "nested snippets".

Which one to use? If you have python give MarcWeber/UltiSnips a try because its
fast and supports all important features. You can prefer the UltiSnip versions
of the snippets by setting the "always_use_first_snippet" option to 1.

If you have VimL only (vim without python support) your best option is using
garbas/vim-snipmate and cope with the minor bugs found in the engine.

Related repositories
====================
We also encourage people to maintain sets of snippets for particular use cases
so that all users can benefit from them.  People can list their snippet repositories here:

     * https://github.com/rbonvall/snipmate-snippets-bib (snippets for BibTeX files)
     * https://github.com/sudar/vim-arduino-snippets (snippets for Arduino files)
     * https://github.com/zedr/zope-snipmate-bundle.git (snippets for Python, TAL and ZCML)
     * https://github.com/bonsaiben/bootstrap-snippets (snippets for Twitter Bootstrap markup, in HTML and Haml)

Installation using VAM: "github:rbonvall/snipmate-snippets-bib"

Historical notes
================

[vim-snipmate][1] was originally started by [Michael Sanders][2] who has now
unfortunately abandoned the project. [Rok Garbas][3] is now maintaining a
[fork][4] of the project in hopes of improving the existing code base.


Language maintainers
--------------------

No one can really be proficient in all programming languages. If you would like
to maintain snippets for a language, please get in touch.

* Python - [honza](http://github.com/honza)
* Javascript - [honza](http://github.com/honza)
* HTML Django - [honza](http://github.com/honza)
* Markdown - [honza](http://github.com/honza)
* Ruby - [taq](http://github.com/taq)
* PHP - [chrisyue](http://github.com/chrisyue)
* Scala - [gorodinskiy](https://github.com/gorodinskiy)

Contributing notes
------------------

Until further work is done on `vim-snipmate`, please don't add folding markers
into snippets. `vim-snipmate` has some comments about how to patch all snippets
on the fly adding those.

Because MarcWeber/UltiSnips [6] supports also snipmate-snippets there is no
need to duplicate all snippets - only those snippets who use advanced UltiSnips
features should be duplicated in UltiSnips (?)

Currently all snippets from UltiSnips have been put into UltiSnips - some work
on merging should be done (dropping duplicates etc)

Authors
-------

For a list of authors, please see the `AUTHORS` files.

License
-------

Just as the original snipMate plugin, all the snippets are licensed under the
terms of the MIT license.


[1]: http://github.com/garbas/vim-snipmate
[2]: http://github.com/msanders
[3]: http://github.com/garbas
[4]: http://github.com/garbas/vim-snipmate
[5]: http://github.com/Shougo/neosnippet
[6]: http://github.com/MarcWeber/UltiSnips
[7]: http://github.com/SirVer/ultisnips
