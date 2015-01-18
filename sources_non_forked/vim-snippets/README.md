snipMate & UltiSnip Snippets
============================

This repository contains snippets files for various programming languages.

It is community-maintained and many people have contributed snippet files and
other improvements already.

Contents
--------

- `snippets/*`: snippets using snipMate format
- `UltiSnips/*`: snippets using UltiSnips format

Snippet engines supporting vim-snippets
----------------------------------------

There are different forks of snippet engines which allow the user to insert
snippets by typing the name of a snippet hitting the expansion mapping.

- [github.com/SirVer/ultisnips](https://github.com/SirVer/ultisnips):   
  python, supports all snippets in this repo.
- [github.com/garbas/vim-snipmate](https://github.com/garbas/vim-snipmate):   
  VimL, snipmate-snippets, engine sometimes behaves strange. Supports
  snippets/*
- [github.com/Shougo/neosnippet](https://github.com/Shougo/neosnippet.vim):   
  VimL, supports snippets/* with some configuration.
- [github.com/drmingdrmer/xptemplate](https://github.com/drmingdrmer/xptemplate):
  Totally different syntax, does not read snippets contained in this file, but
  it is also very powerful. It does not support vim-snippets (just listing it
  here for completeness)

There tries to be a more comprehensive list (which still is incomplete) here:
http://vim-wiki.mawercer.de/wiki/topic/text-snippets-skeletons-templates.html

UltiSnips has additional features such as high speed, nesting snippets,
expanding snippets in snippets and offers powerful transformations on text in
snippets (like visual selections or placeholder texts).

Which one to use? If you have python give
[SirVer/ultisnips](https://github.com/SirVer/ultisnips) a try because its fast
and has the most features.

If you have VimL only (vim without python support) your best option is using
[garbas/vim-snipmate](https://github.com/garbas/vim-snipmate) and cope with the
minor bugs found in the engine.

Q: Should "snipMate be deprecated in favour of UltiSnips"?

A: No, because snipMate is VimL, and UltiSnips requires Python.
Some people want to use snippets without having to install Vim with Python
support. Yes - this sucks.

One solution would be: Use snippets if they are good enough, but allow overriding them
in UltiSnips. This would avoid most duplication while still serving most users.
AFAIK there is a nested-placeholder branch for snipMate too. snipMate is still
improved by Adnan Zafar. So maybe time is not ready to make a final decision yet.

[github issue/discussion](https://github.com/honza/vim-snippets/issues/363)

Installation
------------

First be aware that there are many options, see "Snippet engines" above.
Second be aware than there are [tons of plugin managers](http://vim-wiki.mawercer.de/wiki/topic/vim%20plugin%20managment.html)
which is why Marc Weber thinks that it doesn't make sense to repeat the same
repetitive information everywhere.

*Recommended way:*
[vim-addon-manager](https://github.com/MarcWeber/vim-addon-manager) (because Marc Weber wrote it for exactly
this reason, it supports simple dependency management). E.g. you're done by this
line in your `.vimrc`:

```vim
" assuming you want to use snipmate snippet engine
ActivateAddons vim-snippets snipmate
```

[vim-pi](https://bitbucket.org/vimcommunity/vim-pi/issue/90/we-really-need-a-web-interface)
Is the place to discuss plugin managers and repository resources.

About how to install snipMate see [snipmate@garbas](https://github.com/garbas/vim-snipmate)

(Bundle, Pathogen, git clone - keywords to make people find this link by ctrl-f search)
I know that I should be reading the docs of the snippet engine, just let me copy paste into my `.vimrc`:
[See this pull request](https://github.com/honza/vim-snippets/pull/307/files).

TROUBLE
=======

If you still have trouble getting this to work create a GitHub ticket, ask on
IRC or the mailing list.

Policies / for contributors
---------------------------

Some snippets are useful for almost all languages, so let's try to have the same
triggers for them:

```
if : if without else
ife: if $1 else $2
eif : else if ($1) { .. }
el  : else ..
wh  : while (cond) ...
```

Don't add useless placeholder default texts like:

```
if (${1:condition}){
  ${2:some code here}
}
```
instead use:

```
if (${1}){
  ${2}
}
```

Exception: Functions which are used less often, such as Vim's `matchall()`, `matchstr()`
functions which case hints may be helpful to remember order. In the VimL case
get vim-dev plugin which has function completion

Thus for conditions (while, if ..) and block bodies just use ${N} - Thanks

Open questions:
What about one line if ee then .. else .. vs if \n .. then \n ... \n else \n .. ?
Which additional policies to add?
Discuss at: https://github.com/honza/vim-snippets/issues/230

*folding markers*:
Until further work is done on `vim-snipmate`, please don't add folding markers
into snippets. `vim-snipmate` has some comments about how to patch all snippets
on the fly adding those.

Currently all snippets from UltiSnips have been put into UltiSnips - some work
on merging should be done (dropping duplicates etc). Also see engines section above.

Related repositories
--------------------

We also encourage people to maintain sets of snippets for particular use cases
so that all users can benefit from them.  People can list their snippet repositories here:

* https://github.com/rbonvall/snipmate-snippets-bib (snippets for BibTeX files)
* https://github.com/sudar/vim-arduino-snippets (snippets for Arduino files)
* https://github.com/zedr/zope-snipmate-bundle.git (snippets for Python, TAL and ZCML)
* https://github.com/bonsaiben/bootstrap-snippets (snippets for Twitter Bootstrap markup, in HTML and Haml)

Installation using VAM: https://github.com/MarcWeber/vim-addon-manager

Future - ideas - examples
-------------------------

[overview snippet engines](http://vim-wiki.mawercer.de/wiki/topic/text-snippets-skeletons-templates.html)
If you have ideas you can add them to that list of "snippet engine features by example".

Historical notes
----------------

[vim-snipmate][1] was originally started by [Michael Sanders][2] who has now
unfortunately abandoned the project. [Rok Garbas][3] is now maintaining a
[fork][4] of the project in hopes of improving the existing code base.

Versions / dialects / ..
========================

There are some issues, such as newer language versions may require other
snippets than older. If this exists we currently recommend doing this:

* add snippets/ruby.snippets (common snippets)
* add snippets/ruby-1.8.snippets (1.8 only)
* add snippets/ruby-1.9.snippets (1.9 only)

then configure https://github.com/garbas/vim-snipmate this way:

```vim
let g:snipMate = {}
let g:snipMate.scope_aliases = {}
let g:snipMate.scope_aliases['ruby'] = 'ruby,ruby-rails,ruby-1.9'
```

If it happens that you work on a project requiring ruby-1.8 snippets instead,
consider using `vim-addon-local-vimrc` and override the filetypes.

Well - of course it may not make sense to create a new file for each
ruby-library-version triplet. Sometimes postfixing a name such as

```
migrate_lib_20_down
migrate_lib_20_up
```

will do it then if syntax has changed.

Language maintainers
--------------------

No one can really be proficient in all programming languages. If you would like
to maintain snippets for a language, please get in touch.

Notes: People are interested in snippets - and their interest may wane again.
This list is kept up-to-date on a best effort basis.

* Clojure - [lpil](https://github.com/lpil)
* Elixir - [iurifq](https://github.com/iurifq)
* Falcon - [steveno](https://github.com/steveno)
* HTML Django - [honza](http://github.com/honza)
* Javascript - [honza](http://github.com/honza)
* Markdown - [honza](http://github.com/honza)
* PHP - [chrisyue](http://github.com/chrisyue)
* Python - [honza](http://github.com/honza)
* Ruby - [taq](http://github.com/taq)
* Rust - [lpil](https://github.com/lpil)
* Scala - [gorodinskiy](https://github.com/gorodinskiy)

License
-------

Just as the original snipMate plugin, all the snippets are licensed under the
terms of the MIT license.

[1]: http://github.com/garbas/vim-snipmate
[2]: http://github.com/msanders
[3]: http://github.com/garbas
[4]: http://github.com/garbas/vim-snipmate
[7]: http://github.com/SirVer/ultisnips
