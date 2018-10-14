# abolish.vim

I sat on this plugin for 3 years before releasing it, primarily
because it's so gosh darn hard to explain.  It's three superficially
unrelated plugins in one that share a common theme: working with
variants of a word.

## Abbreviation

I know how to spell "separate".  I know how to spell "desperate".  My
fingers, however, have trouble distinguishing between the two, and I
invariably have a 50 percent chance of typing "seperate" or "desparate"
each time one of these comes up.  At first, I tried abbreviations:

    :iabbrev  seperate  separate
    :iabbrev desparate desperate

But this falls short at the beginning of a sentence.

    :iabbrev  Seperate  Separate
    :iabbrev Desparate Desperate

To be really thorough, we need uppercase too!

    :iabbrev  SEPERATE  SEPARATE
    :iabbrev DESPARATE DESPERATE

Oh, but consider the noun form, and the adverb form!

    :iabbrev  seperation  separation
    :iabbrev desparation desperation
    :iabbrev  seperately  separately
    :iabbrev desparately desperately
    :iabbrev  Seperation  separation
    :iabbrev Desparation Desperation
    :iabbrev  Seperately  Separately
    :iabbrev Desparately Desperately
    :iabbrev  SEPERATION  SEPARATION
    :iabbrev DESPARATION DESPERATION
    :iabbrev  SEPERATELY  SEPARATELY
    :iabbrev DESPARATELY DESPERATELY

Wait, there's also "separates", "separated", "separating",
"separations", "separator"...

Abolish.vim provides a simpler way.  The following one command produces
48 abbreviations including all of the above.

    :Abolish {despa,sepe}rat{e,es,ed,ing,ely,ion,ions,or}  {despe,sepa}rat{}

My current configuration has 25 Abolish commands that create hundreds of
corrections my fingers refuse to learn.

## Substitution

One time I had an application with a domain model called
"facility" that needed to be renamed to "building". So, a simple
search and replace, right?

    :%s/facility/building/g

Oh, but the case variants!

    :%s/Facility/Building/g
    :%s/FACILITY/BUILDING/g

Wait, the plural is more than "s" so we need to get that too!

    :%s/facilities/buildings/g
    :%s/Facilities/Buildings/g
    :%s/FACILITIES/BUILDINGS/g

Abolish.vim has your back.  One command to do all six, and you can
repeat it with `&` too!

    :%Subvert/facilit{y,ies}/building{,s}/g

From a conceptual level, one way to think about how this substitution
works is to imagine that in the braces you are declaring the
requirements for turning that word from singular to plural.  In
the facility example, the same base letters in both the singular
and plural form of the word are `facilit` To turn "facility" to a
plural word you must change the `y` to `ies` so you specify
`{y,ies}` in the braces.

To convert the word "building" from singular to plural, again
look at the common letters between the singular and plural forms:
`building`.  In this case you do not need to remove any letter
from building to turn it into plural form and you need to
add an `s` so the braces should be `{,s}`.

A few more examples:

Address to Reference

    :Subvert/address{,es}/reference{,s}/g

Blog to Post (you can just do this with a regular :s also)

    :Subvert/blog{,s}/post{,s}/g

Child to Adult

    :Subvert/child{,ren}/adult{,s}/g

Be amazed as it correctly turns the word children into the word adults!

Die to Spinner

    :Subvert/di{e,ce}/spinner{,s}/g

You can abbreviate it as `:S`, and it accepts the full range of flags
including things like `c` (confirm).

There's also a variant for searching and a variant for grepping.

## Coercion

Want to turn `fooBar` into `foo_bar`?  Press `crs` (coerce to
snake\_case).  MixedCase (`crm`), camelCase (`crc`), snake\_case
(`crs`), UPPER\_CASE (`cru`), dash-case (`cr-`), dot.case (`cr.`),
space case (`cr<space>`), and Title Case (`crt`) are all just 3
keystrokes away.

## Installation

If you don't have a preferred installation method, I recommend
installing [pathogen.vim](https://github.com/tpope/vim-pathogen), and
then simply copy and paste:

    cd ~/.vim/bundle
    git clone git://github.com/tpope/vim-abolish.git

Once help tags have been generated, you can view the manual with
`:help abolish`.

## Self-Promotion

Like abolish.vim? Follow the repository on
[GitHub](https://github.com/tpope/vim-abolish) and vote for it on
[vim.org](http://www.vim.org/scripts/script.php?script_id=1545).  And if
you're feeling especially charitable, follow [tpope](http://tpo.pe/) on
[Twitter](http://twitter.com/tpope) and
[GitHub](https://github.com/tpope).

## License

Copyright (c) Tim Pope.  Distributed under the same terms as Vim itself.
See `:help license`.
