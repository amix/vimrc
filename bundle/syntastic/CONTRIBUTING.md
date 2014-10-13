# CONTRIBUTING
- - -
1\. [Bug reports / GitHub issues](#bugreps)  
2\. [Submitting a patch](#patches)  
3\. [General style notes](#generalstyle)  
4\. [Syntax checker notes](#checkerstyle)  
- - -

<a name="bugreps"></a>

## 1. Bug reports / GitHub issues

Please note that the preferred channel for posting bug reports is the
[issue tracker at GitHub][0].  Reports posted elsewhere are less likely
to be seen by the core team.

When reporting a bug make sure you search the existing GitHub issues
for the same/similar issues.  If you find one, feel free to add a `+1`
comment with any additional information that may help us solve the
issue.

When creating a new issue be sure to state the following:

* steps to reproduce the bug;
* the version of Vim you are using (run `:ver` to find out);
* the version of syntastic you are using (see `:SyntasticInfo`).

For syntax checker bugs also state the version of the checker executable
that you are using.  Adding debugging information is typically useful
too:

* open a file handled by your checker;
* set `g:syntastic_debug` to 1 or 3;
* run the checker;
* copy the output of `:mes`.

<a name="patches"></a>

## 2. Submitting a patch

Before you consider adding features to syntastic, _please_ spend a few
minutes (re-)reading the latest version of the [manual][1].  Syntastic
is changing rapidly at times, and it's quite possible that some of the
features you want to add exist already.

To submit a patch:

* fork the [repo][2] on GitHub;
* make a [topic branch][3] and start hacking;
* submit a pull request based off your topic branch.

Small, focused patches are preferred.

Large changes to the code should be discussed with the core team first.
Create an issue and explain your plan and see what we say.

Also make sure to update the manual whenever applicable.  Nobody can use
features that aren't documented.

<a name="generalstyle"></a>

## 3. General style notes

Follow the coding conventions/styles used in the syntastic core:

* use 4 space indents;
* don't use abbreviated keywords - e.g. use `endfunction`, not `endfun`
(there's always room for more fun!);
* don't use `l:` prefixes for variables unless actually required (i.e.
almost never);
* code for maintainability; we would rather a function be a couple of
lines longer and have (for example) some [explaining variables][4] to
aid readability.

<a name="checkerstyle"></a>

## 4. Syntax checker notes

Make sure to read the [guide][5] if you plan to add new syntax checkers.

Use the existing checkers as templates, rather than writing everything
from scratch.

The preferred style for error format strings is one "clause" per line.
E.g. (from the `coffee` checker):

```vim
let errorformat =
    \ '%E%f:%l:%c: %trror: %m,' .
    \ 'Syntax%trror: In %f\, %m on line %l,' .
    \ '%EError: In %f\, Parse error on line %l: %m,' .
    \ '%EError: In %f\, %m on line %l,' .
    \ '%W%f(%l): lint warning: %m,' .
    \ '%W%f(%l): warning: %m,' .
    \ '%E%f(%l): SyntaxError: %m,' .
    \ '%-Z%p^,' .
    \ '%-G%.%#'
```

[0]: https://github.com/scrooloose/syntastic/issues
[1]: https://github.com/scrooloose/syntastic/blob/master/doc/syntastic.txt
[2]: https://github.com/scrooloose/syntastic
[3]: https://github.com/dchelimsky/rspec/wiki/Topic-Branches#using-topic-branches-when-contributing-patches
[4]: http://www.refactoring.com/catalog/extractVariable.html
[5]: https://github.com/scrooloose/syntastic/wiki/Syntax-Checker-Guide
