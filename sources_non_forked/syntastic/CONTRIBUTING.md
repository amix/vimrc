# Bug reports / Github issues

When reporting a bug make sure you search the existing github issues for the
same/similar issues. If you find one, feel free to add a `+1` comment with any
additional information that may help us solve the issue.

When creating a new issue be sure to state the following:

* Steps to reproduce the bug.
* The version of vim you are using.
* The version of syntastic you are using.

For syntax checker bugs also state the version of the checker executable that you are using.

# Submitting a patch

* Fork the repo on github
* Make a [topic branch](https://github.com/dchelimsky/rspec/wiki/Topic-Branches#using-topic-branches-when-contributing-patches) and start hacking
* Submit a pull request based off your topic branch

Small focused patches are preferred.

Large changes to the code should be discussed with the core team first. Create an issue and explain your plan and see what we say.

# General style notes

Following the coding conventions/styles used in the syntastic core:

* Use 4 space indents.
* Don't use abbreviated keywords - e.g. use `endfunction`, not `endfun` (there's always room for more fun!).
* Don't use `l:` prefixes for variables unless actually required (i.e. almost never).
* Code for maintainability. We would rather a function be a couple of lines longer and have (for example) some [explaining variables](http://www.refactoring.com/catalog/introduceExplainingVariable.html) to aid readability.

# Syntax checker style notes

The preferred style for error format strings is one "clause" per line. E.g.
(from the coffeelint checker):

```viml
let errorformat = '%E%f:%l:%c: %trror: %m,' .
            \ 'Syntax%trror: In %f\, %m on line %l,' .
            \ '%EError: In %f\, Parse error on line %l: %m,' .
            \ '%EError: In %f\, %m on line %l,' .
            \ '%W%f(%l): lint warning: %m,' .
            \ '%W%f(%l): warning: %m,' .
            \ '%E%f(%l): SyntaxError: %m,' .
            \ '%-Z%p^,' .
            \ '%-G%.%#'
```
