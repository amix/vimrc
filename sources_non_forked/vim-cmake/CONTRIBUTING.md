# Contributing Guidelines

The easiest method to contribute to this project is by providing feedback,
requesting a feature or reporting a bug.  Prior to opening an issue, check the
existing ones, as there might be one that already covers your points.  When
you need to open a new one, provide as much information as you can, especially
for bug reports.

<!--=========================================================================-->

## Pull Requests

Do you feel like contributing code?  Awesome!  Find an open issue you want to
tackle (can be one of your own), then open a pull request following [these
guidelines][pr-guidelines].  Make sure to **test your changes thoroughly** and,
if needed,
* update the documentation in `doc/cmake.txt`
* generate help tags with `vim -u NONE -c "helptags doc | q"`
* update the README
* update the "Unreleased" section in the CHANGELOG

<!--=========================================================================-->

## Coding Style

In case you are planning to submit a pull request, please keep your changes
minimal and maintain a clear coding style.  Name your functions and variables
sensibly, comment non-obvious lines of code and match the formatting style of
the rest of the code (indentation, line width, spacing, etc.).

This project adheres to the [Google Vimscript Style Guide][style-guide], with a
few exceptions:
* use four spaces for indents (not two)
* indent continued lines by eight spaces (not four)

You can use [Vint][vint] and run `vint .` in the repository root to check the
code against the above style guide.

<!--=========================================================================-->

[pr-guidelines]: https://opensource.guide/how-to-contribute/#opening-a-pull-request
[style-guide]: https://google.github.io/styleguide/vimscriptguide.xml
[vint]: https://github.com/Vimjas/vint
