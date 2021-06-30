### Version 003 (October 10, 2014)

Almost 3 years' worth of fixes and (hopefully) improvements.

### Version 002 (December 5, 2011)

Added binary numbers (0b0101) and fixed some bugs (#9, #62, #63, #65).

### Version 001 (October 18, 2011)

Removed deprecated `coffee_folding` option, added `coffee_compile_vert` option,
split out compiler, fixed indentation and syntax bugs, and added Haml support
and omnicompletion.

 - The coffee compiler is now a proper vim compiler that can be loaded with
   `:compiler coffee`.
 - The `coffee_compile_vert` option can now be set to split the CoffeeCompile
   buffer vertically by default.
 - CoffeeScript is now highlighted inside the `:coffeescript` filter in Haml.
 - Omnicompletion (`:help compl-omni`) now uses JavaScript's dictionary to
   complete words.
 - We now have a fancy version number.
