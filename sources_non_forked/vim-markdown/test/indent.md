1. Confirm indent with new line insert after list items

'\' is not list item.
\ foo

If only space and three '*' or '-' character are in the line,
this line means horizontal item.
If current line is below horizontal item, it need not to indent.
Following example is horizontal item.

---
***
- - -
* * *

And list item must be specified space after [*-+].
Following example is list item.

* foo
- bar
+ baz

But following example is not list item.
*foo
-bar
+baz
