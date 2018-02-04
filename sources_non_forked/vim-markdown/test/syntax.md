# Fenced code living in an indented environment is correctly highlighted

1. run this command to do this:

    ```
some command
    ```

2. Subsequent list items are correctly highlighted.

Fenced code block with language:

```ruby
def f
  0
end
```

# Links

[a](b "c")

[a]()

[good spell](a)

[badd spell](a)

[a](b "c")

[a]( b
"c" )

a (`a`) b. Fix: <https://github.com/plasticboy/vim-markdown/issues/113>

Escaped:

\[a](b)

[a\]b](c)

## Known failures

Escape does not work:

[a\](b)

Should not be links because of whitespace:

[a] (b)

[a](a
b)

[a](a b)

# Reference links

Single links:

[a][b]

[good spell][a]

[badd spell][a]

[a][]

[a] []

[a][b] c [d][e]

Reference link followed by inline link:

[a] [b](c)

## Known failures

Should be shortcut reference links:

[a]

[a] b [c]

Should be a single link:

[a] [b]

[a] b [c](d)
