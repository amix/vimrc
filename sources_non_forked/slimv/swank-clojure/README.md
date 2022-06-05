# Swank Clojure

[Swank Clojure](http://github.com/technomancy/swank-clojure) is a
server that allows [SLIME](http://common-lisp.net/project/slime/) (the
Superior Lisp Interaction Mode for Emacs) to connect to Clojure
projects. To use it you must launch a swank server, then connect to it
from within Emacs.

## Usage

If you just want a standalone swank server with no third-party
libraries, you can just install swank-clojure using Leiningen.

    $ lein plugin install swank-clojure 1.3.0-SNAPSHOT
    $ ~/.lein/bin/swank-clojure

    M-x slime-connect

If you put ~/.lein/bin on your $PATH it's even more convenient.

You can also start a swank server from inside your project:

    $ lein swank # you can specify PORT and HOST optionally

Note that the lein-swank plugin now comes with Swank Clojure; it does
not need to be specified as a separate dependency any more.

If you're using Maven, add this to your pom.xml under the
\<dependencies\> section:

    <dependency>
      <groupId>swank-clojure</groupId>
      <artifactId>swank-clojure</artifactId>
      <version>1.2.1</version>
    </dependency>

Then you can launch a swank server like so:

    $ mvn -o clojure:swank

Note that due to a bug in clojure-maven-plugin, you currently cannot
include it as a test-scoped dependency; it must be compile-scoped. You
also cannot change the port from Maven; it's hard-coded to 4005.

Put this in your Emacs configuration to get syntax highlighting in the
slime repl:

    (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

## Connecting with SLIME

Install the "slime-repl" package using package.el. If you are using
Emacs 23, it's best to get [the latest version of package.el from
Emacs
trunk](http://bit.ly/pkg-el). Then
add Marmalade as an archive source:

    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/") t)

Then you can do <kbd>M-x package-list-packages</kbd>. Go down to
slime-repl and mark it with <kbd>i</kbd>. Execute the installation by
pressing <kbd>x</kbd>.

When you perform the installation, you will see warnings related to
the byte-compilation of the packages. This is **normal**; the packages
will work just fine even if there are problems byte-compiling it upon
installation.

Then you should be able to connect to the swank server you launched:

    M-x slime-connect

It will prompt you for your host (usually localhost) and port. It may
also warn you that your SLIME version doesn't match your Swank
version; this should be OK.

Having old versions of SLIME either manually installed or installed
using a system-wide package manager like apt-get may cause issues.

## SLIME Commands

Commonly-used SLIME commands:

* **C-c TAB**: Autocomplete symbol at point
* **C-x C-e**: Eval the form under the point
* **C-c C-k**: Compile the current buffer
* **C-c C-l**: Load current buffer and force dependent namespaces to reload
* **M-.**: Jump to the definition of a var
* **C-c S-i**: Inspect a value
* **C-c C-m**: Macroexpand the call under the point
* **C-c C-d C-d**: Look up documentation for a var
* **C-c C-z**: Switch from a Clojure buffer to the repl buffer
* **C-c M-p**: Switch the repl namespace to match the current buffer
* **C-c C-w c**: List all callers of a given function

Pressing "v" on a stack trace a debug buffer will jump to the file and
line referenced by that frame if possible.

Note that SLIME was designed to work with Common Lisp, which has a
distinction between interpreted code and compiled code. Clojure has no
such distinction, so the load-file functionality is overloaded to add
<code>:reload-all</code> behaviour.

## Embedding

You can embed Swank Clojure in your project, start the server from
within your own code, and connect via Emacs to that instance:

    (ns my-app
      (:require [swank.swank]))
    (swank.swank/start-repl) ;; optionally takes a port argument

Then use M-x slime-connect to connect from within Emacs.

You can also start the server directly from the "java" command-line
launcher if you AOT-compile it and specify "swank.swank" as your main
class.

## Debug Repl

For now, see [Hugo Duncan's
blog](http://hugoduncan.org/post/2010/swank_clojure_gets_a_break_with_the_local_environment.xhtml)
for an explanation of this excellent feature. Further documentation to come.

## swank-clojure.el

Previous versions of Swank Clojure bundled an Elisp library called
swank-clojure.el that provided ways to launch your swank server from
within your Emacs process. It's much more reliable to launch the
server from your build tool, so this has been removed.

## Community

The [mailing list](http://groups.google.com/group/swank-clojure) and
clojure channel on Freenode are the best places to bring up
questions/issues.

Contributions are preferred as either Github pull requests or using
"git format-patch". Please use standard indentation with no tabs,
trailing whitespace, or lines longer than 80 columns. See [this post
on submitting good patches](http://technomancy.us/135) for some
tips. If you've got some time on your hands, reading this [style
guide](http://mumble.net/~campbell/scheme/style.txt) wouldn't hurt
either.

## License

Copyright (C) 2008-2011 Jeffrey Chu, Phil Hagelberg, Hugo Duncan, and
contributors

Licensed under the EPL. (See the file COPYING.)
