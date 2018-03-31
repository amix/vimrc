Before reporting a bug, you should try stripping down your Vim configuration
and removing other plugins.  The sad truth about VimScript is that it is
fraught with incompatibilities waiting to happen.  I'm happy to work around
them where I can, but it's up to you to isolate the conflict.

Fugitive is particularly prone to regressions due to Git version issues,
platform issues, and interactions with other plugins.  I end up bisecting a
lot more than other projects, and thus I'm especially meticulous here about
maintaining a clean, readable, history.  Squash and force push any requested
changes to a pull request.  And if your [commit message
sucks](https://commit.style), I'm not going to accept it.  Period.

If your contribution involves adding a configuration option, you are going to
need a very compelling justification for it.  Options add a maintenance
burden, support burden, and documentation bloat, and oftentimes can be
achieved much more simply with a custom map or autocommand.  If your option
controls an underlying Git command, ask yourself why Git itself does not offer
such configuration.

Beyond that, don't be shy about asking before patching.  What takes you hours
might take me minutes simply because I have both domain knowledge and a
perverse knowledge of VimScript so vast that many would consider it a symptom
of mental illness.  On the flip side, some ideas I'll reject no matter how
good the implementation is.  "Send a patch" is an edge case answer in my book.
