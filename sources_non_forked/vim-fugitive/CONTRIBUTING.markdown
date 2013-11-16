Before reporting a bug, you should try stripping down your Vim configuration
and removing other plugins.  The sad truth about VimScript is that it is
fraught with incompatibilities waiting to happen.  I'm happy to work around
them where I can, but it's up to you to isolate the conflict.

If your [commit message sucks](http://stopwritingramblingcommitmessages.com/),
I'm not going to accept your pull request.  I've explained very politely
dozens of times that
[my general guidelines](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)
are absolute rules on my own repositories, so I may lack the energy to explain
it to you yet another time.  And please, if I ask you to change something,
`git commit --amend`.

Beyond that, don't be shy about asking before patching.  What takes you hours
might take me minutes simply because I have both domain knowledge and a
perverse knowledge of VimScript so vast that many would consider it a symptom
of mental illness.  On the flip side, some ideas I'll reject no matter how
good the implementation is.  "Send a patch" is an edge case answer in my book.
