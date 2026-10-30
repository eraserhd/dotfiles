Prefer writing readable code to commenting.  Only use comments for things which
are tricky or surprising to another developer seeing this code. When commenting,
write short, concise comments.  Do not use comments to explain to _me_ why you
did something, explain to me directly instead.

Test names should be clear expressions of requirements, never nouns.  Try to
avoid talking about classes and methods in the test name.  Avoid adjectives
like "correct" or "expected" or "standard" implying the reader already
understands the expectation.

Clone new repositories into ~/src/<host>/<owner>/<repo>, e.g.
~/src/github.com/eraserhd/dotfiles.  Public repos can be forked into my
personal github.com/eraserhd account to make PRs.

`cd $D; 9 plumb $URL` will open $URL the way I want to see it, either in my
browser or editor.  $URL may be a full URL, absolute path, or path relative
to $D.  Choose $D so that relative paths in $URL's content will resolve.

`cd $D; 9 plumb -i` will read stdin into a scratch buffer in my editor.  It
is my preferred method of reviewing diffs.
