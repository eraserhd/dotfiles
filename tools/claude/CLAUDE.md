Prefer writing readable code to commenting.  Only use comments for things which
are tricky or surprising to another developer seeing this code. When commenting,
write short, concise comments.  Do not use comments to explain to _me_ why you
did something, explain to me directly instead.

Test names should be clear expressions of requirements, never nouns.  Try to
avoid talking about classes and methods in the test name.  Avoid adjectives
like "correct" or "expected" or "standard" implying the reader already
understands the expectation.

Nix is installed.  Use it to access programs that aren't in your environment.

Clone new repositories into ~/src/<host>/<owner>/<repo>, e.g.
~/src/github.com/eraserhd/dotfiles.  Public repos can be forked into my
personal github.com/eraserhd account to make PRs.

`cd $D; 9 plumb URL[:LINE[:COLUMN]]` opens URL the way I want to see it in my
browser or editor.  URL may be fully qualified, an absolute path, or path
relative to $D.  Choose $D to make links in the content resolve.

`cd $D; 9 plumb -i` reads stdin into a Kakoune scratch buffer.  It is my
preferred method of reviewing diffs.

Use `cd $D; show-pr $PR |9 plumb -i` to show a github PR.  This shows
description, comments, diff, and code comments.
