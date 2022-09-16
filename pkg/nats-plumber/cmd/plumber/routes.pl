

browser_schema --> "http".
browser_schema --> "https".
anything --> [_], anything.
anything --> "".
browser_url --> browser_schema, "://", anything.

send(msg('browser.open',Url,Headers)) :-
  plumb(msg('plumb.click',Url,Headers)),
  atom_chars(Url,Chars),
  phrase(browser_url,Chars).
