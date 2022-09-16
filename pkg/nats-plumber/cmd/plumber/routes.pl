

browser_schema --> "http".
browser_schema --> "https".
anything([C|Cs]) --> [C], anything(Cs).
anything("") --> "".
browser_url --> browser_schema, "://", anything(_).

working_dir_host([C|Cs]) --> [C], { [C] \= "/" }, working_dir_host(Cs).
working_dir_host("") --> "".
working_dir_path(P) --> anything(P).
working_dir(Host,Path) --> "file://", working_dir_host(Host), working_dir_path(Path).

send(msg('browser.open',Url,Headers)) :-
  plumb(msg('plumb.click',Url,Headers)),
  atom_chars(Url,Chars),
  phrase(browser_url,Chars).
