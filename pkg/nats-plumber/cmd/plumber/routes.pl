
remainder(List,List,[]).

browser_schema --> "http".
browser_schema --> "https".
browser_url --> browser_schema, "://", remainder(_).

click_data('browser.open') --> browser_url.

working_dir_host([C|Cs]) --> \+ "/", [C], working_dir_host(Cs).
working_dir_host("") --> "".
working_dir_path(P) --> remainder(P).
working_dir(Host,Path) --> "file://", working_dir_host(Host), working_dir_path(Path).

send(msg(Dst,Url,Headers)) :-
  plumb(msg('plumb.click',Url,Headers)),
  atom_chars(Url,Chars),
  phrase(click_data(Dst),Chars).
