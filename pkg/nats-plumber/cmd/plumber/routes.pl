
url(U) :- sub_atom(U,0,_,_,'http://').
url(U) :- sub_atom(U,0,_,_,'https://').

send(msg('browser.open',Url,Headers)) :-
  plumb(msg('plumb.click',Url,Headers)),
  url(Url).
