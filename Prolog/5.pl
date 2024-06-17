% last(?L, ?U), donde U es el último elemento de la lista L.
last(_,U) :- append(_, [U], U).

% reverse(+L, -L1), donde L1 contiene los mismos elementos que L, pero en orden inverso.
reversemio([],[]).
reversemio([X|L], L1) :- append(L2, [X], L1), reversemio(L, L2).

% prefijo(?P, +L), donde P es prefijo de la lista L.
% prefijo([],_).
% prefijo([X|P], [Y|L]) :- append([X], L, [Y|L]), prefijo(P, L).
% Manera mas prolija:
prefijo(P, L) :- append(P,_,L).

% sufijo(?S, +L), donde S es sufijo de la lista L.
sufijo(S,L) :- append(_,S,L).

% sublista(?S, +L), donde S es sublista de L.
% Todo prefijo de sufijo de una lista es una sublista
sublista([], _).
sublista([X|XS], L) :- prefijo(R, L), sufijo([X|XS], R). % Haciendo esto exijo que el sufijo consista de al menos un elemento

% pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L.
% pertecene(X,[Y|L]) :- append([X], L, [Y|L]).
% pertecene(X, [_|L]) :- pertecene(X, L).  
% Forma mas elegante:
pertecene(X, L) :- sublista([X],L).
