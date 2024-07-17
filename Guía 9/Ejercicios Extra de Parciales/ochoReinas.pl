%% ochoReinas(?XS)
ochoReinas(XS) :- length(XS, 8), noSeAtacan([],XS).

%% noSeAtacan(+Visitados, ?XS) Uso Visitados para guardar a los "ya visitados" para despues saber la posicion de cada uno
noSeAtacan(_,[]).
noSeAtacan(Visitados,[X|XS]) :- between(1, 8, X), not(member(X, Visitados)), append(Visitados, [X], ReVisitados),
                                not((member(E, Visitados), seAtacan(E,X, ReVisitados))),
                                noSeAtacan(ReVisitados, XS).

%% seAtacan(+P1, +P2, +L)
seAtacan(F1, F2, _) :- F1 = F2.
seAtacan(F1, F2, L) :- nth1(C1, L, F1), nth1(C2, L, F2), C1 > C2, Dif is (C1 - C2), (F2 + Dif) =:= C1.
seAtacan(F1, F2, L) :- nth1(C1, L, F1), nth1(C2, L, F2), C2 > C1, Dif is (C2 - C1), (F1 + Dif) =:= C2.