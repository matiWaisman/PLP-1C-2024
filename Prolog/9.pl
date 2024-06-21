% elementosTomadosEnOrden(+L,+N,-Elementos) que tenga éxito si L es una lista, N ≥ 0 y Elementos es una lista de N elementos de L, preservando el orden en que aparecen en la lista original.
elementosTomadosEnOrden(_, 0, []).
elementosTomadosEnOrden([X|L], N, Elementos) :- M is N - 1, elementosTomadosEnOrden(L, M, EnOrden), append([X], EnOrden, Elementos).
elementosTomadosEnOrden([_|L], N, Elementos) :- longitud(L, Len), Len > 0, elementosTomadosEnOrden(L, N, Elementos), longitud(Elementos, N).
% Tira repetidos :/
longitud([], 0).
longitud([_|XS], Long) :- longitud(XS, J), Long is J + 1.

% Este no funciona :/
elementosTomadosEnOrden2(_, 0, []).
elementosTomadosEnOrden2(L, N, Elementos) :- append(L1, L2, L), L1 \= [], longitud(L1, Len1), Len1 =< N, M is N - Len1, elementosTomadosEnOrden2(L2, M, E1), append(L1, E1, Elementos), longitud(Elementos, N).