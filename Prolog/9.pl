% elementosTomadosEnOrden(+L,+N,-Elementos) que tenga éxito si L es una lista, N ≥ 0 y Elementos es una lista de N elementos de L, preservando el orden en que aparecen en la lista original.
elementosTomadosEnOrden(_, 0, []).
elementosTomadosEnOrden([X|L], N, Elementos) :- elementosTomadosEnOrden(L, M, EnOrden), N is M + 1, append([X], EnOrden, Elementos).
elementosTomadosEnOrden([_|L], N, Elementos) :- elementosTomadosEnOrden(L, N, Elementos), longitud(Elementos, N).
% Tira repetidos :/
longitud([], 0).
longitud([_|XS], Long) :- longitud(XS, J), Long is J + 1.