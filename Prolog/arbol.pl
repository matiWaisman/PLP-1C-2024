arbol(bin(bin(bin(nil, 4, nil), 2, bin(nil, 5, bin(nil, 6, nil))), 1, bin(bin(nil, 7, nil), 3, nil))).

%% camino(+A, -C)
camino(bin(nil,V,nil), [V]).
camino(bin(I,V,_), [V|C]) :- I \= nil, camino(I, C).
camino(bin(_,V,D), [V|C]) :- D \= nil, camino(D, C).

%% caminoMasLargo(+A, -C)
caminoMasLargo(A, C) :- camino(A, C), not((camino(A, B), C \= B, length(B, Lb), length(C, Lc), Lb > Lc)).

%% caminoUnicoDeLong(+A, +N, -C)
caminoUnicoDeLong(A,N,C) :- camino(A, C), length(C, N), not((camino(A, B), C \= B, length(B, Lb), Lb =:= N)).