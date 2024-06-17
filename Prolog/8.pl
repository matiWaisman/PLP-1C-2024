% intersección(+L1, +L2, -L3), tal que L3 es la intersección sin repeticiones de las listas L1 y L2, respetando en L3 el orden en que aparecen los elementos en L1.
interseccion([], _, []).
interseccion([X|L1], L2, L3) :- member(X, L2), interseccion(L1, L2, LR), not(member(X, LR)), append([X], LR, L3).
interseccion([X|L1], L2, L3) :- member(X, L2), interseccion(L1, L2, LR), member(X, LR), borrar(LR, X, LRSX), append([X], LRSX, L3).
interseccion([X|L1], L2, L3) :- not(member(X,L2)), interseccion(L1, L2, L3).

% borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista ListaOriginal.
borrar([], _, []).
borrar([E|ListaOriginal], X, ListaSinXs) :- E \= X, borrar(ListaOriginal, X, LR), append([E], LR, ListaSinXs).
borrar([E|ListaOriginal], X, ListaSinXs) :- E == X, borrar(ListaOriginal, X, ListaSinXs).

% sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1
sacarDuplicados([], []).
sacarDuplicados([X|L1], L2) :- sacarDuplicados(L1, LSD), not(member(X, LSD)), append([X], LSD, L2).
sacarDuplicados([X|L1], L2) :- sacarDuplicados(L1, L2),member(X, L2).

% permutación(+L1, ?L2), que tiene éxito cuando L2 es permutación de L1.
% Hago de auxiliar el predicado insertar(?E, +L1, ?L2) que inserta el elemento E en alguna posicion de L1 resultando en L2
insertar(E, L1, L2) :- append(I, D, L1), append(I, [E|D], L2).

permutacion([], []).
permutacion([X|L1], L2) :- permutacion(L1, PL1), insertar(X, PL1, L2).

% reparto(+L, +N, -LListas) que tenga éxito si LListas es una lista de N listas (N ≥ 1) de cualquier longitud - incluso vacías - tales que al concatenarlas se obtiene la lista L.
% Hago de auxiliar el predicado longitud(+L1, ?Long)
longitud([], 0).
longitud([_|XS], Long) :- longitud(XS, J), Long is J + 1.

% Me traigo el predicado aplanar del ejercicio 6
% aplanar(+Xs, -Ys) 
aplanar([],[]).
% Si el primer elemento no es una lista.
aplanar([X|XS], YS) :- not(aplanar(X,_)), aplanar(XS, XSA), append([X], XSA, YS).
% Si el primer elemento es una lista.
aplanar([XS|LS], YS) :- aplanar(XS, XSA), aplanar(LS, LSA), append(XSA, LSA, YS).
% Funciona con ?L, ?N, +LListas, al reves de lo que pide la consigna :/
reparto(L, N, LListas) :- longitud(LListas, N), aplanar(LListas, L).

% Falta reparto y reparto sin vacias