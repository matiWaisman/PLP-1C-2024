% Un cuadrado semi-latino es una matriz cuadrada de naturales (incluido el cero) donde todas las filas de la matriz suman lo mismo.

% Definir el predicado cuadradoSemiLatino(+N, -XS). El predicado debe ir devolviendo matrices (utilizando la representación antes mencionada), que sean cuadrados semi-latinos de dimensión N*N. Dichas matrices deben devolverse de manera ordenada: primero aquellas cuyas filas suman 0, luego 1, luego 2, etc.

longitud([], 0).
longitud([_|XS], Long) :- longitud(XS, J), Long is J + 1.


% armarLista(+Len, +Suma, -L)
armarLista(0, _, []).
armarLista(Len, Suma, L) :- Len > 0, between(0, Suma, X), Len2 is Len - 1, S is Suma - X, 
                            armarLista(Len2, S, Lp), append([X], Lp, L), sumlist(L, Suma).

% armarMatriz(+SizeFila, +FilasRestantes, +Suma, -Matriz)
armarMatriz(_, 0, _, []).
armarMatriz(SizeFila, Filas, Suma, Matriz) :- Filas > 0 , armarLista(SizeFila, Suma, L), 
                                              Fr is Filas - 1, armarMatriz(SizeFila, Fr, Suma, CasiMatriz),
                                              append([L], CasiMatriz, Matriz).


% desde2(+X,?Y)
desde2(X, X).
desde2(X,Y) :- nonvar(Y), X < Y.
desde2(X,Y) :- var(Y), N is X + 1, desde2(N,Y).

% cuadradoSemiLatino(+N, -XS).
cuadradoSemiLatino(N, XS) :- desde2(0, X), armarMatriz(N, N, X, XS).