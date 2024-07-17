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

% cuadradoMagico(+N, -XS), que instancia XS con cuadrados cuyas filas y columnas suman todas un mismo valor.

% iésimo(?I, +L, -X), donde X es el I-ésimo elemento de la lista L. 
iesimo(1, [X|_], X).
iesimo(I, [_|L], X) :- iesimo(J, L, X), I is J + 1.

% Devuelve la lista de la columna de una matriz
% columna(+C, +Filas, +Matriz, -L)
columna(C, 1, Matriz, [X]) :- iesimo(1, Matriz, L), iesimo(C, L, X).
columna(C, Filas, Matriz, L) :- Filas > 1, iesimo(Filas, Matriz, Fila), iesimo(C, Fila, X), F is Filas - 1,
                                columna(C, F, Matriz, CasiLista), append([X], CasiLista, L).

% columnasSuman(+Columnas, +Target, +Matriz)
columnasSuman(1, Target, Matriz) :- longitud(Matriz, L), columna(1, L, Matriz, Columna), 
                                    sumlist(Columna, Sum), Sum =:= Target.
columnasSuman(Columnas, Target, Matriz) :- Columnas > 1, longitud(Matriz, L), columna(Columnas, L, Matriz, Columna), 
                                           sumlist(Columna, Sum), Sum =:= Target, 
                                           C is Columnas - 1, columnasSuman(C, Target, Matriz).

% filaSuma(+Matriz, -S) 
filaSuma(Matriz, S) :- iesimo(1, Matriz, Fila), sumlist(Fila, S).

% cuadradoMagico(+N, -XS)
cuadradoMagico(N, XS) :- cuadradoSemiLatino(N, XS), filaSuma(XS, S), columnasSuman(N, S, XS). 