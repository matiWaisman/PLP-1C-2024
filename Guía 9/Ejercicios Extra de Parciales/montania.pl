% montania(+L, -L1, -C, -L2)
montania(L, L1, C, L2) :- append(L1, [C|L2], L), creciente(L1), reverse(L2, L2r), creciente(L2r), mayorATodos(C, L).

% creciente(+L)
creciente([]).
creciente([X|XS]) :- not((member(Y, XS), Y \= X, Y < X)), creciente(XS).

mayorATodos(E, L) :- not((member(El, L), El > E)).

% desde(+X, -Y).
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% todasLasListas(+A, -L)
todasLasListas(A, L) :- desde(1, Y), armarLista(Y, A, L).

armarLista(0, _, []).
armarLista(I, A, [X|L]) :- I > 0, member(X, A), I2 is I-1, armarLista(I2, A, L).