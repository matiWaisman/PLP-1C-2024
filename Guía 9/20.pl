% Dado un predicado unario P sobre números naturales, definir un predicado que determine el mínimo X que satisfaga P(X).

% Sup que ese predicado unario es esPar: 
% esPar(?X)
esPar(X) :- desde2(0, X), X mod 2 =:= 0.

% desde2(+X,?Y)
desde2(X, X).
desde2(X,Y) :- nonvar(Y), X < Y.
desde2(X,Y) :- var(Y), N is X + 1, desde2(N,Y).

% minimo(?X)
minimo(X) :- esPar(X), not((between(0, X, Y), Y < X, esPar(Y))).