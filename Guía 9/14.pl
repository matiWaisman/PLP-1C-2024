% parSuma(+Z, -X, -Y)
parSuma(Z, X, Y) :- S1 is Z - 1, between(1, S1, X), Y is Z - X. % El S1 es para que Y no sea 0.

% desde2(+X,?Y)
desde2(X, X).
desde2(X,Y) :- nonvar(Y), X < Y.
desde2(X,Y) :- var(Y), N is X + 1, desde2(N,Y).

% generarPares(-X, -Y)
generarPares(X, Y) :- desde2(2, Z), parSuma(Z, X, Y).

% coprimos(-X, -Y)
coprimos(X,Y) :- generarPares(X, Y), gcd(X,Y) =:= 1.

