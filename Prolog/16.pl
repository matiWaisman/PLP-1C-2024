% desde2(+X,?Y)
desde2(X, X).
desde2(X,Y) :- nonvar(Y), X < Y.
desde2(X,Y) :- var(Y), N is X + 1, desde2(N,Y).

% esTriángulo(+T) que, dada una estructura de la forma tri(A,B,C), indique si es un triángulo válido.
esTriangulo(tri(A,B,C)) :- A < B+C, B < A+C, C < B+A.

% generarTripla(+S, ?A, ?B, ?C)
generarTripla(S, A, B, C) :- desde2(3, S), Sprima is S - 2, between(1, Sprima, A), Sprimaprima is S - A - 1, 
                             between(1, Sprimaprima, B), C is S - A - B, C \= 0, A + B + C =:= S.
% perímetro(?T,?P), que es verdadero cuando T es un triángulo (válido) y P es su perímetro.
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A + B + C.
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), generarTripla(P, A,B,C), esTriangulo(tri(A,B,C)).

% triángulo(-T), que genera todos los triángulos válidos, sin repetir resultados.
triangulo(T) :- perimetro(T,_).
