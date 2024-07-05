% arbol(-A) que genere estructuras de árbol binario, dejando los valores de los nodos sin instanciar.
arbol(A) :- desde(0, X),  arbolDeN(X, A).  

% arbolDeN(+N, -A)
arbolDeN(0,nil).
arbolDeN(N,bin(I,_,D)) :- N > 0, N2 is N-1, paresQueSuman(N2,NI,ND), arbolDeN(NI,I), arbolDeN(ND,D).

paresQueSuman(S,X,Y) :- between(0,S,X), Y is S-X.

% desde(+X, -Y).
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% nodosEn(?A, +L) que es verdadero cuando A es un árbol cuyos nodos pertenecen al conjunto conjunto de átomos L (representado mediante una lista no vacía, sin orden relevante y sin repetidos). 
nodosEn(A, L) :- var(A), arbol(A), nodosEn(A, L).
nodosEn(nil, _).
nodosEn(bin(AI, X, AD), L) :- member(X, L), nodosEn(AI, L), nodosEn(AD, L).

% sinRepEn(-A, +L) que genere todos los árboles cuyos nodos pertenezcan al alfabeto L y usando como máximo una vez cada símbolo del mismo.
sinRepEn(A,L) :- nodosEn(A, L), noHayRepetidos(A, L).

% noHayRepetidos(+A, +L)
noHayRepetidos(_, []).
noHayRepetidos(nil, _).
noHayRepetidos(bin(AI, R, AD), [X|L]) :- cantidadApariciones(X, bin(AI, R, AD), 0), noHayRepetidos(bin(AI, R, AD), L).
noHayRepetidos(bin(AI, R, AD), [X|L]) :- cantidadApariciones(X, bin(AI, R, AD), 1), noHayRepetidos(bin(AI, R, AD), L).

% cantidadApariciones(+E, +T, -Aps)
cantidadApariciones(_,nil,0).
cantidadApariciones(E, bin(AI, X, AD), Aps) :- X = E, cantidadApariciones(E, AI, AparicionesIzq), 
                                            cantidadApariciones(E, AD, AparicionesDerecha),
                                            Aps is AparicionesDerecha + AparicionesIzq + 1.
cantidadApariciones(E, bin(AI, X, AD), Aps) :- X \= E, cantidadApariciones(E, AI, AparicionesIzq), 
                                            cantidadApariciones(E, AD, AparicionesDerecha),
                                            Aps is AparicionesDerecha + AparicionesIzq.
