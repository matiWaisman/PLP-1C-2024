% arbol(-A) que genere estructuras de árbol binario, dejando los valores de los nodos sin instanciar.
arbol(nil).
arbol(bin(AI, _, AD)) :- arbol(AI), arbol(AD).

% nodosEn(?A, +L) que es verdadero cuando A es un árbol cuyos nodos pertenecen al conjunto conjunto de átomos L (representado mediante una lista no vacía, sin orden relevante y sin repetidos). 
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
