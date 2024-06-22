arbol(nil).
arbol(bin(AI, X, AD)) :- arbol(AI), arbol(AD).