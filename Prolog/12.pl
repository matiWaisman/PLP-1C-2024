vacio(nil).

raiz(bin(_,X,_), X).

altura(nil, 0).
altura(bin(I,_,D), Res) :- altura(I, Ai), altura(D, Ad), Res is max(Ai, Ad) + 1. 

cantidadNodos(nil, 0).
cantidadNodos(bin(I,_,D), Res) :- cantidadNodos(I, Ni), cantidadNodos(D, Nd), Res is Ni + Nd + 1.