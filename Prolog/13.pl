%inorder(+AB,-Lista), que tenga éxito si AB es un árbol binario y Lista la lista de sus nodos según el recorrido inorder.
inorder(nil, []).
inorder(bin(I,R,D), Lista) :- inorder(I, InorderIzq), inorder(D, InorderDerecho), 
                              append(InorderIzq, [R|InorderDerecho], Lista).

% arbolConInorder(+Lista,-AB), versión inversa del predicado anterior
arbolConInorder([], nil).


% aBB(+T), que será verdadero si T es un árbol binario de búsqueda.
raiz(bin(_,X,_), X).

aBB(nil).
aBB(bin(nil,_,nil)).
aBB(bin(I,R,D)) :- aBB(I), aBB(D), raiz(I, Ri), raiz(D, Rd), R > Ri, R < Rd.

%aBBInsertar(+X, +T1, -T2), donde T2 resulta de insertar X en orden en el árbol T1.
aBBInsertar(X, nil, bin(nil,X,nil)).
aBBInsertar(X, bin(I,R,D), bin(I,R,TR)) :- X >= R, aBBInsertar(X, D, TR).
aBBInsertar(X, bin(I,R,D), bin(TR,R,D)) :- X < R, aBBInsertar(X, I, TR).