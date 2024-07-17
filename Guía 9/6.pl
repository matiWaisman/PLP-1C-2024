% aplanar(+Xs, -Ys) 
aplanar([],[]).
% Si el primer elemento no es una lista.
aplanar([X|XS], YS) :- not(aplanar(X,_)), aplanar(XS, XSA), append([X], XSA, YS).
% Si el primer elemento es una lista.
aplanar([XS|LS], YS) :- aplanar(XS, XSA), aplanar(LS, LSA), append(XSA, LSA, YS).