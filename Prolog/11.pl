vacio([]).
intercalar(L1, [], L1).
intercalar([], L2, L2) :- not(vacio(L2)). % Para que no tire repetidos
intercalar([X|L1], [Y|L2], L3) :- append([X,Y|[]], LI, L3), intercalar(L1, L2, LI).