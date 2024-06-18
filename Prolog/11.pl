% Con una incognita tira repetidos :/ con intercalar(X, [1,2,3], [4,1,5,2,6,3]).
intercalar(L1, [], L1).
intercalar([], L2, L2).
intercalar([X|L1], [Y|L2], L3) :- append([X,Y|[]], LI, L3), intercalar(L1, L2, LI).