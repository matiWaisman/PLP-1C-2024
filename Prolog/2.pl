vecino(X, Y, [W|Ls]) :- vecino(X, Y, Ls).
vecino(X, Y, [X|[Y|Ls]]).
