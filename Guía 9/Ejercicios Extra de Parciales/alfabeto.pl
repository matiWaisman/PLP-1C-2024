%% palabra(+A, +N, -P)
palabra(_, 0, []).
palabra(A, N, [E|P]) :- N > 0, member(E, A), NPrima is N-1, palabra(A, NPrima, P).

%% frase(+A, -F)
frase(A, F) :- desde(0, X), frases(A, X, F).

frases(_, 0, []).
frases(A, N, F) :- N > 0, between(1, N, L) , palabra(A, L, P), NPrima is N - L, 
                  frases(A, NPrima, CasiF), append([P], CasiF, F).

desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).