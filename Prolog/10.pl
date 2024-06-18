desde(X,X).
desde(X,Y) :- desde(N,Y), N is X+1.
