% Como tenemos N is X+1, para que eso funcione lo de la derecha tiene que estar instanciado, por lo que sí o sí X tiene que venir instanciado.
% Si Y no viene instanciada funciona bien y arroja todos los números desde X hasta infinito. 
% Si Y viene instanciada como un número menor a X de entrada se va a colgar porque va a incrementar a X y nunca va a llegar al caso base porque siempre Y va a ser menor a X.
% Si Y viene instanciada como uno mayor va a llegar al caso base de que X = Y y va a arrojar true, pero después va a seguir aumentando X hasta infinito.
% Así que el patrón de instanciación sería si queremos todos los números de X a infinito: 
% desde(+X, -Y).
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% desde2(+X,?Y)
desde2(X, X).
desde2(X,Y) :- nonvar(Y), X < Y.
desde2(X,Y) :- var(Y), N is X + 1, desde2(N,Y).

