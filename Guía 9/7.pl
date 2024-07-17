% palíndromo(+L, ?L1), donde L1 es un palíndromo cuya primera mitad es L.
palindromo(L,L1) :- reverse(L, RL), append(L, RL, L1).

% iésimo(+I, +L, -X), donde X es el I-ésimo elemento de la lista L. Solo funciona si I viene instanciado.
%iesimo(1, [X|_], X).
%iesimo(I, [_|L], X) :- IM is I - 1, iesimo(IM, L, X).

% iésimo(?I, +L, -X), donde X es el I-ésimo elemento de la lista L. 
iesimo(1, [X|_], X).
iesimo(I, [_|L], X) :- iesimo(J, L, X), I is J + 1.

