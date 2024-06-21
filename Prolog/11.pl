vacio([]).
% intercalar(+L1, +L2, -L3)
intercalar(L1, [], L1).
intercalar([], L2, L2) :- not(vacio(L2)). % Para que no tire repetidos
intercalar([X|L1], [Y|L2], L3) :- append([X,Y|[]], LI, L3), intercalar(L1, L2, LI).

% Se puede obtener L3 de L1 y L2 instanciados, se puede obtener L1 y L2 de solo L3 instanciados (tambien se puede tener L1 o L2 instanciados con L3 y obtenes el restante).
% Tambien se puede det si dadas las 3 instanciadas es verdadero o falso que L3 es la intercalacion de L1 y L2.