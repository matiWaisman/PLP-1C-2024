juntar([],Y, Y).
juntar([X|XS],Y,[X|L]) :- juntar(XS,Y,L).