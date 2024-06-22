% corteMásParejo(+L,-L1,-L2) que, dada una lista de números, realiza el corte más parejo posible con respecto a la suma de sus elementos
corteMasParejo(L, L1, L2) :- unCorte(L, L1, L2, S), not((unCorte(L, _, _, S2), S > S2)). 

% generarCorte(+L,-L1,-L2, -S)
unCorte(L, L1, L2, S) :- append(L1, L2, L), sumlist(L1, S1), sumlist(L2, S2), S is abs(S1 - S2).