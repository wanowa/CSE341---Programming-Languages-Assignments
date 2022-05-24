% FACTS
flight(canakkale, erzincan, 6).
flight(erzincan, antalya, 3).
flight(antalya, izmir, 2).
flight(antalya, diyarbakir, 4).
flight(izmir, istanbul, 2).
flight(izmir, ankara, 6).
flight(istanbul, ankara, 1).
flight(istanbul, rize, 4).
flight(ankara, rize, 5).
flight(ankara, van, 4).
flight(ankara, diyarbakir, 8).
flight(van, gaziantep, 3).

flight(erzincan, canakkale, 6).		
flight(antalya, erzincan, 3).		
flight(izmir, antalya, 2).
flight(diyarbakir, antalya, 4).	
flight(istanbul, izmir, 2).
flight(ankara, izmir, 6).
flight(ankara, istanbul, 1).	
flight(rize, istanbul, 4).
flight(rize, ankara, 5).
flight(van, ankara, 4).
flight(diyarbakir, ankara, 8).	
flight(gaziantep, van, 3).
%-------------------------------------

% RULES
direct_route(X, Y) :- 	%Checks if there is direct route 
	flight(X, Y, _).	%between given cities.


route(X, Y, C) :- 		%Non-Recursion route rule
	flight(X, Y, C).


route(X, Z, C) :- 		%Recursion route rule
	route(X, Y, C1),
	flight(Y, Z, C2),
	not(X==Y),
	not(Y==Z),
	not(X==Z),
	C is C1 + C2.		%Costs are summed


route(X, T, C) :-		%Recursion route rule
	route(X, Y, C1),
	flight(Y, Z, C2),
	flight(Z, T, C3),
	not(X==Y),not(X==Z),
	not(X==T),not(Y==Z),
	not(Y==T),not(Z==T),	
	C is C1 + C2 + C3.	%Costs are summed