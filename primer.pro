man(john).
man(peter).
woman(mary).

loves(john, wine).
loves(mary, wine).
% loves(wine, wine).

% john обича тези, които обичат вино
% ∀X(loves(X, wine) → loves(john, X))
% loves(john, X) :- loves(X, Y), loves(Y, wine).
loves(john, X) :- loves(X, wine).

% o, s(o), s(s(o)), ...
nat(o).
nat(s(X)) :- nat(X).

add(o, X, X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
