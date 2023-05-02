:- set_prolog_flag(occurs_check, true).
% α, β, γ
% α ⇒ α

:- op(140, xfy, ⇒).
% x, y, z
% x@y
% λ(x,x).

:- op(130, yfx, @).

% M : T

:- op(150, xfx, :).

% Γ ⊢ M : T

:- op(160, xfx, ⊢).
:- op(170, xfx, ∈).

% ⊢ M : T

:- op(170, fx, ⊢).

X ∈ Y :- member(X, Y).

% [x : α ⇒ β ⇒ γ, y : α ⇒ β] ⊢ λ(z,x@z@(y@z)) : α ⇒ γ 


Γ ⊢ X : T          :-    X : T ∈ Γ.
Γ ⊢ M1@M2 : Σ      :-    Γ ⊢ M1 : Ρ ⇒ Σ,
                         Γ ⊢ M2 : Ρ.
Γ ⊢ λ(X,N) : Ρ ⇒ Σ :- [X:Ρ|Γ] ⊢ N : Σ.
⊢ M : T            :- [] ⊢ M : T.

i(λ(x,x)).
k(λ(x,λ(y,x))).
s(λ(x,λ(y,λ(z,x@z@(y@z))))).
ω(λ(x,x@x)).
omega(W@W) :- ω(W).
c0(λ(f,λ(x,x))).
c1(λ(f,λ(x,f@x))).
c2(λ(f,λ(x,f@(f@x)))).
