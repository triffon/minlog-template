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

:- op(170, xfx, ⊢).
:- op(150, xfx, ⊣).
:- op(170, xfx, ∈).

% ⊢ M : T

:- op(170, fx, ⊢).

X ∈ Y :- member(X, Y).

% [x : α ⇒ β ⇒ γ, y : α ⇒ β] ⊢ λ(z,x@z@(y@z)) : α ⇒ γ 

% deconstruct(+Ρ, ?TArgs, ?T).
deconstruct(T, [], T).
deconstruct(Ρ⇒Σ, [Ρ|TArgs], T) :- deconstruct(Σ, TArgs, T).

% findmany(+Γ, -Ms, +Ts).
findmany(_, [], [], _).
findmany(Γ, [M|Ms], [T|Ts], OldTypes) :-
    not(member(T, OldTypes)),
    OldTypes ⊣ Γ ⊢ M : T,
    findmany(Γ, Ms, Ts, OldTypes).

% apply(+M, +Ns, -P).
apply(M, [], M).
apply(M, [N|Ns], P) :- apply(M@N, Ns, P).

OldTypes ⊣ Γ ⊢ λ(X,N) : Ρ ⇒ Σ :- OldTypes ⊣ [X:Ρ|Γ] ⊢ N : Σ.
OldTypes ⊣ Γ ⊢ M : T          :- X : Ρ ∈ Γ,
                             % Ρ ≡ TArgs ⇒ T
                             deconstruct(Ρ, TArgs, T),
                             findmany(Γ, Ms, TArgs, [T|OldTypes]),
                             apply(X, Ms, M).
⊢ M : T            :- [] ⊣ [] ⊢ M : T.

i(λ(x,x)).
k(λ(x,λ(y,x))).
s(λ(x,λ(y,λ(z,x@z@(y@z))))).
ω(λ(x,x@x)).
omega(W@W) :- ω(W).
c0(λ(f,λ(x,x))).
c1(λ(f,λ(x,f@x))).
c2(λ(f,λ(x,f@(f@x)))).


ti(α⇒α).
tk(α⇒β⇒α).
ts((α⇒β⇒γ)⇒(α⇒β)⇒α⇒γ).
tn((α⇒α)⇒α⇒α).
ty((α⇒α)⇒α).
tbad(α⇒(α⇒β)⇒(β⇒α)⇒β).
