#lang r5rs

;; py = parse type
;; pt = parse term
;; pf = parse formula
;; pp = pretty print
;; (add-pvar-name <name> <arity>)
;; create a new predicate variable (symbol) with <name> and <arity>

;; (make-arity {<types>})
;; create a new "arity" object with the specified types

;; -> = →
;; & = ∧
;; ord = ∨
;; not = ¬ = . -> F
;; all = ∀
;; ex = ∃

;; (set-goal <formula>)
;; start a new proof with <formula> as a goal

;; (assume {<avar> | <var>}⁺)
;; →⁺ and ∀⁺ with <avar>s or <var>s

;; (use <avar>)
;; →⁻, autoapplying ∀⁻ and ∧⁻ as needed

;; (use-with <avar> {<term>}⁺)
;; →⁻  with assumption <avar> and ∀⁻ with <term>s, autoapplying ∧⁻ as needed

;; (split) = ∧⁺
;; (intro <n>) = ∨⁺ₙ
;; (elim) = ∨⁻
;; (ord-intro [01])
;; (ex-intro <term>) = ∃⁺ <term>
;; (ex-elim <avar>) = ∃⁻ <avar>

;; (add-var-name <name> <type>)
;; create a new variable with <name> of <type>

(add-pvar-name "A" (make-arity))
(set-goal (pf "A -> A"))
(assume "u")
(cdp)
(proof-to-expr (current-proof))

(add-pvar-name "B" (make-arity))
(set-goal (pf "A -> B -> A"))
(assume "u" "v")
(use "u")
(cdp)
(proof-to-expr (current-proof))

(add-pvar-name "C" (make-arity))
(set-goal (pf "(A -> B -> C) -> (A -> B) -> A -> C"))
(assume "u" "v" "w")
(use "u")
(use "w")
(use "v")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "A & B -> A ord B"))
(assume "u")
(intro 0)
(use "u")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "A & B -> A ord B"))
(assume "u")
(intro 1)
(use "u")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "A -> not (not A)"))
(assume "u" "v")
(use "v")
(use "u")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "A & B -> B & A"))
(assume "u")
(split)
(use "u")
(use "u")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "not A ord not B -> not (A & B)"))
(assume "u" "v")
(elim "u")
;; I случай: not A
  (assume "a")
  (use "a")
  (use "v")
;; II случай: not B
  (assume "b")
  (use "b")
  (use "v")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "(not (not A) -> not (not B)) -> not (not (A -> B))"))
;; домашно

(set-goal (pf "not (not A) -> A"))
(use "Stab")
(proof-to-expr (current-proof))

(set-goal (pf "A ord not A"))
(use "Stab")
(assume "u")
(use "u")
(intro 1)
(assume "v")
(use "u")
(intro 0)
(use "v")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "((A -> B) -> A) -> A"))
(assume "u")
(use "Stab")
(assume "v")
(use "v")
(use "u")
(assume "w")
(use "Efq")
(use "v")
(use "w")
(cdp)
(proof-to-expr (current-proof))

(add-var-name "x" (py "alpha"))
(add-pvar-name "p" (make-arity (py "alpha")))

(set-goal (pf "all x p x -> ex x p x"))
(assume "u")
(ex-intro (pt "x"))
;; (use "u")
(use-with "u" (pt "x"))
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "all x (B -> p x) -> B -> all x p x"))
(assume "u" "v" "x")
(use "u")
(use "v")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "not (ex x p x) -> all x not (p x)"))
(assume "u" "x" "v")
(use "u")
(ex-intro (pt "x"))
(use "v")
(cdp)
(proof-to-expr (current-proof))

(set-goal (pf "ex x (p x -> all x p x)"))
;; 