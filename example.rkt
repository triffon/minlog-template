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
;; not = ¬
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
;; (ex-intro <term>) = ∃⁺ <term>
;; (ex-elim <avar>) = ∃⁻ <avar>

;; (current-proof) = current proof object

;; (cdp [<proof>])
;; check and display (current) <proof> as a tree

;; (proof-to-expr <proof>)
;; display proof as a lambda term

;; (add-var-name <name> <type>)
;; create a new variable with <name> of <type>

(add-pvar-name "A" (make-arity))
(set-goal (pf "A -> A"))
