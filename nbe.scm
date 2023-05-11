;; x - променлива
;; (m1 m2) - апликация
;; (λ (x) m) - абстракция

(define (make-app m1 m2) (list m1 m2))
(define (make-λ x m) `(λ (,x) ,m)) ; (list 'λ (list x) m)

(define (var? m) (not (pair? m)))
(define (app? m) (and (list? m) (= (length m) 2)))
(define (abs? m) (and (list? m) (= (length m) 3)))

(define get-fun car)
(define get-arg cadr)

(define get-var caadr)
(define get-body caddr)

;; α - променлива
;; (⇒ ρ σ) - функционален тип

(define (make-arrow ρ σ) `(⇒ ,ρ ,σ))
(define get-dom cadr)
(define get-ran caddr)

(define base? var?)
(define arrow? pair?)

;; оценките ще са функции от променливи в стойности

(define (modify ξ x a)
  (lambda (y)
    (if (eqv? x y) a (ξ y))))

(define (evaluate M ξ)
  (cond ((var? M) (ξ M))
        ((app? M)
         (let ((M₁ (get-fun M))
               (M₂ (get-arg M)))
           ((evaluate M₁ ξ) (evaluate M₂ ξ))))
        ((abs? M)
         (let ((x (get-var M))
               (N (get-body M)))
           (lambda (a) (evaluate N (modify ξ x a)))))))

(define (⇑ τ M)
  (if (base? τ) M
      (let ((ρ (get-dom τ))
            (σ (get-ran τ)))
        (lambda (a)
          (⇑ σ (make-app M (⇓ ρ a)))))))

(define (⇓ τ a)
  (if (base? τ) a
      (let ((ρ (get-dom τ))
            (σ (get-ran τ))
            (x (gensym 'x)))
        (make-λ x
                  (⇓ σ (a (⇑ ρ x)))))))

(define (nbe τ M)
  (⇓ τ (evaluate M 'nothing))))

(define S '(λ (x) (λ (y) (λ (z) ((x z) (y z))))))
(define K '(λ (x) (λ (y) x)))
(define ti '(⇒ α α))

(define I (nbe ti `((,S ,K) ,K)))
;; !!! (define x (nbe 'α `(,I x)))
