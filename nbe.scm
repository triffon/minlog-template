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

(define (unionv l1 l2)
  (cond ((null? l1) l2)
        ((memv (car l1) l2) (unionv (cdr l1) l2))
        (else (cons (car l1) (unionv (cdr l1) l2)))))

(define (fv M)
  (cond ((var? M) (list M))
        ((app? M) (unionv (fv (get-fun M))
                          (fv (get-arg M))))
        ((abs? M) (filter
                   (lambda (x)
                     (not (eqv? x (get-var M))))
                   (fv (get-body M))))))

(define (make-λs M vars)
  (foldr make-λ M vars))

(define (make-arrows ρs σ)
  (foldr make-arrow σ ρs)) 

(define (close M)
  (make-λs M (fv M)))

(define (rename-var M x y)
  (cond ((and (var? M) (eqv? M x) y))
        ((var? M) M)
        ((app? M) (make-app
                   (rename-var (get-fun M) x y)
                   (rename-var (get-arg M) x y)))
        ((abs? M) (make-λ
                   (get-var M)
                   (rename-var (get-body M) x y)))))

(define (reduce-vars xs M)
  (if (null? xs) M
      (rename-var
       (reduce-vars (cdr xs) (get-body M))
       (get-var M) (car xs))))
  
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

;; ((x₁ τ₁) (x₂ τ₂) ... (xₙ τₙ)) - контекст 

(define (nbe Γ τ M)
  (let ((xs (map car Γ))
        (ρs (map cadr Γ)))
    (reduce-vars xs
                 (⇓ (make-arrows ρs τ)
                    (evaluate (make-λs M xs) 'empty)))))

(define S '(λ (x) (λ (y) (λ (z) ((x z) (y z))))))
(define K '(λ (x) (λ (y) x)))
(define ti '(⇒ α α))

(define I (nbe '() ti `((,S ,K) ,K)))
(define x (nbe '((x α)) 'α `(,I x)))

(define (repeated-app f x n)
  (if (= n 0) x
      (make-app f (repeated-app f x (- n 1)))))

(define (c n)
  (make-λ 'f (make-λ 'x (repeated-app 'f 'x n))))

(define c+
  (make-λ 'm
          (make-λ 'n
                  (make-λ 'f
                          (make-λ 'x
                                  (make-app (make-app 'm 'f) (make-app (make-app 'n 'f) 'x)))))))

(define tn `(⇒ ,ti ,ti))
(define c13 (make-app (make-app c+ (c 5)) (c 8)))
