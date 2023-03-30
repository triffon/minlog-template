(define (repeated n f x)
  (if (= n 0) x
      (f (repeated (- n 1) f x))))

(define (c n)
  (lambda (f)
    (lambda (x)
      (repeated n f x))))

(define c0 (c 0))
(define c1 (c 1))
(define c5 (c 5))

(define (1+ x) (+ x 1))

(define id
  (lambda (x) x))

(define (printn cn)
  ((cn 1+) 0))

(define cs
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(define c+
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((m f) ((n f) x)))))))

(define c++
  (lambda (m)
    (m cs)))

(define c*
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (m (n f))))))

(define c**
  (lambda (m)
    (lambda (n)
      ((m (c+ n)) c0))))

(define c^^
  (lambda (m)
    (lambda (n)
      ((n (c* m)) c1))))

(define c^
  (lambda (m)
    (lambda (n)
      (n m))))

(define c#t
  (lambda (x)
    (lambda (y)
      x)))

(define c#f
  (lambda (x)
    (lambda (y)
      y)))

(define (printb b)
  ((b #t) #f))

(define cif id)

(define c¬
  (lambda (p)
    ((p c#f) c#t)))

(define c∧
  (lambda (p)
    (lambda (q)
      ((p q) c#f))))

(define c∨
  (lambda (p)
    (lambda (q)
      ((p c#t) q))))
