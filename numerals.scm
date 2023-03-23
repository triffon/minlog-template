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
