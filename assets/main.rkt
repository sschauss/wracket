(module
  (define- (fib n)
    (if (<= n 2)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))
  (define+ (main n) (fib n)))
