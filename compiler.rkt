#lang at-exp racket
(provide compile-file)

(define (compile-file filename)
  (let* ([i (open-input-file filename)]
         [f (string-replace filename "rkt" "wat")]
         [o (open-output-file f #:exists 'replace)])
    (let-values ([(c env) (compile (read i) (list))])
      (write-string c o))))

(define (compile d env)
  (match d
    [`,(? number?) (compile-number d env)]
    [`,(? symbol?) (compile-symbol d env)]
    [`(+ ,l ,r) (compile-add l r env)]
    [`(- ,l ,r) (compile-minus l r env)]
    [`(* ,l ,r) (compile-multiply l r env)]
    [`(/ ,l ,r) (compile-divide l r env)]
    [`(= ,l ,r) (compile-equal l r env)]
    [`(!= ,l ,r) (compile-unequal l r env)]
    [`(> ,l ,r) (compile-greater l r env)]
    [`(>= ,l ,r) (compile-greater-equal l r env)]
    [`(< ,l ,r) (compile-less l r env)]
    [`(<= ,l ,r) (compile-less-equal l r env)]
    [`(define- (,name ,params ...) ,body ...) (compile-define- name params body env)]
    [`(define+ (,name ,params ...) ,body ...) (compile-define+ name params body env)]
    [`(let [,name ,exp] ,body ...) (compile-let name exp body env)]
    [`(if ,cond ,exp1 ,exp2) (compile-if-then-else cond exp1 exp2 env)]
    [`(module ,body ...) (compile-module body env)]
    [`(,name ,params ...) (compile-call name params env)]))

(define (compile* exps env)
  (letrec ([compile* (lambda (exps env result)
                       (if (empty? exps)
                           (values result env)
                           (let-values ([(exp env) (compile (first exps) env)])
                             (compile* (rest exps) env (append result (list exp))))))])
    (compile* exps env '())))

(define (compile-number n env)
  (values @~a{(i32.const @n)} env))

(define (compile-symbol s env)
  (values @~a{(get_local $@s)} env))

(define (compile-add l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.add @l @r)} env)))

(define (compile-minus l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.sub @l @r)} env)))

(define (compile-multiply l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.mul @l @r)} env)))

(define (compile-divide l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.div @l @r)} env)))

(define (compile-equal l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.eq @l @r)} env)))

(define (compile-unequal l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.ne @l @r)} env)))

(define (compile-greater l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.gt_s @l @r)} env)))

(define (compile-greater-equal l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.ge_s @l @r)}) env))

(define (compile-less l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.lt_s @l @r)} env)))

(define (compile-less-equal l r env)
  (let*-values ([(l env) (compile l env)]
                [(r env) (compile r env)])
    (values @~a{(i32.le_s @l @r)} env)))

(define (compile-let name exp body env)
  (let*-values ([(exp env) (compile exp env)]
                [(body env) (compile* body env)]
                [(body) (string-join body)]
                [(env) (cons name env)])
    (values @~a{(block (result i32) (set_local $@name @exp) @body)} env)))

(define (compile-define- name params body env)
  (let*-values ([(body env) (compile* body env)]
                [(body) (string-join body)]
                [(params) (string-join (map (lambda (param) @~a{(param $@param i32)}) params))]
                [(locals) (string-join (map (lambda (name) @~a{(local $@name i32)}) env))])
    (values @~a{(func $@name @params (result i32) @locals (return @body))} env)))
                       
(define (compile-define+ name params body env)
  (let*-values ([(body env) (compile* body env)]
                [(body) (string-join body)]
                [(params) (string-join (map (lambda (param) @~a{(param $@param i32)}) params))]
                [(locals) (string-join (map (lambda (name) @~a{(local $@name i32)}) env))])
    (values @~a{(func $@name @params (result i32) @locals (return @body)) (export "@name" (func $@name))} env)))

(define (compile-if-then-else cond exp1 exp2 env)
  (let*-values ([(cond env) (compile cond env)]
                [(exp1 env) (compile exp1 env)]
                [(exp2 env) (compile exp2 env)])
    (values @~a{(if (result i32) @cond (then @exp1) (else @exp2))} env)))

(define (compile-param name env)
  (values @~a{(param $@name i32)} env))

(define (compile-module body env)
  (let*-values ([(body env) (compile* body env)]
                [(body) (string-join body)])
    (values @~a{(module @body)} env)))

(define (compile-call name params env)
  (let*-values ([(params env) (compile* params env)]
                [(params) (string-join params)])
    (values @~a{(call $@name @params)} env)))