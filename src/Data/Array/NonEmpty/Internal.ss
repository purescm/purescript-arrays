(library (Data.Array.NonEmpty.Internal foreign)
  (export foldr1Impl
          foldl1Impl
          traverse1Impl)
  (import (only (rnrs base) define lambda cons let if quote)
          (only (chezscheme) fx- fx< fx>= fx+)
          (prefix (purs runtime lib) rt:)
          (prefix (purs runtime srfi :214) srfi:214:))

  (define foldr1Impl
    (lambda (f xs)
      (let loop ([acc (srfi:214:flexvector-back xs)]
                 [i (fx- (rt:array-length xs) 2)])
        (if (fx>= i 0)
          (loop ((f (rt:array-ref xs i)) acc) (fx- i 1))
          acc))))

  (define foldl1Impl
    (lambda (f xs)
      (let loop ([acc (rt:array-ref xs 0)] [i 1])
        (if (fx< i (rt:array-length xs))
          (loop ((f acc) (rt:array-ref xs i)) (fx+ i 1))
          acc))))

  (define traverse1Impl
    (lambda (apply map f)

      (define kons (lambda (x) (lambda (ys) (cons x ys))))
      (define singleton (lambda (x) (cons x '())))

      (lambda (array)
        ((map srfi:214:list->flexvector)
         (srfi:214:flexvector-fold-right
           (lambda (s x) ((apply ((map kons) (f x))) s))
           ((map singleton) (f (srfi:214:flexvector-back array)))
           (srfi:214:flexvector-copy array 0 (fx- (rt:array-length array) 1)))))))
)
