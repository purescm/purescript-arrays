(library (Data.Array.ST foreign)
  (export new
          unshiftAllImpl
          unsafeThawImpl
          unsafeFreezeImpl
          pushImpl
          toAssocArrayImpl
          freeImpl
          thawImpl)
  (import (only (rnrs base) define lambda list cons)
          (prefix (purs runtime lib) rt:)
          (prefix (purs runtime srfi :214) srfi:214:))
          
  (define new
    (lambda () (rt:make-array)))

  (define unshiftAllImpl
    (lambda (as xs)
      (srfi:214:flexvector-for-each
        as
        (lambda (a) (srfi:214:flexvector-add-front! xs a)))
      (srfi:214:flexvector-length xs)))

  (define unsafeThawImpl
    (lambda (xs) xs))

  (define unsafeFreezeImpl
    (lambda (xs) xs))

  (define pushImpl
    (lambda (a xs)
      (srfi:214:flexvector-add-back! xs a)
      (srfi:214:flexvector-length xs)))

  (define toAssocArrayImpl
    (lambda (xs)
      (srfi:214:flexvector-map/index
        (lambda (i x)
          (rt:make-object (list (cons "index" i) (cons "value" x))))
        xs)))
            
  (define copyImpl
    (lambda (xs) (srfi:214:flexvector-copy xs)))

  (define freeImpl copyImpl)

  (define thawImpl copyImpl)
)
