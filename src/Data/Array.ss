(library (Data.Array foreign)
  (export rangeImpl
          replicateImpl
          fromFoldableImpl
          length
          unconsImpl
          indexImpl
          findMapImpl
          findIndexImpl
          findLastIndexImpl
          _insertAt
          _deleteAt
          _updateAt
          reverse
          concat
          filterImpl
          partitionImpl
          scanlImpl
          scanrImpl
          sortByImpl
          sliceImpl
          zipWithImpl
          anyImpl
          allImpl
          unsafeIndexImpl
          )
  (import (only (rnrs base) define lambda begin quote let let* cond if not or * + - = < > >= boolean? error)
          (prefix (purs runtime lib) rt:)
          (prefix (purs runtime srfi :214) srfi:214:))

;;------------------------------------------------------------------------------
;; Array creation --------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define rangeImpl
    (lambda (start end)
      (let* ([step (if (> start end) -1 1)]
            [result (srfi:214:make-flexvector (+ (* step (- end start)) 1))])
        (let recur ([i start]
                    [n 0])
          (if (not (= i end))
            (begin
              (srfi:214:flexvector-set! result n i)
              (recur (+ i step) (+ n 1)))
            (begin
              (srfi:214:flexvector-set! result n i)
              result))))))

  (define replicateImpl
    (lambda (count value)
      (if (< count 1)
        (rt:make-array)
        (let ([result (srfi:214:make-flexvector count)])
          (srfi:214:flexvector-fill! result value)
          result))))
      
  (define fromFoldableImpl
    (lambda (foldr xs)
      (error #f "fromFoldableImpl not implemented")))

  (define length rt:array-length)

  (define unconsImpl
    (lambda (empty next xs)
      (if (= (rt:array-length xs) 0)
        (empty 'unit)
        ((next (rt:array-ref xs 0)) (srfi:214:flexvector-copy xs 1)))))

  (define indexImpl
    (lambda (just nothing xs i)
      (if (or (< i 0) (>= i (rt:array-length xs)))
        nothing
        (just (rt:array-ref xs i)))))

  (define findMapImpl
    (lambda (nothing isJust f xs)
      (error #f "findMapImpl not implemented")))

  (define findIndexImpl
    (lambda (just nothing f xs)
      (let ([i (srfi:214:flexvector-index f xs)])
        (if (boolean? i)
          nothing
          (just i)))))

  (define findLastIndexImpl
    (lambda (just nothing f xs)
      (let ([i (srfi:214:flexvector-index-right f xs)])
        (if (boolean? i)
          nothing
          (just i)))))

  (define _insertAt
    (lambda (just nothing i a l)
      (if (or (< i 0) (> i (rt:array-length l)))
        nothing
        (let ([l1 (srfi:214:flexvector-copy l)])
          (srfi:214:flexvector-add! l1 i a)
          (just l1)))))

  (define _deleteAt
    (lambda (just nothing i l)
      (error #f "_deleteAt not implemented")))

  (define _updateAt
    (lambda (just nothing i a l)
      (error #f "_updateAt not implemented")))

;;------------------------------------------------------------------------------
;; Transformations -------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define reverse
    (lambda (l)
      (error #f "reverse not implemented")))

  (define concat
    (lambda (xss)
      (error #f "concat not implemented")))

  (define filterImpl
    (lambda (f xs)
      (error #f "filterImpl not implemented")))

  (define partitionImpl
    (lambda (f xs)
      (error #f "partitionImpl not implemented")))

  (define scanlImpl
    (lambda (f b xs)
      (error #f "scanlImpl not implemented")))

  (define scanrImpl
    (lambda (f b xs)
      (error #f "scanrImpl not implemented")))

;;------------------------------------------------------------------------------
;; Sorting ---------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define sortByImpl
    (lambda (compare fromOrdering xs)
      (error #f "sortByImpl not implemented")))

;;------------------------------------------------------------------------------
;; Subarrays -------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define sliceImpl
    (lambda (s e l)
      (srfi:214:flexvector-copy l s e)))

;;------------------------------------------------------------------------------
;; Zipping ---------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define zipWithImpl
    (lambda (f xs ys)
      (error #f "zipWithImpl not implemented")))

;;------------------------------------------------------------------------------
;; Folding ---------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define anyImpl
    (lambda (p xs)
      (error #f "anyImpl not implemented")))

  (define allImpl
    (lambda (p xs)
      (error #f "allImpl not implemented")))

;;------------------------------------------------------------------------------
;; Partial ---------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define unsafeIndexImpl
    (lambda (xs n)
      (rt:array-ref xs n)))
)

