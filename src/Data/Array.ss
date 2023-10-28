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
  (import (only (rnrs base) define lambda begin quote let let* cond if not and or * + - = < > >= <= boolean? error)
          (only (chezscheme) fx/)
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
      (let ([len (rt:array-length xs)])
        (let recur ([i 0])
          (if (< i len)
            (let ([result (f (rt:array-ref xs i))])
              (if (isJust result)
                result
                (recur (+ i 1))))
            nothing)))))

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
      (if (or (< i 0) (>= i (rt:array-length l)))
        nothing
        (let ([l1 (srfi:214:flexvector-copy l)])
          (srfi:214:flexvector-remove! l1 i)
          (just l1)))))

  (define _updateAt
    (lambda (just nothing i a l)
      (if (or (< i 0) (>= i (rt:array-length l)))
        nothing
        (let ([l1 (srfi:214:flexvector-copy l)])
          (srfi:214:flexvector-set! l1 i a)
          (just l1)))))


;;------------------------------------------------------------------------------
;; Transformations -------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define reverse srfi:214:flexvector-reverse-copy)

  (define concat
    (lambda (xss)
      (srfi:214:flexvector-concatenate (srfi:214:flexvector->list xss))))

  (define filterImpl srfi:214:flexvector-filter)

  (define partitionImpl
    (lambda (f xs)
      (error #f "partitionImpl not implemented")))

  (define scanlImpl
    (lambda (f b xs)
      (let* ([len (rt:array-length xs)]
             [out (srfi:214:make-flexvector len)])
        (let recur ([i 0]
                    [acc b])
          (if (< i len)
            (let ([next ((f acc) (rt:array-ref xs i))])
              (srfi:214:flexvector-set! out i next)
              (recur (+ i 1) next))
            out)))))

  (define scanrImpl
    (lambda (f b xs)
      (let* ([len (rt:array-length xs)]
             [out (srfi:214:make-flexvector len)])
        (let recur ([i (- len 1)]
                    [acc b])
          (if (>= i 0)
            (let ([next ((f (rt:array-ref xs i)) acc)])
              (srfi:214:flexvector-set! out i next)
              (recur (- i 1) next))
            out)))))

;;------------------------------------------------------------------------------
;; Sorting ---------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define sortByImpl
    (lambda (compare fromOrdering xs)

      (define sort!
        (lambda (xs start end)

          (define merge!
            ;; l = index of start of the left-side
            ;; m = index of start of the right-side
            ;; r = index of last element of right-size
            (lambda (l m r)
              ;; Make temporary copies of left and right
              (let ([lc (srfi:214:flexvector-copy xs l m)]
                    [rc (srfi:214:flexvector-copy xs m (+ r 1))])
                (let loop ([k l]
                           [li 0]
                           [ri 0])
                  (cond
                    [(and (< li (rt:array-length lc)) (< ri (rt:array-length rc)))
                      (let ([x (rt:array-ref lc li)]
                            [y (rt:array-ref rc ri)])
                        (if (<= (fromOrdering ((compare x) y)) 0)
                          (begin
                            (srfi:214:flexvector-set! xs k x)
                            (loop (+ k 1) (+ li 1) ri))
                          (begin
                            (srfi:214:flexvector-set! xs k y)
                            (loop (+ k 1) li (+ ri 1)))))]
                    [(< li (rt:array-length lc))
                      (begin
                        (srfi:214:flexvector-set! xs k (rt:array-ref lc li))
                        (loop (+ k 1) (+ li 1) ri))]

                    [(< ri (rt:array-length rc))
                      (begin
                        (srfi:214:flexvector-set! xs k (rt:array-ref rc ri))
                        (loop (+ k 1) li (+ ri 1)))])))))

            (if (< (- end start) 1)
              xs
              (let* ([middle (+ start (fx/ (- end start) 2))])
                (sort! xs start middle)
                (sort! xs (+ middle 1) end)
                (merge! start (+ middle 1) end)))))

      (let ([res (srfi:214:flexvector-copy xs)])
        (sort! res 0 (- (rt:array-length res) 1))
        res)))


;;------------------------------------------------------------------------------
;; Subarrays -------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define sliceImpl
    (lambda (s e l)
      (if (> s e)
        (rt:make-array)
        (srfi:214:flexvector-copy l s e))))

;;------------------------------------------------------------------------------
;; Zipping ---------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define zipWithImpl
    (lambda (f xs ys)
      (error #f "zipWithImpl not implemented")))

;;------------------------------------------------------------------------------
;; Folding ---------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define anyImpl srfi:214:flexvector-any)

  (define allImpl srfi:214:flexvector-every)

;;------------------------------------------------------------------------------
;; Partial ---------------------------------------------------------------------
;;------------------------------------------------------------------------------

  (define unsafeIndexImpl
    (lambda (xs n)
      (rt:array-ref xs n)))
)

