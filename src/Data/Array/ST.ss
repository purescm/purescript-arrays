(library (Data.Array.ST foreign)
  (export new
          unshiftAllImpl
          spliceImpl
          pokeImpl
          peekImpl
          popImpl
          unsafeThawImpl
          unsafeFreezeImpl
          lengthImpl
          shiftImpl
          pushImpl
          pushAllImpl
          toAssocArrayImpl
          sortByImpl
          thawImpl
          freezeImpl)
  (import (only (rnrs base) define lambda cond let let* begin cons if and < > >= <= + -)
          (only (chezscheme) fx/)
          (prefix (purs runtime lib) rt:)
          (prefix (purs runtime srfi :214) srfi:214:))
          
  (define new
    (lambda () (rt:make-array)))

  (define lengthImpl rt:array-length)

  (define pokeImpl
    (lambda (i a xs)
      (let ([ret (and (>= i 0) (< i (rt:array-length xs)))])
        (if ret (srfi:214:flexvector-set! xs i a))
        ret)))

  (define peekImpl
    (lambda (just nothing i xs)
      (if (and (>= i 0) (< i (rt:array-length xs)))
        (just (rt:array-ref xs i))
        nothing)))

  (define popImpl
    (lambda (just nothing xs)
      (if (> (rt:array-length xs) 0)
        (just (srfi:214:flexvector-remove-back! xs))
        nothing)))

  (define unshiftAllImpl
    (lambda (as xs)
      (srfi:214:flexvector-add-all! xs 0 (srfi:214:flexvector->list as))
      (rt:array-length xs)))

  (define shiftImpl
    (lambda (just nothing xs)
      (if (> (rt:array-length xs) 0)
        (just (srfi:214:flexvector-remove-front! xs))
        nothing)))

  (define spliceImpl
    (lambda (i howMany bs xs)
      (if (> howMany 0)
        (let ([removed (srfi:214:make-flexvector howMany)])
          (srfi:214:flexvector-copy! removed 0 xs i (+ i howMany))
          (srfi:214:flexvector-remove-range! xs i (+ i howMany))
          (srfi:214:flexvector-add-all! xs i (srfi:214:flexvector->list bs))
          removed)
        (begin
          (srfi:214:flexvector-add-all! xs i (srfi:214:flexvector->list bs))
          (rt:make-array)))))

  (define unsafeThawImpl
    (lambda (xs) xs))

  (define unsafeFreezeImpl
    (lambda (xs) xs))

  (define pushImpl
    (lambda (a xs)
      (srfi:214:flexvector-add-back! xs a)
      (rt:array-length xs)))

  (define pushAllImpl
    (lambda (as xs)
      (srfi:214:flexvector-append! xs as)
      (rt:array-length xs)))

  (define sortByImpl
    (lambda (compare fromOrdering xs)

      (define sort!
        (lambda (xs start end)

          (define merge!
            ;; l = index of start of the left-side
            ;; m = index of start of the right-side
            ;; r = index of last element of right-side
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

      (sort! xs 0 (- (rt:array-length xs) 1))
      xs))

  (define toAssocArrayImpl
    (lambda (xs)
      (srfi:214:flexvector-map/index
        (lambda (i x)
          (rt:make-object (cons "index" i) (cons "value" x)))
        xs)))
            
  (define copyImpl
    (lambda (xs) (srfi:214:flexvector-copy xs)))

  (define thawImpl copyImpl)

  (define freezeImpl copyImpl)
)
