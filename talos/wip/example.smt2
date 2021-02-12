
;; def MaxVal = {
;;     @v = Token Natural;
;;     0 < v; v < 65536;
;;     ^ v
;; }

;; def Natural = {
;;   @ds = Many (1..) Digit;
;;   ^ for (val = 0; d in ds) (addDigit val d);
;; }

;; def Digit = {
;;   @d = '0' .. '9';
;;   ^ d - '0';
;; }

;; def addDigit val d =
;;   10 * val + (d as int)
;; def Token P = {
;;   $$ = P;
;;   Many (1..) WS;
;; }

(declare-datatypes (t) ( ( maybe nothing (just (val t))) ))

(declare-datatypes (T1 T2) ( (Pair (mk-pair (first T1) (second T2)))) )

(declare-datatypes () ( (Unit unit) ))

(define-sort byte () (_ BitVec 8))
(define-sort byte-list () (Array Int byte))
(declare-datatypes () ( (Input (mk-input (bytes byte-list) (loc Int))) ))

(define-sort parser-m (a) (maybe (Pair Input a)))


(define-fun pure-byte ((i Input) (v byte)) (parser-m byte)
  (just (mk-pair i v)))

(define-fun pure-unit ((i Input)) (parser-m Unit)
  (just (mk-pair i unit)))

(define-fun pure-int ((i Input) (v Int)) (parser-m Int)
  (just (mk-pair i v)))

(define-fun pure-list-int ((i Input) (v (List Int))) (parser-m (List Int))
  (just (mk-pair i v)))


(define-fun fail-unit () (parser-m Unit)
  (as nothing (parser-m Unit)))

;; ------------------------------------------------------------------------------
;; Helpers

(define-fun next-input ((i Input)) (parser-m byte)
   (just (mk-pair (mk-input (bytes i) (+ (loc i) 1))
                  (select (bytes i) (loc i)))))

(define-fun p-assert ((b Bool) (i Input)) (parser-m Unit)
  (ite b (pure-unit i) fail-unit))

;; ------------------------------------------------------------------------------

(define-fun digit ((i Input)) (parser-m Int)
  (let ((r (next-input i))) 
    (ite (is-nothing r)
         (as nothing (parser-m Int))
         (let ((i0 (first (val r)))
               (v0 (second (val r))))
           (let ((r1 (p-assert (and (bvult #x2f v0) (bvult v0 #x3a)) i0)))
             (ite (is-nothing r1)
                  (as nothing (parser-m Int))
                  (let ((i1 (first (val r1))))
                    (pure-int i1 (- (bv2int v0) 48) ))))))))

(define-fun-rec many-digit ((i Input)) (parser-m (List Int))
  (let ((r0 (digit i)))
    (ite (is-nothing r0)
         (pure-list-int i nil)
         (let ((i0 (first (val r0)))
               (v0 (second (val r0))))
           (let ((r1 (many-digit i0)))
             (let ((i1 (first (val r1)))
                   (v1 (second (val r1))))
               (pure-list-int i1 (insert v0 v1))))))))

(define-fun-rec mkInt ((bs (List Int)) (acc Int)) Int
  (ite (is-nil bs)
       acc
       (mkInt (tail bs) 
              (+ (* acc 10) (head bs)))))

  
;; (define-fun token-natural ((

(declare-const i1 Input)
(declare-const i2 Input)
(declare-const r (List Int))

(assert (> (mkInt r 0) 100))
(assert (= (many-digit i1) (just (mk-pair i2 r ))))
(check-sat)
(get-value (r (select (bytes i1) 0)))
