(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((#1=#:require (package)
          (unless (find-package package)
            (cond
              ((and (find-package '#:ql) (find-symbol '#:quickload '#:ql))
                (funcall (find-symbol '#:quickload '#:ql) package))
              ((and (find-package '#:asdf) (find-symbol '#:load-system '#:asdf))
                (funcall (find-symbol '#:load-system '#:asdf) package))
              (t
                (require package))))))
    (require '#:float-features)))

;;
;; This implementation was ported from the Scala version in
;; jsoniter found here:
;;  <https://github.com/plokhotnyuk/jsoniter-scala/blob/master/jsoniter-scala-core/jvm/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/core/JsonWriter.scala>
;; which is in turn based on the Java version here:
;;   https://github.com/c4f7fcce9cb06515/Schubfach/blob/3c92d3c9b1fead540616c918cdfef432bca53dfa/todec/src/math/FloatToDecimal.java
;;
;; NOTE - There are many improvements to be made by properly
;;        defining numerical ranges of variables. The Java and Scala
;;        versions use signed types everywhere, and this can often
;;        get in the way of optimizations and code readability with
;;        CL's number system (hence the %->int32 and such macros
;;
;;        At the time of writing, performance here is still at least
;;        2x the builtin writer on SBCL, with about 5x with (speed 3)
;;

(defpackage #:com.inuoe.jzon/schubfach
  (:use #:cl)
  (:local-nicknames
    (#:ff #:org.shirakumo.float-features))
  (:export
    #:write-double))

(in-package #:com.inuoe.jzon/schubfach)

(defmacro %->int32 (x)
  (let ((x-sym (gensym "X")))
    `(let ((,x-sym (ldb (byte 32 0) ,x)))
      (logior (ldb (byte 31 0) ,x-sym)
              (- (mask-field (byte 1 31) ,x-sym))))))

(defmacro %->int64 (x)
  (let ((x-sym (gensym "X")))
    `(let ((,x-sym (ldb (byte 64 0) ,x)))
      (logior (ldb (byte 63 0) ,x-sym)
              (- (mask-field (byte 1 63) ,x-sym))))))

(defmacro %lsl32 (x bits)
  (let ((x-sym (gensym (string '#:x)))
        (bits-sym (gensym (string '#:bits))))
  `(let ((,x-sym (ldb (byte 32 0) ,x))
         (,bits-sym (logand ,bits 31)))
    (%->int32 (mask-field (byte (- 32 ,bits-sym) 0)
                          (ash ,x-sym (- ,bits-sym)))))))

(defmacro %lsl64 (x bits)
  (let ((x-sym (gensym (string '#:x)))
        (bits-sym (gensym (string '#:bits))))
    `(let ((,x-sym (ldb (byte 64 0) ,x))
           (,bits-sym ,(logand bits 63)))
      (%->int64 (mask-field (byte (- 64 ,bits-sym) 0)
                            (ash ,x-sym (- ,bits-sym)))))))

(defmacro %ash32 (x bits)
  `(%->int32 (ash ,x (logand ,bits 31))))

(defmacro %ash64 (x bits)
  `(%->int64 (ash ,x (logand ,bits 63))))

(defmacro %float-to-raw-int-bits (x)
  `(the (unsigned-byte 32) (ff:single-float-bits ,x)))

(defmacro %double-to-raw-long-bits (x)
  `(the (unsigned-byte 64) (ff:double-float-bits ,x)))

(defmacro %multiply-high (x y)
  #+sbcl `(sb-kernel:%multiply-high ,x ,y)
  #-sbcl `(ldb (byte 64 64) (* ,x ,y)))


(defparameter *digits* (let ((ds (make-array 100 :element-type '(unsigned-byte 16)))
                             (i 0)
                             (j 0))
                          (declare (type (integer 0) i j))
                          (loop :while (< j 10)
                                :for k :of-type (integer 0) := 0
                                :do (loop :while (< k 10)
                                          :do (setf (aref ds i)
                                                    (logior (ash (+ k (char-code #\0)) 8)
                                                            (ash (+ j (char-code #\0)) 0)))
                                              (incf i)
                                              (incf k))


                                    (incf j))
                          ds))

(defparameter *gs* (let* ((gs (make-array 1234 :element-type '(signed-byte 64)))
                          (i 0)
                          (pow5 1))
                      (declare (type (integer 0 1235) i)
                               (type (integer 1) pow5))
                      (loop :while (< i 650)
                            :do (let ((av (+ (ash pow5 (- (- (integer-length pow5) 126))) 1)))
                                  (setf (aref gs (- 648 i)) (logand (%->int64 (ash av -63)) #x7FFFFFFFFFFFFFFF))
                                  (setf (aref gs (- 649 i)) (logand (%->int64 av) #x7FFFFFFFFFFFFFFF))
                                  (setf pow5 (* pow5 5))
                                  (incf i 2)))
                      (setf pow5 5)
                      (loop :while (< i 1234)
                            :do (let ((inv (+ (truncate (ash 1 (+ (integer-length pow5) 125))
                                                        pow5)
                                              1)))
                                  (setf (aref gs i) (logand (%->int64 (ash inv -63)) #x7FFFFFFFFFFFFFFF))
                                  (setf (aref gs (+ i 1)) (logand (%->int64 inv) #x7FFFFFFFFFFFFFFF))
                                  (setf pow5 (* pow5 5))
                                  (incf i 2)))
                      gs))

(declaim (inline %rop3))
(defun %rop3 (g1 g0 cp)
  (declare (type (signed-byte 64) g1 g0 cp))
  (the (values (signed-byte 64) &optional)
    (let* ((x1 (%multiply-high g0 cp))
           (z (%->int64 (+ (%lsl64 (* g1 cp) 1) x1)))
           (y1 (%multiply-high g1 cp)))
      (logior (%->int64 (+ (%lsl64 z 63) y1))
              (%lsl64 (- (logand z #x7FFFFFFFFFFFFFFF)) 63)))))


(defun %write-fraction-digits (q0 last-pos pos pos-lim buf ds)
  (declare (type (signed-byte 32) q0 last-pos pos pos-lim)
           (type (simple-base-string 24) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (cond
      ((> pos pos-lim)
        (let* ((q1 (%->int32 (ash (* q0 1374389535) -37)))
               (d (aref ds (- q0 (* q1 100)))))
          (setf (char buf (- pos 1)) (code-char (ldb (byte 7 0) d)))
          (setf (char buf (- pos 0)) (code-char (ldb (byte 7 8) d)))
          (%write-fraction-digits q1 last-pos (- pos 2) pos-lim buf ds)))
      (t last-pos))))

(defun %write-significant-fraction-digits32 (q0 last-pos pos pos-lim buf ds)
  (declare (type (unsigned-byte 32) q0 last-pos pos pos-lim)
           (type (simple-base-string 24) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (let* ((gp (* q0 1374389535))
           (q1 (%->int32 (ash gp -37))))
      (cond
        ((zerop (logior (logand gp #x1FC0000000)
                        last-pos))
          (%write-significant-fraction-digits32 q1 last-pos (- pos 2) pos-lim buf ds))
        (t
          (%write-fraction-digits
            q1
            (let ((d (aref ds (- q0 (* q1 100)))))
              (setf (char buf (- pos 1)) (code-char (ldb (byte 7 0) d)))
              (setf (char buf (- pos 0)) (code-char (ldb (byte 7 8) d)))
              (if (not (zerop last-pos))
                last-pos
                ;; 12345 == ('0' << 8) | '9'
                (+ (if (<= d 12345) 0 1) pos)))
            (- pos 2)
            pos-lim
            buf
            ds))))))

(declaim (inline %write-significant-fraction-digits64))
(defun %write-significant-fraction-digits64 (q0 pos pos-lim buf ds &aux (q032 (ldb (byte 32 0) q0)))
  (declare (type (unsigned-byte 64) q0)
           (type (integer 0 26) pos)
           (type (signed-byte 32) pos-lim)
           (type (simple-base-string 24) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (cond
      ((= q0 q032) (%write-significant-fraction-digits32 q032 0 pos pos-lim buf ds))
      (t
        (let ((q1 (truncate q0 100000000)))
          (%write-significant-fraction-digits32
            (ldb (byte 32 0) q1)
            (let ((r1 (ldb (byte 32 0) (- q0 (* q1 100000000)))))
              (if (zerop r1)
                0
                (%write-significant-fraction-digits32 r1 0 pos (- pos 8) buf ds)))
            (- pos 8)
            pos-lim
            buf
            ds))))))

(declaim (inline %write-2-digits))
(defun %write-2-digits (q0 pos buf ds)
  (declare (type (signed-byte 32) q0)
           (type (integer 0 26) pos)
           (type (simple-base-string 24) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (let ((d (aref ds q0)))
      (setf (char buf (+ pos 0)) (code-char (ldb (byte 7 0) d)))
      (setf (char buf (+ pos 1)) (code-char (ldb (byte 7 8) d)))
      (+ pos 2))))

(declaim (inline %write-3-digits))
(defun %write-3-digits (q0 pos buf ds)
  (declare (type (signed-byte 32) q0)
           (type (integer 0 21) pos)
           (type (simple-base-string 24) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (let* ((q1 (ash (* q0 1311) -17))
           (d (aref ds (- q0 (* q1 100)))))
      (setf (char buf (+ pos 0)) (code-char (+ q1 (char-code #\0))))
      (setf (char buf (+ pos 1)) (code-char (ldb (byte 7 0) d)))
      (setf (char buf (+ pos 2)) (code-char (ldb (byte 7 8) d)))
      (+ pos 3))))

(defun %write-positive-int-digits (q0 pos buf ds)
  (declare (type (unsigned-byte 31) q0)
           (type (integer 0 23) pos)
           (type (simple-base-string 24) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (cond
    ((< q0 100)
      (cond
        ((< q0 10)
          (setf (char buf pos) (code-char (+ q0 (char-code #\0)))))
        (t
          (let ((d (aref ds q0)))
            (setf (char buf (- pos 1)) (code-char (ldb (byte 7 0) d)))
            (setf (char buf (- pos 0)) (code-char (ldb (byte 7 8) d))))))
      (values))
    (t
      (let* ((q1 (ldb (byte 32 0) (ash (* q0 1374389535) -37)))
             (d (aref ds (- q0 (* q1 100)))))

        (setf (char buf (- pos 1)) (code-char (ldb (byte 7 0) d)))
        (setf (char buf (- pos 0)) (code-char (ldb (byte 7 8) d)))

        (%write-positive-int-digits q1 (- pos 2) buf ds)))))

(declaim (inline %offset64))
(defun %offset64 (q0)
  (cond
    ((< q0 10)                  0)
    ((< q0 100)                 1)
    ((< q0 1000)                2)
    ((< q0 10000)               3)
    ((< q0 100000)              4)
    ((< q0 1000000)             5)
    ((< q0 10000000)            6)
    ((< q0 100000000)           7)
    ((< q0 1000000000)          8)
    ((< q0 10000000000)         9)
    ((< q0 100000000000)        10)
    ((< q0 1000000000000)       11)
    ((< q0 10000000000000)      12)
    ((< q0 100000000000000)     13)
    ((< q0 1000000000000000)    14)
    ((< q0 10000000000000000)   15)
    ((< q0 100000000000000000)  16)
    (t                          17)))

(defun %write-double (x buf
                      &aux
                      (pos 0)
                      (bits (%double-to-raw-long-bits x))
                      (ds (load-time-value *digits*))
                      (gs (load-time-value *gs*)))
  (declare (type double-float x)
           (type (simple-base-string 24) buf)
           (type (integer 0 23) pos)
           (type (simple-array (signed-byte 64) (1234)) gs))
  (when (logbitp 63 bits)
    (setf (char buf pos) #\-)
    (incf pos))
  (cond
    ((zerop x)
      (setf (char buf (+ pos 0)) #\0)
      (setf (char buf (+ pos 1)) #\.)
      (setf (char buf (+ pos 2)) #\0)
      (+ pos 3))
    (t
      (let* ((ieee-exponent (ldb (byte 11 52) bits))
             (ieee-mantissa (ldb (byte 52 0) bits))
             (e (- ieee-exponent 1075))
             (m (logior ieee-mantissa #x10000000000000))
             (dv 0)
             (exp 0))
        (declare (type (signed-byte 32) e))
        (declare (type (signed-byte 64) dv))
        (declare (type (integer -325 324) exp))
        (cond
          ((and (>= e -52) (<= e 0) (zerop (logand (%ash64 m e) e)))
            ;; todo confirm that this is eq to
            ;;    dv = m >> -e
            (setf dv (ash m e)))
          (t
            (let ((exp-shift 0)
                  (exp-corr 0)
                  (cbl-shift 2))
              (declare (type (member 0 1) exp-shift))
              (declare (type (member 0 131007) exp-corr))
              (declare (type (member 1 2) cbl-shift))
              (cond
                ((zerop ieee-exponent)
                  (setf e -1074)
                  (setf m ieee-mantissa)
                  (when (< ieee-mantissa 3)
                    (setf m (* m 10))
                    (setf exp-shift 1)))
                ((= ieee-exponent #x7FF) (error "IllegalNumberError"))
                ((and (zerop ieee-mantissa) (> ieee-exponent 1))
                  (setf exp-corr 131007)
                  (setf cbl-shift 1)))

              (setf exp (ash (- (* e 315653) exp-corr) -20))

              (let* ((i (ash (+ exp 324) 1))
                     (g1 (aref gs (+ i 0)))
                     (g0 (aref gs (+ i 1)))
                     (h (logand (+ (ash (* exp -108853) -15) e 2) 63))
                     (cb (ash m 2))
                     (outm1 (- (logand m #x01) 1))
                     (vb (%rop3 g1 g0 (%ash64 cb h)))
                     (vbls (+ (%rop3 g1 g0 (%ash64 (- cb cbl-shift) h)) outm1))
                     (vbrd (- outm1 (%rop3 g1 g0 (%ash64 (+ cb 2) h))))
                     (s (ash vb -2)))
                (declare (type (integer 0 1233) i))
                (declare (type (signed-byte 64) g1))
                (declare (type (signed-byte 64) g0))
                (declare (type (signed-byte 32) h))
                (declare (type (signed-byte 64) cb))
                (declare (type (signed-byte 32) outm1))
                (declare (type (signed-byte 64) vb))
                (declare (type (signed-byte 64) vbls))
                (declare (type (signed-byte 64) vbrd))
                (declare (type (signed-byte 64) s))
                (when (or (< s 100)
                          (progn
                            ;; divide a positive int by 10
                            (setf dv (truncate s 10))
                            (let* ((sp10 (* dv 10))
                                   (sp40 (ash sp10 2))
                                   (upin (%->int32 (- vbls sp40))))
                              (or (>= (logxor (+ (%->int32 (+ sp40 vbrd)) 40) upin) 0)
                                  (progn
                                    (incf dv (%lsl32 (lognot upin) 31))
                                    (incf exp)
                                    nil)))))
                  (let* ((s4 (ash s 2))
                         (uin (%->int32 (- vbls s4))))
                    (setf dv (ldb (byte 64 0)
                               (+ (%lsl32
                                    (lognot
                                      (if (< (logxor (+ (%->int32 (+ s4 vbrd)) 4) uin) 0)
                                        uin
                                        (+ (logand (%->int32 vb) #x3)
                                           (logand (%->int32 s) #x1)
                                           -3)))
                                    31)
                                  s)))
                    (decf exp exp-shift)))))))
        (let ((len (%offset64 dv)))
          (incf exp len)
          (incf len 1)
          (cond
            ((or (< exp -3) (>= exp 7))
              (let ((last-pos (%write-significant-fraction-digits64 dv (+ pos len) pos buf ds)))
                (declare (type (integer 0 23) last-pos))
                (setf (char buf pos) (char buf (+ pos 1)))
                (setf (char buf (+ pos 1)) #\.)
                (setf pos
                  (cond
                    ((< last-pos (+ pos 3))
                      (setf (char buf last-pos) #\0)
                      (1+ last-pos))
                    (t last-pos)))
                (setf (char buf pos) #\E)
                (incf pos)
                (when (< exp 0)
                  (setf (char buf pos) #\-)
                  (incf pos)
                  (setf exp (- exp)))
                (cond
                  ((< exp 10)
                    (setf (char buf pos) (code-char (+ exp (char-code #\0))))
                    (+ pos 1))
                  ((< exp 100)
                    (%write-2-digits exp pos buf ds))
                  (t (%write-3-digits exp pos buf ds)))))
            ((< exp 0)
              (let ((dot-pos (+ pos 1)))
                (setf (char buf (+ pos 0)) #\0)
                (setf (char buf (+ pos 2)) #\0)
                (setf (char buf (+ pos 3)) #\0)
                (decf pos exp)
                (let ((last-pos (%write-significant-fraction-digits64 dv (+ pos len) pos buf ds)))
                  (setf (char buf dot-pos) #\.)
                  last-pos)))
            ((< exp (- len 1))
              (let* ((last-pos (%write-significant-fraction-digits64 dv (+ pos len) pos buf ds))
                     (before-dot-pos (+ pos exp)))
                (loop :while (<= pos before-dot-pos)
                      :do (setf (char buf pos) (char buf (+ pos 1)))
                          (incf pos))
                (setf (char buf pos) #\.)
                last-pos))
            (t
              (incf pos len)
              (%write-positive-int-digits dv (- pos 1) buf ds)
              (setf (char buf (+ pos 0)) #\.)
              (setf (char buf (+ pos 1)) #\0)
              (+ pos 2))))))))

(defun stringify-double (x &aux (buf (make-array 24 :element-type 'base-char)))
  (declare (dynamic-extent buf))
  (check-type x double-float)
  (let ((n (%write-double x buf)))
    (subseq buf 0 n)))

(defun write-double (x stream &aux (buf (make-array 24 :element-type 'base-char)))
  (declare (dynamic-extent buf))
  (check-type x double-float)
  (check-type stream stream)
  (let ((n (%write-double x buf)))
    (write-string buf stream :end n)))
