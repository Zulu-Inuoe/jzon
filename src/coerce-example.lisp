(defpackage #:com.inuoe.jzon.coerce-example
  (:use #:cl)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon)))

(in-package #:com.inuoe.jzon.coerce-example)

(defclass coordinate ()
  ((reference
    :initarg :reference)
   (x
    :initform 0
    :initarg :x
    :accessor x)
   (y
    :initform 0
    :initarg :y
    :accessor y)))

(defclass object ()
  ((alive
    :initform nil
    :initarg :alive
    :type boolean)
   (coordinate
    :initform nil
    :initarg :coordinate
    :type (or null coordinate))
   (children
    :initform nil
    :initarg :children
    :type list)))

(jzon:parse (jzon:stringify (make-instance 'object :coordinate (make-instance 'coordinate))) :type 'object)