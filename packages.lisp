(defpackage :cl-op
  (:use :cl)
  (:export #:op
           #:op*
           #:_
           #:__))
           
(defpackage :cl-op.hof
  (:use :cl)
  (:export #:flip
           #:disjoin
           #:conjoin
           #:compose))
