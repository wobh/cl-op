;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :cl-op.system
  (:use :cl :asdf))

(in-package :cl-op.system)

(defsystem :cl-op
  :name "cl-op"
  :description "Partial application library."
  :author "Simon Belak"
  :version "0.4.1"
  :licence "MIT"
  :serial t
  :components ((:file "packages")
               (:file "op")))
