;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:ql-system-builder-asd
  (:use :cl :asdf))

(in-package :ql-system-builder-asd)

(defsystem ql-system-builder
  :name "ql-system-builder"
  :description "interactive tool to quickly generate a quicklisp system skeleton"
  :version "0.0.0"
  :maintainer "fouric <fouric@protonmail.com>"
  :author "fouric <fouric@protonmail.com>"
  :license "MIT"

  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "ql-system-builder"))

  :depends-on (:cl-charms :alexandria :fouric))
