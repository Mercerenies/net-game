(load (merge-pathnames "quicklisp\\setup.lisp" (user-homedir-pathname)))

(defpackage #:net-game
  (:use :common-lisp :ql))
(in-package #:net-game)

; (ql:quickload 'cl-json)

(load "./lisp/scheme.lisp")

(load "./lisp/util.lisp")
(load "./lisp/load.lisp")

(load "./lisp/item.lisp")
(load "./lisp/creature.lisp")

(load "./lisp/action.lisp")
(load "./lisp/state.lisp")

(load "./lisp/play.lisp")
