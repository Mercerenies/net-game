(load (merge-pathnames "quicklisp\\setup.lisp" (user-homedir-pathname)))

(defpackage #:net-game
  (:use :common-lisp :ql))
(in-package #:net-game)

(ql:quickload 'cl-json)

(load "./lisp/util.lisp")
(load "./lisp/data.lisp")
(load "./lisp/name.lisp")
(load "./lisp/action.lisp")
(load "./lisp/state.lisp")
(load "./lisp/load.lisp")
(load "./lisp/building.lisp")
(load "./lisp/play.lisp")
