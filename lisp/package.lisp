;(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(defpackage #:net-game
  (:use :common-lisp)) ; :ql
(in-package #:net-game)

; (ql:quickload 'cl-json)

(load "./lisp/os.lisp")

(load "./lisp/scheme.lisp")

(load "./lisp/util.lisp")
(load "./lisp/base.lisp")
(load "./lisp/load.lisp")
(load "./lisp/numbernoun.lisp")
(load "./lisp/parser.lisp")

(load "./lisp/action.lisp")
(load "./lisp/state.lisp")
(load "./lisp/speech.lisp")
(load "./lisp/console.lisp")

(load "./lisp/quests.lisp")
(load "./lisp/brain.lisp")
(load "./lisp/knowledge.lisp")

(load "./lisp/inventory.lisp")
(load "./lisp/item.lisp")
(load "./lisp/creature.lisp")
(load "./lisp/food.lisp")
(load "./lisp/person.lisp")
(load "./lisp/spawner.lisp")
(load "./lisp/combat.lisp")

(load "./lisp/updates.lisp")
(load "./lisp/delta.lisp")
(load "./lisp/godmode.lisp")
(load "./lisp/play.lisp")
