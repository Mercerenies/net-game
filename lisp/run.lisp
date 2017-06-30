(load "./lisp/package.lisp")

(in-package #:net-game)

(restart-case
    (run-game :gamemode 'master)
  (abort ()
    (echo 0 "Aborting...")))
