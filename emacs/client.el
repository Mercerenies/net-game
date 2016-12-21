
;; This is VERY experimental Emacs integration. Load this file and run `M-x net-game-run' while the current
;; working directory is the base directory of the net-game project. This should spawn off two windows, one
;; which will allow you to play the game and the other which will output logging information. A prefix
;; argument can be supplied, which will specify the debug level. Expect lots of changes to this in the
;; future. It is very experimental and has lots of room for improvement.

;; NOTES:
;; * If you are just playing the game to play, do not use this. It is for debugging primarily, and if all you
;;   want to do is play, you can invoke the game from the command line more easily.
;; * EMACS 25 AND NEWER ONLY! This uses some very new features of Emacs and will not work in anything before
;;   Emacs 25.0

(defun net-game-spawn (cmd)
  (let ((buffer (get-buffer-create "*net-game*"))
        (err-buffer (get-buffer-create "*net-game-log*")))
    (with-current-buffer buffer
      (comint-mode))
    (with-current-buffer err-buffer
      (special-mode))
    (make-process :name "net-game"
                  :buffer buffer
                  :command cmd
                  :stderr err-buffer)
  (switch-to-buffer-other-window buffer)
  (display-buffer-below-selected err-buffer
                                 '((window-height . 0.2)))))

(defun net-game-run (&optional debug) ; TODO Support the reinforcement learning engine (-r)
  (interactive "P")
  (setq debug (or debug 3))
  (let ((command `("bash" "./bash/client.sh" "-d" ,(number-to-string debug))))
    (net-game-spawn command)))
