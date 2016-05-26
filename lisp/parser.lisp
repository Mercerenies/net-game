(in-package #:net-game)

; <command> ::= <verb> [ <noun-phrase> ] { <prep-phrase> }
; <noun-phrase> ::= [ <noun> ]
; <prep-phrase> ::= <prep> <noun>

(defstruct (sentence :named (:type vector))
  (verb nil)
  (noun nil)
  (preps nil))

; TODO If a noun is a prefix of another noun, the longer one is inaccessible
(defun scan-sentence (words sentence)
  (loop with vec = (make-array 15
                               :fill-pointer 0
                               :element-type 'character
                               :adjustable t)
        for ch across (concatenate 'string sentence " ")
        do (vector-push-extend ch vec)
        when (and (eq ch #\SPACE)
                  (find (string-trim '(#\SPACE) vec) words
                        :test #'string-equal))
            collect (copy-seq (string-trim '(#\SPACE) vec))
            and do (setf (fill-pointer vec) 0)
        finally (unless (zerop (length vec))
                  (return nil))))

; The first argument should be the output from scan-sentence
; Returns (verb noun &rest preps) where preps is a plist
(defun parse-sentence (phrase &key
                                (nouns nil) (verbs nil)
                                (preps nil) (arts '("a" "an" "the")))
  (loop with state = 'verb
        with verb = nil
        with noun = nil
        with prepos = nil
        with curr-prep = nil
        for curr in phrase
        for part = (cond
                     ((member curr nouns :test #'string-equal) 'noun)
                     ((member curr verbs :test #'string-equal) 'verb)
                     ((member curr preps :test #'string-equal) 'prep)
                     ((member curr arts  :test #'string-equal) 'art )
                     (t (return nil)))
        unless (eq part 'art) ; Always ignore articles
            do (case state
                 (verb (if (eq part 'verb)
                           (progn (setf verb curr)
                                  (setf state 'ready))
                           (return nil)))
                 (ready (case part
                          (noun (if (and (null noun)
                                         (null prepos))
                                    (setf noun curr)
                                    (return nil)))
                          (prep (setf curr-prep curr)
                                (setf state 'prep))
                          (t (return nil))))
                 (prep (if (eq part 'noun)
                         (progn (setf (getf prepos curr-prep) curr)
                                (setf curr-prep nil)
                                (setf state 'ready))
                         (return nil))))
        finally (return (if (eq state 'ready)
                            (make-sentence :verb verb :noun noun :preps prepos)
                            nil))))

(defun scan-then-parse (words sentence &rest keys &key &allow-other-keys)
  (apply #'parse-sentence (scan-sentence words sentence) keys))

(defun parse-with-words (sentence &rest keys &key &allow-other-keys)
  (let ((words (append (getf keys :nouns) (getf keys :verbs)
                       (getf keys :preps) (getf keys :arts) nil)))
    (apply #'scan-then-parse words sentence keys)))

; Uses *player*, *world*
(defun parse-default (sentence)
  (let ((nouns (append (mapcar #'get-name (location-contents (get-loc *player*)))
                       (mapcar #'get-name (inventory *player*))
                       (mapcar #'location-short-name (halo (get-loc *player*) 1))
                       '("fists"))))
    (parse-with-words sentence
                      :nouns nouns
                      :verbs '("go" "examine" "use" "activate" "collect"
                               "drop" "help" "quit" "attack" "talk")
                      :preps '("with")
                      :arts '("the" "a" "an"))))

; Uses *player*, *world*
(defun enhanced-parse (sentence)
  (flet ((translate-noun (noun)
           (and noun
                (or (find noun (append (location-contents (get-loc *player*))
                                       (inventory *player*)
                                       (halo (get-loc *player*) 1))
                          :test #'string-equal
                          :key #'(lambda (x)
                                   (if (typep x 'location)
                                       (location-short-name x)
                                       (get-name x))))
                    (intern-upcase noun)))))
    (let ((parse (parse-default sentence)))
      (when parse
        (setf (sentence-verb parse) (intern-upcase (sentence-verb parse)))
        (setf (sentence-noun parse) (translate-noun (sentence-noun parse)))
        (setf (sentence-preps parse)
              (loop for curr = (sentence-preps parse) then (cddr curr)
                    for key = (first curr)
                    for value = (second curr)
                    while (not (null curr))
                    append (list (intern-upcase key)
                                 (translate-noun value)))))
      parse)))
