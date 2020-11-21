;;; rubik.el --- scramble rubik's cube and time your solves (and log them, hopefully)
;;; Commentary:
;;; Code:
(define-derived-mode rubik-mode special-mode "Rubik")

(defconst *rubik-scramble-basic-moves*
  '(("R" . nil) ("L" . nil) ("U" . nil) ("D" . nil) ("F" . nil) ("B" . nil))
  "Just basic moves, no prime no double.")

(defun move-to-string (move)
  "Convert MOVE to string."
  (if (cdr move)
      (concat (car move) (char-to-string (cdr move)))
      (car move)))

(defun get-prime-version-move (move)
  "Get prime version of MOVE."
  (cons (car move) ?\'))

(defun get-doubled-version-move (move)
  "Get doubled version of MOVE."
  (cons (car move) ?2))

(defun get-version-of-move (move)
  "Get version of MOVE."
  (cdr move))

(defun is-prime-move? (move)
  "Predicate to detect if MOVE is prime move."
  (eq (get-version-of-move move) ?\'))

(defun is-doubled-move? (move)
  "Predicate to detect if MOVE is doubled move."
  (eq (get-version-of-move move) ?2))

(defun is-basic-move? (move)
  "Predicate to detect if MOVE is basic move."
  (let ((version-of-move (get-version-of-move move)))
    (not (or (eq version-of-move ?\')
        (eq version-of-move ?2)))))

(defun get-basic-version-of-move (move)
  "Get basic versionof MOVE."
  (if (or (is-prime-move? move) (is-doubled-move? move))
      (cons (car move) nil)
      move))

(defconst *rubik-scramble-moves*
  (append
   *rubik-scramble-basic-moves*
   (mapcar 'get-prime-version-move   *rubik-scramble-basic-moves*)
   (mapcar 'get-doubled-version-move *rubik-scramble-basic-moves*))
  "The all possible scramble moves, basic moves + primes of them + double basic moves.")

(defconst rubik-scramble-move-count 20
  "Count of scramble moves.")

(defun rubik ()
  "Scramble your rubik!"
  (interactive)
  (switch-to-buffer "*rubik*")
  (rubik-mode)
  (rubik-scramble))

(defun rubik-scramble ()
  "Erase buffer and generate new scramble."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (string-join (mapcar 'move-to-string (generate-scramble '())) " "))))

(defun generate-scramble (scramble)
  "Generate new SCRAMBLE."
  (if (= (length scramble) rubik-scramble-move-count)
      (reverse scramble)
      (generate-scramble (cons
                          (generate-next-move (car scramble))
                          scramble))))

(defun generate-next-move (last-move)
  "Generate next move, but it shouldn't be equal to the prime or doubled version of LAST-MOVE."
  (let ((next-move (elt *rubik-scramble-moves* (random (length *rubik-scramble-moves*)))))
    (if (string=
         (car next-move)
         (car last-move))
        (generate-next-move last-move)
      next-move
      )))

(provide 'rubik)

;;; rubik.el ends here
