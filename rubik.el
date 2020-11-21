;;; rubik.el --- scramble rubik's cube and time your solves (and log them, hopefully)
;;; Commentary:
;;; Code:
(define-derived-mode rubik-mode special-mode "Rubik")

(defconst *rubik-scramble-basic-moves*
  '("R" "L" "U" "D" "F" "B")
  "Just basic moves, no prime no double.")

(defun move-to-string (move)
  "Convert MOVE to string."
  (if (cdr move)
      (concat (car move) (char-to-string (cdr move)))
      (car move)))

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
                          (generate-next-move (caar scramble))
                          scramble))))

(defun generate-next-move (last-move)
  "Generate next move, but it shouldn't be equal to the prime or doubled version of LAST-MOVE."
  (let ((next-move (elt *rubik-scramble-basic-moves* (random (length *rubik-scramble-basic-moves*))))
        (random-modifier (random 3)))
    (if (string=
         next-move
         last-move)
        (generate-next-move last-move)
      (cond ((eq random-modifier 0) (cons next-move nil))
            ((eq random-modifier 1) (cons next-move ?\'))
            ((eq random-modifier 2) (cons next-move ?2))))))

(provide 'rubik)

;;; rubik.el ends here
