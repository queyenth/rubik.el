;;; rubik.el --- scramble rubik's cube and time your solves (and log them, hopefully)
;;; Commentary:
;;; Code:
(require 'cl-lib)
(define-derived-mode rubik-mode special-mode "Rubik"
  (define-key rubik-mode-map (kbd "g") 'rubik-scramble)
  (define-key rubik-mode-map (kbd "SPC") 'rubik-timer))

(defcustom rubik-solves-location (expand-file-name "rubik.txt" user-emacs-directory)
  "The full path to rubik solves file."
  :type 'string
  :group 'rubik-mode)

(defvar timer nil
  "Timer for handling rubik.")

(defvar latest-solves '()
  "List of latest solves.")

(defun rubik-timer ()
  "Stop or run timer."
  (interactive)
  ;If timer is non-nil, that means we need to stop timer, and record the solve.
  (if timer
      (let ((seconds-spend (float-time (time-subtract (current-time) timer))))
        (setq timer nil)
        (setq latest-solves (cons seconds-spend latest-solves))
        (write-region (concat (number-to-string seconds-spend) "\n") nil rubik-solves-location t)
        (rubik-redraw))
    (setq timer (current-time))))

(defconst *rubik-scramble-basic-moves*
  '("R" "L" "U" "D" "F" "B")
  "Just basic moves, no prime no double.")

(defun move-to-string (move)
  "Convert MOVE to string."
  (if (cdr move)
      (concat (car move) (cdr move))
      (car move)))

(defconst rubik-scramble-move-count 20
  "Count of scramble moves.")

(defun rubik ()
  "Scramble your rubik!"
  (interactive)
  (switch-to-buffer "*rubik*")
  (rubik-mode)
  (rubik-init)
  (rubik-redraw))

(defun rubik-read-solves-from-file ()
  "Read solves from file."
  (with-temp-buffer
    (insert-file-contents rubik-solves-location)
    (cl-mapcar #'string-to-number (split-string (buffer-string) "\n" t))))

(defun rubik-init ()
  "Init rubik (save file, you know the stuff)"
  (if (not (file-exists-p rubik-solves-location))
      (with-temp-buffer (write-file rubik-solves-location))
    (setq latest-solves (nreverse (rubik-read-solves-from-file)))))

(defun rubik-redraw ()
  "Redraw the whole screen (like after finishing solve)."
  (rubik-scramble)
  (rubik-log))

(defun rubik-scramble ()
  "Erase buffer and generate new scramble."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (delete-region (line-beginning-position) (line-end-position))
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
            ((eq random-modifier 1) (cons next-move "'"))
            ((eq random-modifier 2) (cons next-move "2"))))))

(defun rubik-log ()
  "Draw rubik solves log."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (line-beginning-position) (buffer-end 1))
    (if latest-solves
        (insert (concat "\nLatest solve: " (number-to-string (car latest-solves)))))
    (insert (concat "\n" "AO3: " (rubik-get-ao 3) "\n" "AO5: " (rubik-get-ao 5)))))

(defun rubik-get-ao (count &optional exclude)
  "Get AO for last COUNT tries. If EXCLUDE is t, then exclude the worst one and the best one."
  (let ((average "N/A")
        (available-count (min count (length latest-solves))))
    (if latest-solves
        (setq average (number-to-string (/ (cl-reduce #'+ latest-solves :end available-count) available-count))))
    average))

(provide 'rubik)

;;; evilify rubik-el
(evil-set-initial-state 'rubik-mode 'normal)
(evil-define-key 'normal rubik-mode-map (kbd "gr") 'rubik-scramble)
(evil-define-key 'normal rubik-mode-map (kbd "SPC") 'rubik-timer)

;;; rubik.el ends here

(defun get-coord (y x)
  (+ (* 3 y) x))

;; So the weird thing here is that back side is actually reverse
;; columns, i don't know, don't ask about that, I know it sucks
(defun get-column (kubik-representation side column-number)
  (let ((side-to-consider (elt kubik-representation side))
        (column (make-vector 3 0)))
    (dotimes (number 3)
      (aset column number (elt side-to-consider (get-coord number column-number))))
    column))

(defun change-column (side column column-number)
  (dotimes (number 3)
    (aset side (get-coord number column-number) (elt column number))))

(defun get-row (kubik-representation side row-number)
  (let ((side-to-consider (elt kubik-representation side))
        (row (make-vector 3 0)))
    (dotimes (number 3)
      (aset row number (elt side-to-consider (get-coord row-number number))))
    row))

(defun change-row (side row row-number)
  (dotimes (number 3)
    (aset side (get-coord row-number number) (elt row number))))

(defun change-by-list (sides columns column-number func)
  (if (and sides columns)
      (let ((side (car sides))
            (column (car columns)))
        (funcall func side column column-number)
        (change-by-list (cdr sides) (cdr columns) column-number func))))

; 0 1 5 4 -> 1 5 4 0
(defun shift-list (value)
  (append (cdr value) (list (car value))))

(defun kubik-column-move (kubik-representation sides-to-change column-number rotation-side)
  (let ((sides (cl-mapcar (lambda (x) (elt kubik-representation x)) sides-to-change))
        (columns (cl-mapcar (lambda (x) (get-column kubik-representation x column-number)) (shift-list sides-to-change))))
    (change-by-list sides columns column-number 'change-column)
    (aset kubik-representation rotation-side (rotate-side (elt kubik-representation rotation-side)))))

(defun kubik-row-move (kubik-representation sides-to-change row-number rotation-side)
  (let ((sides (cl-mapcar (lambda (x) (elt kubik-representation x)) sides-to-change))
        (rows (cl-mapcar (lambda (x) (get-row kubik-representation x row-number)) (shift-list sides-to-change))))
    (change-by-list sides rows row-number 'change-row)
    (aset kubik-representation rotation-side (rotate-side (elt kubik-representation rotation-side)))))

; top side - 0
; front side - 1
; left side - 2
; right side - 3
; back side - 4
; bottom side - 5

;; r l u d f b
(defun kubik-r-move (kubik-representation)
  "Just R move."
  (kubik-column-move kubik-representation '(0 1 5 4) 2 3))

(defun kubik-l-move (kubik-representation)
  "Just L move."
  (kubik-column-move kubik-representation '(1 0 4 5) 0 2))

(defun kubik-u-move (kubik-representation)
  "Just U move."
  (kubik-row-move kubik-representation '(2 1 3 4) 0 0))

(defun rotate-x (y x a)
  (cond ((= a 90) (- y))
        ((= a -90) y)
        ((= a 180) (- x))
        ((= a -180) (- x))))

(defun rotate-y (y x a)
  (cond ((= a 90) x)
        ((= a -90) (- x))
        ((= a 180) (- y))
        ((= a -180) (- y))))

(defun rotate-side-prime (side)
  "Rotate -90 degrees."
  (let ((rotated-side (make-vector 9 0)))
    (dotimes (row 3)
      (dotimes (column 3)
        (let* ((y (1- row))
               (x (1- column))
               (new-y (1+ (rotate-y y x -90)))
               (new-x (1+ (rotate-x y x -90))))
          (aset rotated-side (get-coord new-y new-x)
                             (elt side (get-coord row column))))))
    rotated-side))

(defun rotate-side (side)
  "Rotate SIDE."
  (let ((rotated-side (make-vector 9 0)))
    (dotimes (row 3)
      (dotimes (column 3)
        (let* ((y (1- row))
               (x (1- column))
               (new-y (1+ (rotate-y y x 90)))
               (new-x (1+ (rotate-x y x 90))))
          (aset rotated-side (+ (* 3 new-y) new-x)
                             (elt side (+ (* 3 row) column))))))
    rotated-side))
