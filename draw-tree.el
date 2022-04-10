;; -*- lexical-binding: t -*-

(defvar draw-tree-result "" "holds return value of draw-tree")
(defconst draw-tree-*nothing* (cons 'N '()))
(defconst draw-tree-*visited* (cons 'V '()))

(defun draw-tree-emptyp (x) (eq x draw-tree-*nothing*))

(defun draw-tree-visitedp (x) (eq (car x) draw-tree-*visited*))

(defun draw-tree-mark-visited (x) (cons draw-tree-*visited* x))

(defun draw-tree-members-of (x) (cdr x))

(defun draw-tree-donep (x)
  (and (consp x) (draw-tree-visitedp x) (null (cdr x))))

(defun draw-tree-draw-fixed-string (s)
  (let* ((b (make-string 8 ?\s))
         (k (length s))
         (s (if (> k 7) (substring s 0 7) s))
         (s (if (< k 3) (concat " " s) s))
         (k (length s)))
    (setq draw-tree-result
	  (concat draw-tree-result (concat s (substring b 0 (- 8 k)))))))

(defun draw-tree-draw-atom (n)
  (cond ((null n)
	 (draw-tree-draw-fixed-string "()"))
	((symbolp n)
	 (draw-tree-draw-fixed-string (symbol-name n)))
	((numberp n)
	 (draw-tree-draw-fixed-string (number-to-string n)))
	((stringp n)
	 (draw-tree-draw-fixed-string (concat "\"" n "\"")))
	((char-displayable-p n)
	 (draw-tree-draw-fixed-string (concat "\\" (string n))))
	((not (eq n nil))
	 (draw-tree-draw-fixed-string "t"))
	((eq n nil)
	 (draw-tree-draw-fixed-string "nil"))
	(t
	 (error "draw-tree-draw-atom: unknown type" n))))

(defun draw-tree-draw-conses (n)
  (defun draw-tree-draw-conses-inner (n r)
    (cond ((not (consp n))
           (draw-tree-draw-atom n)
           (nreverse r))
          ((null (cdr n))
           (setq draw-tree-result (concat draw-tree-result "[o|/]"))
           (nreverse (cons (car n) r)))
          (t
           (setq draw-tree-result (concat draw-tree-result "[o|o]---"))
           (draw-tree-draw-conses-inner (cdr n) (cons (car n) r)))))
  (draw-tree-draw-conses-inner n '()))

(defun draw-tree-draw-bars (n)
  (defun draw-tree-draw-bars-inner (n)
    (cond ((not (consp n)) nil)
          ((draw-tree-emptyp (car n))
           (draw-tree-draw-fixed-string "")
           (draw-tree-draw-bars-inner (cdr n)))
          ((and (consp (car n))
                (draw-tree-visitedp (car n)))
           (draw-tree-draw-bars-inner (draw-tree-members-of (car n)))
           (draw-tree-draw-bars-inner (cdr n)))
          (t
           (draw-tree-draw-fixed-string "|")
           (draw-tree-draw-bars-inner (cdr n)))))
  (draw-tree-draw-bars-inner (draw-tree-members-of n)))

(defun draw-tree-skip-empty (n)
  (if (and (consp n)
	   (or (draw-tree-emptyp (car n))
	       (draw-tree-donep (car n))))
      (draw-tree-skip-empty (cdr n))
    n))

(defun remove-trailing-nothing (n)
  (reverse (draw-tree-skip-empty (reverse n))))

(defun draw-tree-all-verticalp (n)
  (or (not (consp n))
      (and (null (cdr n))
           (draw-tree-all-verticalp (car n)))))

(defun draw-tree-draw-members (n)
  (defun draw-tree-draw-members-inner (n r)
    (cond ((not (consp n))
           (draw-tree-mark-visited
            (remove-trailing-nothing
             (reverse r))))
          ((draw-tree-emptyp (car n))
           (draw-tree-draw-fixed-string "")
           (draw-tree-draw-members-inner (cdr n)
			       (cons draw-tree-*nothing* r)))
          ((not (consp (car n)))
           (draw-tree-draw-atom (car n))
           (draw-tree-draw-members-inner (cdr n)
			       (cons draw-tree-*nothing* r)))
          ((null (cdr n))
           (draw-tree-draw-members-inner (cdr n)
			       (cons (draw-final (car n)) r)))
          ((draw-tree-all-verticalp (car n))
           (draw-tree-draw-fixed-string "[o|/]")
           (draw-tree-draw-members-inner (cdr n)
			       (cons (caar n) r)))
          (t
           (draw-tree-draw-fixed-string "|")
           (draw-tree-draw-members-inner (cdr n)
			       (cons (car n) r)))))
  (draw-tree-draw-members-inner (draw-tree-members-of n) '()))

(defun draw-tree-draw-final (n)
  (cond ((not (consp n))
         (draw-tree-draw-atom n)
         draw-tree-*nothing*)
        ((draw-tree-visitedp n)
         (draw-tree-draw-members n))
        (t
         (draw-tree-mark-visited (draw-tree-draw-conses n)))))

(defun draw-tree (n)
  (setq draw-tree-result "\n")

  (defun draw-tree-inner (n)
    (if (not (draw-tree-donep n))
        (progn (setq draw-tree-result (concat draw-tree-result "\n"))
               (draw-tree-draw-bars n)
               (setq draw-tree-result (concat draw-tree-result "\n"))
               (draw-tree-inner (draw-tree-draw-members n)))))
  
  (if (not (consp n))
      (draw-tree-draw-atom n)
    (draw-tree-inner (draw-tree-mark-visited (draw-tree-draw-conses n))))
  
  (setq draw-tree-result (concat draw-tree-result "\n"))
  draw-tree-result)
