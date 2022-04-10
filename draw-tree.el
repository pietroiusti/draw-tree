;; -*- lexical-binding: t -*-

(defvar draw-tree-result nil "holds return value of draw-tree")
(defconst *nothing* (cons 'N '()))
(defconst *visited* (cons 'V '()))

(defun draw-tree (n)

  (setq draw-tree-result "\n")

  (defun emptyp (x) (eq x *nothing*))

  (defun visitedp (x) (eq (car x) *visited*))

  (defun mark-visited (x) (cons *visited* x))

  (defun members-of (x) (cdr x))

  (defun donep (x)
    (and (consp x) (visitedp x) (null (cdr x))))

  (defun draw-fixed-string (s)
    (let* ((b (make-string 8 ?\s))
           (k (length s))
           (s (if (> k 7) (substring s 0 7) s))
           (s (if (< k 3) (concat " " s) s))
           (k (length s)))
      (setq draw-tree-result
	   (concat draw-tree-result (concat s (substring b 0 (- 8 k)))))))

  (defun draw-atom (n)
    (cond ((null n)
	   (draw-fixed-string "()"))
	  ((symbolp n)
	   (draw-fixed-string (symbol-name n)))
	  ((numberp n)
	   (draw-fixed-string (number-to-string n)))
	  ((stringp n)
	   (draw-fixed-string (concat "\"" n "\"")))
	  ((char-displayable-p n)
	   (draw-fixed-string (concat "\\" (string n))))
	  ((not (eq n nil))
	   (draw-fixed-string "t"))
	  ((eq n nil)
	   (draw-fixed-string "nil"))
	  (t
	   (error "draw-atom: unknown type" n))))

  (defun draw-conses (n)
    (defun draw-conses-inner (n r)
      (cond ((not (consp n))
             (draw-atom n)
             (nreverse r))
            ((null (cdr n))
             (setq draw-tree-result (concat draw-tree-result "[o|/]"))
             (nreverse (cons (car n) r)))
            (t
             (setq draw-tree-result (concat draw-tree-result "[o|o]---"))
             (draw-conses-inner (cdr n) (cons (car n) r)))))
    (draw-conses-inner n '()))

  (defun draw-bars (n)
    (defun draw-bars-inner (n)
      (cond ((not (consp n)) nil)
            ((emptyp (car n))
             (draw-fixed-string "")
             (draw-bars-inner (cdr n)))
            ((and (consp (car n))
                  (visitedp (car n)))
             (draw-bars-inner (members-of (car n)))
             (draw-bars-inner (cdr n)))
            (t
             (draw-fixed-string "|")
             (draw-bars-inner (cdr n)))))
    (draw-bars-inner (members-of n)))

  (defun skip-empty (n)
    (if (and (consp n)
	     (or (emptyp (car n))
		 (donep (car n))))
	(skip-empty (cdr n))
      n))
  
  (defun remove-trailing-nothing (n)
    (reverse (skip-empty (reverse n))))
  
  (defun all-verticalp (n)
    (or (not (consp n))
	(and (null (cdr n))
             (all-verticalp (car n)))))
  
  (defun draw-members (n)
    (defun draw-members-inner (n r)
      (cond ((not (consp n))
             (mark-visited
              (remove-trailing-nothing
               (reverse r))))
            ((emptyp (car n))
             (draw-fixed-string "")
             (draw-members-inner (cdr n)
				 (cons *nothing* r)))
            ((not (consp (car n)))
             (draw-atom (car n))
             (draw-members-inner (cdr n)
				 (cons *nothing* r)))
            ((null (cdr n))
             (draw-members-inner (cdr n)
				 (cons (draw-final (car n)) r)))
            ((all-verticalp (car n))
             (draw-fixed-string "[o|/]")
             (draw-members-inner (cdr n)
				 (cons (caar n) r)))
            (t
             (draw-fixed-string "|")
             (draw-members-inner (cdr n)
				 (cons (car n) r)))))
    (draw-members-inner (members-of n) '()))

  (defun draw-final (n)
    (cond ((not (consp n))
           (draw-atom n)
           *nothing*)
          ((visitedp n)
           (draw-members n))
          (t
           (mark-visited (draw-conses n)))))

  (defun draw-tree-inner (n)
    (if (not (donep n))
        (progn (setq draw-tree-result (concat draw-tree-result "\n"))
               (draw-bars n)
               (setq draw-tree-result (concat draw-tree-result "\n"))
               (draw-tree-inner (draw-members n)))))
  
  (if (not (consp n))
      (draw-atom n)
    (draw-tree-inner (mark-visited (draw-conses n))))
  
  (setq draw-tree-result (concat draw-tree-result "\n"))
  draw-tree-result)
