
;;
;;  Scripting for Shapes  (Scheme and Python)
;;
;;  Copyright 2023 Edward Blake
;;
;;  See the file "license.terms" for information on usage and redistribution
;;  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;
;;     $Id$
;;

(use gauche.threads)

(define (relative_path_from_absolute Path1 Path2)
	(define (split_components Path)
		(define Path_1 (list->string (map (lambda (C) (if (eq? C #\\) #\/ C)) (string->list Path))))
		(filter (lambda (Str) (not (equal? "" Str))) (string-split Path_1 "/")))
	(define (detour P1 P2)
		(if (not (pair? P2))
			P1
			(cons ".." (detour P1 (cdr P2))))
		)
	(define Path1L (split_components Path1))
	(define Path2L (split_components Path2))
	(if (not (equal? (car Path1L) (car Path2L)))
		Path1
		(let ()
			(define PathList
				(let loop ((Path1 Path1L) (Path2 Path2L))
					(if (not (pair? Path2))
						Path1
						(if (equal? (car Path1) (car Path2))
							(loop (cdr Path1) (cdr Path2))
							(detour Path1 Path2)
							)
						)
					))
			(string-join PathList "/"))
		)
	)

(define (filename_dir Path1)
	(define (split_components Path)
		(define Path_1 (list->string (map (lambda (C) (if (eq? C #\\) #\/ C)) (string->list Path))))
		(filter (lambda (Str) (not (equal? "" Str))) (string-split Path_1 "/")))
	(define Path1L (split_components Path1))
	(string-join (reverse (cdr (reverse Path1L))) "/")
	)

;; Gauche scheme does a flush automatically after newline
(define (**flush-out)
	(flush-all-ports))

(define (**add-to-load-path p)
	;; We only need run-time modification of the load-path variable
	(set! *load-path* (cons p *load-path*))
	)

