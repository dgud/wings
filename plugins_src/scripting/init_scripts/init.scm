
;;
;;  Scripting for Shapes  (init for Scheme)
;;
;;  Copyright 2023 Edward Blake
;;
;;  See the file "license.terms" for information on usage and redistribution
;;  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;
;;     $Id$
;;

;;
;; This scheme code acts as a boot strap to define some useful functions
;; and then load the actual script. The actual script is sent by the erlang
;; plugin as a symbolic expression through standard input.
;;

;; Main function of script, can be called multiple times
;;
(define *scr-main-fun* #f)
(define (main-function a)
	(set! *scr-main-fun* a))

;; The full path of the script being run
(define *script-full-path* "")

;; The directory of the script being run, the script
;; might want to use this to load it's other files.
(define *script-directory* "")

;; Parameters being passed to the script from the script's
;; parameter options window.
(define *params* '())

;; Extra parameters passed as the third argument
;; These are parameters that are not set via a parameter
;; window and may be auxiliary variables dependent on the
;; type of script (e.g. "content" contains an e3d_file tuple
;; for exporter plugins, parameters set with "script_params"
;; also show up in here).
(define *extra-params* '())


(define **scr-langstrs '())

(define (set-lang-string List)
	(set! **scr-langstrs
		(map (lambda (A)
				(cons (vector-ref A 0)
					  (vector-ref A 1)) )
			List))
	)


(define (?__ Id Str)
	(define Found (assq Id **scr-langstrs))
	(if (eq? Found #f)
		Str
		(cdr Found))
	)


;; Used by the constructors for E3D objects
;;
(define (**loader-construct-from sc default names)
	(define s-name (car default))
	(define s-defaults (cdr default))
	(cons s-name
		(let loop ((sn names) (sd s-defaults))
			(if (equal? '() sn)
				'()
				(let ((res (assq (car sn) sc)))
				(if res
					(cons (list-ref res 1) (loop (cdr sn) (cdr sd)))
					(cons (car sd)         (loop (cdr sn) (cdr sd))))))
			)
		)
	)

;; Useful functions for e3d records
(define (e3d_transf? l) (equal? 'e3d_transf (list-ref l 0)))
(define (e3d_transf-mat l) (list-ref l 1)) ; 
(define (e3d_transf-inv l) (list-ref l 2)) ; 
(define (make-e3d_transf . sc)
	(define default (list 'e3d_transf '()  '()))
	(define names   (list             'mat 'inv))
	(**loader-construct-from sc default names)
	)

(define (ray? l) (equal? 'ray (list-ref l 0)))
(define (ray-o l) (list-ref l 1)) ; 
(define (ray-d l) (list-ref l 2)) ; 
(define (ray-n l) (list-ref l 3)) ; Near, far (or MinT MaxT)
(define (ray-f l) (list-ref l 4)) ; 
(define (ray-bfc l) (list-ref l 5)) ; Backface culling?
(define (make-ray . sc)
	(define default (list 'ray #(0 0) #(0 0) 0.0 1.0 #t))
	(define names   (list      'o  'd  'n  'f 'bfc))
	(**loader-construct-from sc default names)
	)

(define (e3d_face? l) (equal? 'e3d_face (list-ref l 0)))
(define (e3d_face-vs l) (list-ref l 1)) ; List of vertex indices.
(define (e3d_face-vc l) (list-ref l 2)) ; Vertex color indices.
(define (e3d_face-tx l) (list-ref l 3)) ; List of texture indices.
(define (e3d_face-ns l) (list-ref l 4)) ; List of normal indices.
(define (e3d_face-mat l) (list-ref l 5)) ; Materials for face.
(define (e3d_face-sg l) (list-ref l 6)) ; Smooth group for face.
(define (e3d_face-vis l) (list-ref l 7)) ; Visible edges (as in 3DS).
(define (make-e3d_face . sc)
	(define default (list 'e3d_face '() '() '() '() '() 1 -1))
	(define names   (list           'vs 'vc 'tx 'ns 'mat 'sg 'vis))
	(define sc_1 (map
		(lambda (b)
			(if (eq? (car b) 'mat)
				;; Ensure that mat stays a list of atoms
				(let ((matlist (list-ref b 1)))
					(list (car b) (cons "!list" matlist)))
				b
				)
			) sc))
	(**loader-construct-from sc_1 default names)
	)

(define (e3d_mesh? l) (equal? 'e3d_mesh (list-ref l 0)))
(define (e3d_mesh-type l) (list-ref l 1)) ; 'triangle | 'quad | 'polygon
(define (e3d_mesh-vs l) (list-ref l 2)) ; Vertex table (list).
(define (e3d_mesh-vc l) (list-ref l 3)) ; Vertex color table (list).
(define (e3d_mesh-tx l) (list-ref l 4)) ; Texture coordinates (list).
(define (e3d_mesh-ns l) (list-ref l 5)) ; Normal table (list).
(define (e3d_mesh-fs l) (list-ref l 6)) ; Face table (list of e3d_face).
(define (e3d_mesh-he l) (list-ref l 7)) ; List of chains of hard edges.
(define (e3d_mesh-matrix l) (list-ref l 8)) ; Local coordinate system.
(define (make-e3d_mesh . sc)
	(define default (list 'e3d_mesh 'poly '() '() '() '() '() '() 'identity))
	(define names   (list           'type  'vs  'vc  'tx  'ns  'fs 'he  'matrix))
	(**loader-construct-from sc default names)
	)

(define (e3d_object? l) (equal? 'e3d_object (list-ref l 0)))
(define (e3d_object-name l) (list-ref l 1)) ; Name of object (string)
(define (e3d_object-obj l) (list-ref l 2)) ; Object implementation.
(define (e3d_object-mat l) (list-ref l 3)) ; Materials for this object.
(define (e3d_object-attr l) (list-ref l 4)) ; List of attributes.
(define (make-e3d_object . sc)
	(define default (list 'e3d_object 'undefined #f '() '()))
	(define names   (list             'name 'obj  'mat  'attr))
	(**loader-construct-from sc default names)
	)

(define (e3d_file? l) (equal? 'e3d_file (list-ref l 0)))
(define (e3d_file-objs l) (list-ref l 1)) ; List of objects.
(define (e3d_file-mat l) (list-ref l 2)) ; List of materials.
(define (e3d_file-creator l) (list-ref l 3)) ; Creator string.
(define (e3d_file-dir l) (list-ref l 4)) ; Directory for file.
(define (make-e3d_file . sc)
	(define default (list 'e3d_file '() '() "" ""))
	(define names   (list           'objs 'mat 'creator 'dir))
	(**loader-construct-from sc default names)
	)
; (e3d_file `(objs ,objs) `(mat ,mat))
;   


(define (e3d_image? l) (equal? 'e3d_image (list-ref l 0)))
(define (e3d_image-type l) (list-ref l 1))
(define (e3d_image-bytes_pp l) (list-ref l 2))
(define (e3d_image-alignment l) (list-ref l 3))
(define (e3d_image-order l) (list-ref l 4))
(define (e3d_image-width l) (list-ref l 5))
(define (e3d_image-height l) (list-ref l 6))
(define (e3d_image-image l) (list-ref l 7))
(define (e3d_image-filename l) (list-ref l 8))
(define (e3d_image-name l) (list-ref l 9))
(define (e3d_image-extra l) (list-ref l 10))
(define (make-e3d_image . sc)
	(define default (list 'e3d_image 'r8g8b8 3 1 'lower_left 0 0 #f 'none '() '()))
	(define names   (list            'type   'bytes_pp 'alignment 'order 'width 'height 'image 'filename 'name 'extra))
	(**loader-construct-from sc default names)
	)


(define (**send-back-list l)
	(newline)
	(write l)
	(newline)
	(**flush-out))

(define (**send-back-list-ep l)
	(newline (current-error-port))
	(write l (current-error-port))
	(newline (current-error-port))
	(**flush-out))
	

(define (**loader-get-script-directory path)
	;; Assuming only R5RS available, we need a function
	;; to get the directory.
	(let ((len (string-length path)))
		(let ((chr (string-ref path (- len 1)))
		      (sub (substring path 0 (- len 1))))
			(if (or (eq? chr #\\) (eq? chr #\/))
				(substring path 0 len)
				(**loader-get-script-directory sub)))))

(define (wings-set-variable! varname varval)
	(**send-back-list (list '%setvar "var1" '(1 2 3 4)))
	(let ((reply (read)))
	  reply)
	)
(define (wings-query str)
	(**send-back-list (list '%query str))
	(let ((reply (read)))
	  reply)
	)
(define (wings-pb-message . Args)
	(define A1 (car Args))
	(define A2 (cdr Args))
	(cond
		((eq? A2 '())
			(wings-pb-message-1 A1))
		(#t
			(wings-pb-message-2 A1 (car A2)))))

(define (wings-pb-message-2 Prc Str)
	(**send-back-list-ep (list '%pbmessage Prc Str)))

(define (wings-pb-message-1 Str)
	(**send-back-list-ep (list '%pbmessage 1.0 Str)))

(define (wings-result-text List)
	(if (string? List)
		(wings-result-text (list List))
		(**send-back-list (list '%resulttext List))
		)
	)


;; Long running process
;;
;; Example:
;; 
;; (display "Started")(newline)
;; (long-running-process
;; 	(lambda ()
;; 		(let loop ((N 100000000))
;; 			(if (> N 0)
;; 				(loop (- N 1))
;; 				'ok))
;; 	))
;; (display "Done")(newline)
;; 

(define (long-running-process Fun)
	;; A thread that simply sends keep-alive messages back to the scripting
	;; plugin to keep it from timing out while the script processes something
	;; that takes a while.
	;;
	(define StandbyState #f)
	(define StandbySema  (make-semaphore 1))
	(define StandbyThread
		(make-thread (^[] (guard (e [else (report-error e) #f])
			(define (get_val)
				(semaphore-acquire! StandbySema)
				(let ((Val (if StandbyState #t #f)))
					(semaphore-release! StandbySema)
					Val))
			(let loop ()
				(if (get_val)
					'done
					(let ()
						;; The standard output seems to have some sort of buffering
						;; that prevents it from sending from this thread, current
						;; workaround is sending to standard error to bypass the buffering
						(**send-back-list-ep (list '%keepalive 0))
						(thread-sleep! 1)
						(loop))))
		))))

	(thread-start! StandbyThread)
	(unwind-protect (Fun)
		(semaphore-acquire! StandbySema)
		(set! StandbyState #t)
		(semaphore-release! StandbySema)
		(thread-join! StandbyThread)
		)
	)


(define (script-loop)
	(define cmd (read))
	(case (list-ref cmd 0)
		((run_init)
			(begin
				(set! *script-full-path* (list-ref cmd 1))
				(set! *script-directory* (**loader-get-script-directory *script-full-path*))
				(**add-to-load-path *script-directory*)
				(set-lang-string (list-ref cmd 2))
				(begin (load *script-full-path*))
				(if (equal? #f *scr-main-fun*)
					(begin
						(display "ERROR: main function not set" (current-error-port))
						(newline (current-error-port))
						(exit))
					(begin
						(**send-back-list'(ok))
						(script-loop))))
			)
		((run)
			(begin
				(let ((params (list-ref cmd 2))
					  (extra-params (list-ref cmd 3)))
					(*scr-main-fun* params extra-params))
				(newline)
				(**flush-out)
				(**send-back-list '(%ok))
				(script-loop))
			)
		(else
			(begin
				(display "Not run")
				(newline)
				(exit))))
	)

(script-loop)

