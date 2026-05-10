;; distribute_over_tranches -- distribute calls to MEVAL over processes
;;
;; Copyright 2025 by Robert Dodier.
;; I release this work under terms of the GNU General Public License, version 2.
;;
;; Given the task of evaluating an expression EXPR for values
;; of a symbol EXPR-LOOP-VAR = 1, 2, 3, ..., M.
;; Launch N processes to handle the work and divide the M values
;; into N exhaustive and mutually exclusive subsets.
;; The worker processes write their results back to the manager,
;; which assembles the results into the order 1, 2, 3, ..., M.
;;
;; distribute_over_tranches evaluates all of its arguments,
;; so EXPR may need to be quoted to delay evaluation.
;;
;; This implementation makes use of POSIX functions (fork, exit,
;; and waitpid for process management, and pipe, read, write, and
;; close for interprocess communication), as exposed by the
;; SB-POSIX symbol package in SBCL. As such this code can only work,
;; as it stands, in SBCL; another Lisp implementation which has POSIX
;; functions would require minor modifications (presumably only to
;; change the name of the symbol package).
;; Non-POSIX Lisps cannot work.
;;
;; Example:
;;
;; (%i1) load ("distribute_over_tranches.lisp");
;; (%i2) x: [1234, 2345, 3456, 4567, 5678, 6789, 7890];
;; (%i2) distribute_over_tranches ('(ifactors (x[i])), i, 7, 4);
;; (%i3) is (% = map (ifactors, x));
;;
;; See rtest_distribute_over_tranches.mac for other examples.

(defun divide-into-tranches (m n)
  (let ((ll (make-array n :initial-element nil)))
    (dotimes (i m) (push i (aref ll (mod i n))))
    (dotimes (i n) (setf (aref ll i) (reverse (aref ll i))))
    ll))

(defun clean-processes (child-pids n)
  ;; One must take care of the case when PID is already killed.
  (flet ((clean-process (pid)
           (if pid ( handler-case (progn
                     (sb-posix:kill pid sb-posix:sigterm)
                     (sb-posix:waitpid  pid 0))
                     (sb-posix:syscall-error () nil)))))
    (dotimes (j n) (clean-process (aref child-pids j)))))

(defun clean-fds (rw-fds n)
  (dotimes (j n) (if (aref rw-fds j)
		     (handler-case
			 (progn
			   (sb-posix:close (first (aref rw-fds j)))
			   (sb-posix:close (second  (aref rw-fds j))))					  
		       (sb-posix:syscall-error () nil)))))



(defmfun $distribute_over_tranches (expr expr-loop-var m n)

  (let
      ((ii-tranches (divide-into-tranches m n))
       (child-pids (make-array n :initial-element nil))
       (p-streams (make-array n :initial-element nil)) ;; parent read streams
       (rw-fds (make-array n :initial-element nil)) ;; read and write file descriptors
       (child-values (make-array n)))
   
    (block clean
      (dotimes (i n)

	(handler-bind ((sb-posix:syscall-error
			(lambda(c)
			  (format t "Reduce concurrency: ~&~a n= ~d ~%" c n)
			  (format t "Cleaning child processes ~%")
			  (clean-processes child-pids n)
			  (clean-fds rw-fds n)
			  (invoke-restart 'cleanup)))
		       (sb-sys:interactive-interrupt
			(lambda(c)
			  (format t "Keyboard interrupt  ~&~a ~%" c)
			  (clean-processes child-pids n)
			  (clean-fds rw-fds n)
			  (return-from clean)
			  (invoke-restart 'abort)))
		       (maxima-$error
			(lambda(c) (declare (ignore c))
			  (format t "Maxima error when i = ~d ~%" i)
			  (invoke-restart 'compute-error)))
		       (error ;; any other lisp error such as arithmetic-error ...
			(lambda(c) (declare (ignore c))
			  (format t "Lisp error when i = ~d ~%" i)
			  (invoke-restart 'compute-error))))
			 
		   		   
	
	  (let ((indices-to-process (aref ii-tranches i))
		(fd-pair (multiple-value-list
			  (restart-case (sb-posix:pipe)
			    (cleanup () (return-from clean))))))
	    (setf (aref rw-fds i) fd-pair)
	    
	    (let ((child-pid (restart-case (sb-posix:fork)
			       (cleanup () (return-from clean))))) 
	      ;; it is important to prevent file descriptor reuse when some fds are closed
	      ;; otherwise read and write fds become confused.
	      
	      (if (eql child-pid 0) 
		  (progn ;; in child, first close all reading pipes
		    (sb-posix:close (first (aref rw-fds i)))
		    (dotimes (j (1- i)) sb-posix:close (first (aref rw-fds j)))
		    (setq errcatch t)
		    (let*
			((child-stream (sb-impl::make-fd-stream
					(second (aref rw-fds i)) :input nil :output t :serve-events t))
			 (f (lambda (j)
			      (restart-case ;; Detect maxima and lisp computation errors.
				  (meval (maxima-substitute (1+ j) expr-loop-var expr))
				(compute-error ()
				  (format t "~%*************~%")
				  (format t "Error for index ~d~%" (1+ j))
				  (format t "*************~%"))))) 
			 (g (lambda (j) (mfuncall '$string (funcall f j))))
			 (g-f-j (mapcar g indices-to-process))
			 (write-s (lambda (s) (write-string s child-stream)))
			 (write-$ (lambda () (write-string "$" child-stream))))
		      (mapcar (lambda (s) (funcall write-s s) (funcall write-$)) g-f-j)
		      ;; write several $ terminated maxima expressions to pipe
		      (finish-output child-stream))
		    ;; so that only the above runs in child, closes fds
		    (sb-posix:exit 0))
		  (progn ;; in parent
		    (setf (aref child-pids i) child-pid)
		    (sb-posix:close (second (aref rw-fds i))) ;; close writing pipe
		    (let ((parent-stream  (sb-impl::make-fd-stream
					   (first (aref rw-fds i)) :input t :output nil :serve-events t)))
		      (setf (aref p-streams i) parent-stream)))))))))

    (dotimes (i n)
      ;; This blocks until all childs have exited, thus all child writes done,
      ;; and all write fds closed in childs. Thus reads should not timeout.
      (sb-posix:waitpid (aref child-pids i) 0))
    
    (dotimes (i n)
      (let
          ((string-input (aref p-streams i))
           (child-values-1 (make-array (length (aref ii-tranches i)))))
        (dotimes (j (length (aref ii-tranches i)))
          (setf (aref child-values-1 j) (third (mread-raw string-input))))
	;; read $ terminated maxima expressions from pipe and convert them to lisp
        (setf (aref child-values i) child-values-1))
      (sb-posix:close (first (aref rw-fds i)))) ;; defer closing read fds in parent

    (let ((all-values-array (make-array m)))
      (dotimes (i n)
        (dotimes (j (length (aref ii-tranches i)))
          (setf (aref all-values-array (nth j (aref ii-tranches i)))
		(aref (aref child-values i) j))))
      (cons '(mlist) (coerce all-values-array 'list)))))
