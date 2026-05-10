+;; distribute_over_tranches -- distribute calls to MEVAL over processes
;;
;; Copyright 2025 by Robert Dodier.
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

(defparameter *mut* (sb-thread:make-mutex)) ;; protects all-thr

(defmfun $distribute_over_tranches (expr expr-loop-var m n)

  (let ((ii-tranches (divide-into-tranches m n))
	(results (make-array n))
	(all-thr ())
	(child-values (make-array n)))
    (dotimes (i n)
       (let ((indices-to-process (aref ii-tranches i))
	     (child-stream (make-string-output-stream))
	     thr) 
	 (setq thr (sb-thread:make-thread 
		    (lambda (ij)
		      (let*		
			  ((f (lambda (j)
				(meval (maxima-substitute (1+ j) expr-loop-var expr))))			
			   (g (lambda (j) (mfuncall '$string (funcall f j))))
			   (g-f-j (mapcar g indices-to-process)) 
			   (write-s (lambda (s) (write-string s child-stream)))
			   (write-$ (lambda () (write-string "$" child-stream))))		       
			(mapcar (lambda (s)(funcall write-s s) (funcall write-$)) g-f-j)
			(sb-thread:with-mutex  (*mut*)
			  (push (list ij thr) all-thr))
			(get-output-stream-string child-stream)))
		    :arguments (list i)))))
 
    ;; Collect  the results when  child threads are finished. Read one by one the results of
    ;; threads, put them in the array results and do this exactly n times. After that all child
    ;; threads are joined so the following is exclusively in main thread. One could look at the
    ;; return value of join-thread. If it is not true, there is an error in the thread.


    (let (one-thr (sl nil) (thr-write nil) (k 0))
      (loop
    	 (sb-thread:with-mutex (*mut*) 
    	   (if all-thr ;; non void list of threads
    	       (progn (incf k)
    		      (setq one-thr (pop all-thr))
    		      (setq thr-write t))
    	       (setq sl t))) ;; if void sleep, but not with mutex held
    	 (if thr-write
    	     (progn 
    	       (setf (aref results (first one-thr))
    		     (sb-thread:join-thread (second one-thr)))
    	       (setq thr-write nil)))
    	 (if sl (sleep .1)) ;; Avoid looping uselessly
    	 (setq sl nil)
    	 (if (= k n) (return))))
	     

    (dotimes (i n)
      (let ((child-values-1 (make-array (length (aref ii-tranches i)))))
    	(with-input-from-string (sti (aref results i))
    	  (dotimes (j (length (aref ii-tranches i)))
    	    (setf (aref child-values-1 j) (third (mread-raw sti)))))
    	(setf (aref child-values i) child-values-1)))
    
    (let ((all-values-array (make-array m)))
      (dotimes (i n) 
    	(dotimes (j (length (aref ii-tranches i)))
    	  (setf (aref all-values-array (nth j (aref ii-tranches i)))
    		(aref (aref child-values i) j))))
      (cons '(mlist) (coerce all-values-array 'list)))))


;; (%i1) x: [1234, 2345, 3456, 4567, 5678, 6789, 7890];
;; (%o1)             [1234, 2345, 3456, 4567, 5678, 6789, 7890]
;; (%i2) :lisp(load "distribute_over_tranches_thread.lisp")

;; T
;; (%i2) distribute_over_tranches ('(ifactors (x[i])), i, 7, 4);
;; (%o2) [[[2, 1], [617, 1]], [[5, 1], [7, 1], [67, 1]], [[2, 7], [3, 3]], 
;; [[4567, 1]], [[2, 1], [17, 1], [167, 1]], [[3, 1], [31, 1], [73, 1]], 
;; [[2, 1], [3, 1], [5, 1], [263, 1]]]
;; (%i3) is (% = map (ifactors, x));
;; (%o3)                                true
