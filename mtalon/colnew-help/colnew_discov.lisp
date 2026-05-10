;;; Input in the form:  diff(F,x,n)=... (lower order terms), conditions in the form
;;; C(F,...)(point)=0                     . Example (see example3):
;;; depends([G,H],x)$ 
;;; list-of-eqns:[ diff(G,x,2)=L^2*s*(G-1)-L*((3-n)/2*H*diff(G,x)+(n-1)*diff(H,x)*G),
;;; diff(H,x,3)= L^3*(1-G^2)+L^2*s*diff(H,x)-L*((3-n)/2*H*diff(H,x,2)+n*(diff(H,x))^2)]$
;;; list-of-conds:[G(0),H(0),diff(H,x)(0),(G-1)(1),diff(H,x)(1)]$
;;; Further implicit parameters, like L, s, n above may appear and used in continuation
;;; Output: n, variables z1,...,zn, and function f(x,z1,....,zn), a list of all eqs.
;;; Idem for conditions a list g(z1,....,zn) of all conds on cond points.
;;; The functions f(x,z1,....,zn) and g(z1,....,zn) are stored as maxima functions, hence
;;; define special variables $f, $g to which they are attached.
;;; Also give access to the many parameters of colnew: ncomp, mstar, ntol etc.

(declaim (special $f $g  $zvec $dmval $var0 $var_list $m_list $ntol $nfixpnt $fixpnt
		  $ncomp $mstar $aleft $aright $ipar |$zeta|))

(defun eq-disc (eqn)

  "Treat one maxima equation. It should be of the form diff(f,x,n)=... Check that."

  (if (not (equal (caar eqn) 'MEQUAL))
      (merror (intl:gettext "eqn:  needs an equation.")))
  (let* ((lhs (cadr eqn))
	 (diffop (caar lhs))
	 (functn (cadr lhs))
	 (variable (caddr lhs))
	 (order (cadddr lhs)))
    (if (not (equal diffop '%DERIVATIVE))
	(merror (intl:gettext "eqn: needs a differential equation.")))
    (if (> order 4) (merror (intl:gettext "Maximum degree of equation 4")))
    (values order functn variable)))

(defun cond-disc (condt)
  
  "Treat one cond, extract the condition and the point at which it is given. To keep things simple
   assume the condition is given in the form C(F,G,..)(p)=0 at point p. For example:
   :lisp #$$ (G+diff(G,x)-1)(3)=0$ -> ((MEQUAL) ((MQAPPLY) ((MPLUS) $g (($DIFF) $g $X) ((MMINUS) 1)) 3) 0)"
  
  (if (not (and (eql (caar condt) 'MEQUAL) (eql (caddr condt) 0)))
      (merror (intl:gettext "condt: must be of the form C(p)=0.")))
  (let* ((lhs (cadr condt)) 
	 (condit (if (eql (caar lhs) 'MQAPPLY) (cadr lhs) (caar lhs)))
	 (point (if (eql (caar lhs) 'MQAPPLY) (caddr lhs) (cadr lhs))))
    (values condit point)))


    
(defun $eqns_to_funs (list-of-eqns)

  "Treat a maxima list of maxima equations and a list of constraints. Outputs ncomp, mstar,
    m-list, for colnew, and assigns maxima function f and several maxima variables."

  (if (not (equal (caar list-of-eqns) 'MLIST))
      (merror (intl:gettext "Needs a list of equation.")))
  (let ((mstar 0) (ncomp 0) (index 0) m-list  newvar subst-list
	var0 var-list funs-list)
    (setq list-of-eqns (cdr list-of-eqns)) ; from maxima list to lisp list

;;; Treat equations
    (dolist (eqn list-of-eqns)
      (incf ncomp)
      (multiple-value-bind (order functn variable) (eq-disc eqn)
	(if (= 0 index) (setf var0 variable)
	    (if (not (equal variable var0))
		(merror (intl:gettext "Only one variable!"))))
	(push order m-list)
;;;  Introduce the Z-vector
	(dotimes (i order)
	  (setq newvar (intern (concatenate 'string "$Z" (write-to-string (incf index)))))
	  (push newvar var-list)
	  (if (= i 0)
	      (push `((MEQUAL)  ,functn ,newvar) subst-list)
	      (push `((MEQUAL)
		      ((%DERIVATIVE) ,functn ,variable ,i) ,newvar)
		    subst-list)))))
    (setf var-list (nreverse var-list))
    (if (> ncomp 20) (merror (intl:gettext "Maximum number of equations 20")))
    (setf mstar (reduce #'+ m-list))
    (setq m-list (cons '(MLIST) (nreverse m-list)))
    (if (> mstar 40) (merror (intl:gettext "Maximum sum of degrees 40")))
    (let ((max-subst-list (cons '(MLIST) subst-list)))
      (dolist (eqn list-of-eqns)
	; Doesn't work if max-subst-list is reversed!
	(push (simplify ($substitute max-subst-list (caddr eqn))) funs-list))
      (setq funs-list (nreverse funs-list))    
;;; Export variables to maxima
      (meval  `((MSETQ) $max_subst_list ,max-subst-list)))
    (meval  `((MSETQ) $mstar ,mstar))
    (meval  `((MSETQ) $ncomp ,ncomp))
    (meval  `((MSETQ) $m_list ,m-list))
    (meval  `((MSETQ) $var_list ,(cons '(MLIST) var-list)))
    (meval  `((MSETQ) $var0 ,var0))
    (meval  `((MDEFINE) ,(cons '($f) (list* var0  var-list)) ,(cons '(MLIST) funs-list))))
  'DONE)

(defun $conds_to_zconds(list-of-conds $aleft $aright $max_subst_list $m_list)
  
  "Express conditions in terms of the zvector, put them in order and discover if there are interior points to be
   inserted into the list of fix points."
  
  (setq list-of-conds (cdr list-of-conds))
  (if (not (eql (length list-of-conds) $mstar))
      (merror (intl:gettext "Need to have as many conditions as free parameters")))
  (let ((conds-list ()) (fixpnt ()) (zet ()) nfixpnt)
    (dolist (condt list-of-conds)
      (multiple-value-bind (condit point) (cond-disc condt)
	(push (list point (simplify ($substitute $max_subst_list  condit))) conds-list)))
    (setq conds-list (nreverse conds-list))
;;; Ensure points of ZETA are in order and constraints are sorted accordingly
    (setq conds-list (stable-sort conds-list #'< :key #'car))
    (setq zet (mapcar #'car conds-list))
    (setq conds-list (mapcar #'cadr conds-list))
    (dolist (zz zet) (if (and (> zz $aleft) (< zz $aright)
			      (not (member zz fixpnt)))
			 (push zz fixpnt)))
    (setq fixpnt (nreverse fixpnt))
    (setq nfixpnt (length fixpnt))
;;; Define  maxima function g, and maxima variables fixpnt ZETA (zeta is the function zeta)
    (meval  `((MDEFINE) ,(cons '($g) (cdr $var_list)) ,(cons '(MLIST) conds-list)))
;;; |$zeta| becomes ZETA in maxima (Manual, section 37.1) Other maxima variables
    (meval  `((MSETQ) |$zeta| ,(cons '(MLIST) zet)))
    (meval  `((MSETQ) $fixpnt ,(cons '(MLIST) fixpnt)))
    (meval  `((MSETQ) $nfixpnt ,nfixpnt)))
  'DONE)
    


(defun $build_guess ($list_of_guess $m_list $ncomp $var0)

  "list-of-guess is the init-guess, a maxima list of ncomp functions, giving a guess for the
   underivated functions. Construct z-vector and dmval in terms of var0, the variable of diff. eqns."

  (if (not (and (eql (caar $list_of_guess) 'MLIST)
		(eql ($length $list_of_guess) $ncomp)))
      (merror (intl:gettext "Guess must be a maxima list of functions.
                             of length ncomp")))
  (let (z-vec  dmval (ml (cdr $m_list)))
    (dolist (gfun  (cdr $list_of_guess))
      (let ((gexp gfun))
;;; For each guess compute derivatives of guess as indicated by m-list
	(dotimes (k (car ml))
	  (push gexp z-vec)
	  (setf gexp (simplify (meval `(($diff) ,gexp ,$var0)))))
	(push gexp dmval)
	(setq ml (cdr ml))))
    (setq z-vec (nreverse z-vec))
    (setq dmval (nreverse dmval))
    (setq $zvec (cons '(MLIST) z-vec))
    (setq $dmval (cons '(MLIST) dmval))
    (list '(MLIST) $zvec $dmval)))


;;; Example:  list_of_guess: [cos(x) + x^2,x^5]$ m_list:[2,3]$ ncomp:2$ var0:x$
;;; result: [[cos(x)+x^2,2*x-sin(x),x^5,5*x^4,20*x^3],[2-cos(x),60*x^2]]


(defun $build_ipar ($m_list $mstar $ncomp $nfixpnt $ntol &optional ($nmax 40) ($nsubint 5))
  
  ;; by default nmax=40 ntol=mstar tolerances set to 1d-5 ncomp=5
  ;; ntol is the number of tolerances, ltol specifies to what Z components they apply, tol their values.
  ;; for Problem 3 ipar=[1,4,10,2,40000,2500,1,0,1,0,0]
  ;; nmax is the maximum number of subintervals, ispace and fspace are resp  nmax*nsizei  nmax*nsizef.
  ;; One can add further fixed points in the array fixpnt and increase accordingly nfixpnt for example to
  ;; avoid a singularity in the equations, since the equations are never applied on fixed points. See Problem 4.
  
  (let ((mmax (apply 'max (cdr $m_list))))
    (if (= $ncomp 0)  (setq $ncomp (max (1+ mmax) (- 5 mmax)))
	(progn (if (< $ncomp mmax) (setq $ncomp mmax))
	       (if (> $ncomp 7) (setq $ncomp 7))))
    (if (= $nsubint 0) (setq $nsubint 5))
    (if (< $ntol 1) (setq $ntol 1))
    (if (> $ntol $mstar) (setq $ntol $mstar))
    (prepare-tol $ntol $mstar) ; fill tol and ltol with basic defaults
    (let* ((kd (* $nsubint $ncomp))
	   (kdm (+ kd $mstar))
	   (nsizei (+ 3 kdm))
	   (nsizef (+ 4 (* 3 $mstar) (* kdm (+ 5 kd)) (* 4 $mstar $mstar)))
	   ;; space is cheap one sets  (2*mstar-nrec)-> 2*mstar and also finally
	   (ispace (max (* $nmax nsizei) 2500))
	   (fspace (max (* $nmax nsizef) 40000))
	   ;; Here one sets ipar[9]=1 assuming there is a guess, for continuation one has to set
	   ;; ipar[9]=3 and ipar[3]:ispace[1] see the example in Problem 3.
	   (ipar (list '(MLIST) 1 $ncomp $nsubint $ntol fspace ispace 0 0 1 0 $nfixpnt)))

      (setq $ipar ipar)))
  'DONE)		


(defun prepare-tol ($ntol $mstar)
  
  ;; ltol is filled with increasing indices k (1<=k<=mstar) for which the tolerance has to be checked.
  ;; tol gives the corresponding tolerances. For example in problem 3 one has mstar=5, but ntol=2, and one chooses
  ;; ltol=[1,3], so Z1 and Z3 are checked, and tol=[1d-7,1d-7]. To see the correspondance with original variables
  ;; print max_subst_list, here only G and H are checked. If the tolerance on Z_k is eps, that means that
  ;; |Z_k - Z_k^0| < eps * Z_k^0 + eps where Z_k^0 is the exact solution and of course this difference is estimated
  ;; by looking at the rate of convergence of the Z_k after successive subdivisions.
  
  (defparameter $ltol  (cons '(MLIST) (loop for j below $ntol collect (1+ j))))
  (defparameter $tol (cons '(MLIST) (loop for f below $ntol collect 1.0D-5)))
  (if (< $ntol $mstar) (format t "You should edit the arrays ltol and tol ~%")
      (format t "The array tol is filled by default, you can edit tol ~%"))
  'DONE)


  
  

	  
    

    
