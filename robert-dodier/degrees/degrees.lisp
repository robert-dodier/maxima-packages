(setf (get '$degrees 'dimension) 'dimension-degrees)

(displa-def degrees dimension-postfix #.(string #\DEGREE_SIGN))
(displa-def minutes dimension-postfix #.(string #\PRIME))
(displa-def seconds dimension-postfix #.(string #\DOUBLE_PRIME))

(defun dimension-degrees (form result)
  (let ((args (rest form)))
    (cond
      ((= (length args) 3)
       (multiple-value-bind (d m s) (values-list args)
         (dimension-nary `((mtimes) ((degrees) ,d) ((minutes) ,m) ((seconds) ,s)) result)))
      ((= (length args) 2)
       (multiple-value-bind (d m) (values-list args)
         (dimension-nary `((mtimes) ((degrees) ,d) ((minutes) ,m)) result)))
      ((= (length args) 1)
       (multiple-value-bind (d) (values-list args)
         (dimension-postfix `((degrees) ,d) result)))
      (t
        (dimension-function form result)))))
