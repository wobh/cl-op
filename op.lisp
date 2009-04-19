(in-package :cl-op)
               
(defun walk (function form &key accumulator (recur-if #'consp))
  "Walk FORM applying FUNCTION to each node."         
  (labels ((walk-subforms (form accumulator result)
             (if form
                 (destructuring-bind (subform . rest) form
                   (multiple-value-bind (subform accumulator) 
                                        (walk function subform 
                                              :accumulator accumulator
                                              :recur-if recur-if)
                     (walk-subforms rest accumulator (cons subform result))))
                 (values (reverse result) accumulator))))
    (multiple-value-bind (form accumulator end-walk-p) 
                         (funcall function form accumulator)
      (cond (end-walk-p (values form accumulator))
            ((funcall recur-if form) (walk-subforms form accumulator nil))
            (t (values form accumulator))))))

(defun rnotany (predicate tree &key (recur-if #'consp))
  "Recursive NOTANY."
  (if (funcall recur-if tree) 
      (every (lambda (tree) (rnotany predicate tree :recur-if recur-if)) tree)
      (not (funcall predicate tree))))
            
(defun recurp (form)
  "Is FORM non-terminal?"
  (not (or (atom form) 
           (member (first form) '(quote op op*))
           (and (eq (first form) 'function) (symbolp (second form))))))
            
(defun simple-slot-p (object)
  "Is OBJECT a simple slot designator?"
  (eq object (intern "_")))

(defun rest-slot-p (object)
  "Is OBJECT a rest slot designator?"
  (eq object (intern "__")))
  
(defun slotp (object)
  "Is OBJECT a slot designator?"
  (or (simple-slot-p object) (rest-slot-p object)))
                      
(defun slots-to-arguments (form)
  "Assign names to slots."
  (walk (lambda (subform arguments)
          (if (slotp subform)
              (let ((argname (gensym "OP-")))
                (values argname (if (rest-slot-p subform)
                                    (append (list argname '&rest) arguments)
                                    (cons argname arguments))))
              (values subform arguments)))
        form :recur-if #'recurp))

(defun liftablep (form)
  "Is FORM suitable for early evaluation?"
  (and (recurp form) (rnotany #'slotp form :recur-if #'recurp)))

(defun special-form-p (form)
  "Is FORM a special form?"
  (and (consp form) (special-operator-p (first form))))
    
(defun lift-invariants (form &key environment)
  "Bind subforms suitable for early evaluation."
  (walk (lambda (subform bindings)
          (let ((expansion (macroexpand subform environment)))
            (if (liftablep expansion) 
                (let ((bind (or (find subform bindings  
                                      :test #'equal :key #'second)
                                (list (gensym) subform))))
                  (values (first bind) (adjoin bind bindings :test #'equal)))
                (values expansion bindings (special-form-p expansion)))))
        form :recur-if #'recurp))

(defmacro op* (&rest form)
  "Make an anonymous function with implicit arguments. Defer evaluation."
  (multiple-value-bind (form slots) (slots-to-arguments form)
    `(lambda ,(reverse slots) ,form)))
            
(defmacro op (&rest form &environment env)
  "Make an anonymous function with implicit arguments."
  (multiple-value-bind (form invariants) 
                       (lift-invariants form :environment env)
    (if invariants 
        `(let ,invariants (op* ,@form)) 
        `(op* ,@form))))
