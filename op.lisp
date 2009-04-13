(in-package :cl-op)
  
(defun starts-with (list head)
  "Does LIST start with HEAD?"
  (and (consp list) (eql (first list) head)))
  
(defun rnotany (predicate tree &key (recur-if #'consp))
  "Recursive NOTANY."
  (if (funcall recur-if tree) 
      (every (lambda (tree) (rnotany predicate tree :recur-if recur-if)) tree)
      (not (funcall predicate tree))))
  
(defun recurp (form)
  "Is FORM non-terminal?"
  (not (or (atom form) 
           (some (lambda (head) (starts-with form head)) '(quote op op*))
           (and (starts-with form 'function) (symbolp (second form))))))

(defun walk-subforms (function form accumulator &optional result)
  "Walk subforms of FORM."
  (if form
      (multiple-value-bind (subform accumulator) 
                           (walk function (first form) accumulator)
        (walk-subforms function (rest form) accumulator (cons subform result)))
      (values (reverse result) accumulator)))
           
(defun walk (function form &optional accumulator)
  "Walk FORM applying FUNCTION to each node."         
  (multiple-value-bind (form accumulator end-walk-p) 
                       (funcall function form accumulator)
    (cond (end-walk-p (values form accumulator))
          ((recurp form) (walk-subforms function form accumulator))
          (t (values form accumulator)))))
            
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
                (if (rest-slot-p subform)
                    (values argname (append (list argname '&rest) arguments))
                    (values argname (cons argname arguments))))
              (values subform arguments)))
        form))

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
        form))

(defun with-bindings (bindings form)
  "Lexically enclose FORM in BINDINGS."
  (if bindings `(let ,bindings ,form) form))

(defmacro op* (&rest form)
  "Make an anonymous function with implicit arguments. Defer evaluation."
  (multiple-value-bind (form slots) (slots-to-arguments form)
    `(lambda ,(reverse slots) ,form)))
            
(defmacro op (&rest form &environment environment)
  "Make an anonymous function with implicit arguments."
  (multiple-value-bind (form invariants) 
                       (lift-invariants form :environment environment)
    (with-bindings invariants `(op* ,@form))))
