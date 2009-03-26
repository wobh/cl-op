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
      
(defun walk (function form)
  "Walk FORM applying FUNCTION to each node."
  (multiple-value-bind (form end-walk-p) (funcall function form)
    (cond (end-walk-p form)
          ((recurp form) (mapcar (lambda (form) (walk function form)) form))
          (t form))))
            
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
  (let ((slots))
    (values (walk (lambda (node)
                    (cond ((slotp node)
                           (when (rest-slot-p node) (push '&rest slots))
                           (first (push (gensym "OP-") slots)))
                          (t node))) 
                  form)
            (reverse slots))))

(defun liftablep (form)
  "Is FORM suitable for early evaluation?"
  (and (recurp form) (rnotany #'slotp form :recur-if #'recurp)))

(defun special-form-p (form)
  "Is FORM a special form?"
  (and (consp form) (special-operator-p (first form))))
  
(defmacro with-rebinder (collector &body body)
  "Locally define collector function for bindings named COLLECTOR."
  (let ((binds (gensym)))
    `(let ((,binds ,(second collector)))
       (flet ((,(first collector) (&optional datum) 
                (if datum
                    (first (or (find datum ,binds :test #'equal :key #'second)
                               (first (push (list (gensym) datum) ,binds))))
                    (reverse ,binds))))
         ,@body))))

(defun lift-invariants (form &key environment)
  "Bind subforms suitable for early evaluation."
  (with-rebinder (bind)
    (values (walk (lambda (node)
                    (let ((expansion (macroexpand node environment)))
                      (if (liftablep expansion) 
                          (bind node) 
                          (values expansion (special-form-p expansion)))))
                  form) 
            (bind))))

(defun with-bindings (bindings form)
  "Lexically enclose FORM in BINDINGS."
  (if bindings `(let ,bindings ,form) form))

(defmacro op* (&rest form)
  "Make an anonymous function with implicit arguments. Defer evaluation."
  (multiple-value-bind (form slots) (slots-to-arguments form)
    `(lambda ,slots ,form)))
            
(defmacro op (&rest form &environment environment)
  "Make an anonymous function with implicit arguments."
  (multiple-value-bind (form invariants) 
                       (lift-invariants form :environment environment)
    (with-bindings invariants `(op* ,@form))))
