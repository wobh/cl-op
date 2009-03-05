(in-package :cl-op)

(defun alist-keys (alist)
  "Get keys of an alist."
  (mapcar #'car alist))
  
(defun alist-datums (alist)
  "Get datums of an alist."
  (mapcar #'cdr alist))

(defun starts-with (list head)
  "Does list start with head?"
  (and (consp list) (eql (first list) head)))
  
(defun recurp (form)
  "Is form non-terminal?"
  (not (or (atom form) 
           (starts-with form 'quote) 
           (and (starts-with form 'function) (symbolp (second form))))))

(defun rnotany (predicate tree &key (recur-if #'consp))
  "Recursive notany."
  (if (funcall recur-if tree) 
      (every (lambda (tree) (rnotany predicate tree :recur-if recur-if)) tree)
      (not (funcall predicate tree))))
         
(defun lambda-form-p (form)
  "Is form a lambda form?"
  (and (starts-with form 'function) (starts-with (second form) 'lambda)))
  
(defun walk (fn form)
  "Walk form applying fn to each node."
  (labels ((rec (form) (mapcar (lambda (form) (walk fn form)) form))
           (rec-special (form)
             (destructuring-bind (special first . rest) form
               `(,special ,first ,@(rec rest)))))
    (multiple-value-bind (form endp) (funcall fn form)
      (cond (endp form)
            ((lambda-form-p form) `(function ,(rec-special (second form))))
            ((starts-with form 'the) (rec-special form))
            ((recurp form) (rec form))
            (t form)))))
  
(defun simple-slot-p (obj)
  "Is obj a simple slot designator?"
  (eq obj (intern "_")))

(defun rest-slot-p (obj)
  "Is obj a rest slot designator?"
  (eq obj (intern "__")))
  
(defun slotp (obj)
  "Is obj a slot designator?"
  (or (simple-slot-p obj) (rest-slot-p obj)))
                
(defun make-bind (datum)
  "Assign a fresh alias to datum."
  (cons (gensym "OP-") datum))
      
(defmacro collect-bind (obj binds)
  "Make a binding for obj push it onto binds and return the new alias."
  `(car (first (push (make-bind ,obj) ,binds))))
  
(defun collect-slots (form)
  "Assign names to slots."
  (let ((slots))
    (flet ((collect (arg)
             (cond ((rest-slot-p arg)
                    (push (cons '&rest nil) slots)
                    (collect-bind arg slots))
                   ((simple-slot-p arg) (collect-bind arg slots))
                   (t arg))))
      (values (walk #'collect form) (reverse slots)))))

(defun liftablep (form)
  "Is form suitable for early evaluation?"
  (and (recurp form) (rnotany #'slotp form :recur-if #'recurp)))
      
(defun collect-invariants (form)
  "Bind subforms suitable for early evaluation."
  (declare (special *env*))
  (let ((invariants))
    (flet ((collect (arg)             
             (if (liftablep (macroexpand arg *env*))
                 (or (car (rassoc arg invariants :test #'equal))
                     (collect-bind arg invariants))
                 arg)))
      (values (walk #'collect form) invariants))))

(defun with-binds (binds form)
  "Lexically enclose form in binds."
  (if binds
      `(let ,(mapcar #'list (alist-keys binds) (alist-datums binds)) ,form)
      form))
      
(defmacro op* (&rest args &environment *env*)
  "Create an anonymous function with implicit arguments. Defer evaluation."
  (declare (special *env*))
  (multiple-value-bind (form slots) (collect-slots args)
    (assert (notany #'rest-slot-p (alist-datums (butlast slots))))    
    `(lambda ,(alist-keys slots) ,form)))
            
(defmacro op (&rest args &environment *env*)
  "Create an anonymous function with implicit arguments."
  (declare (special *env*))
  (multiple-value-bind (args invariants) (collect-invariants args)
    (with-binds invariants `(op* ,@args))))
 
(defmacro pfuncall (&rest args)
  "Partial funcall."
  `(op funcall ,@args))  
  
(defmacro pfuncall* (&rest args) 
  "Partial funcall with defered evaluation."
  `(op* funcall ,@args))
 
(defmacro papply (&rest args)
  "Partial apply."
  `(op apply ,@args))  
  
(defmacro papply* (&rest args) 
  "Partial apply with defered evaluation."
  `(op* apply ,@args))

(defmacro pmultiple-value-call (&rest args) 
  "Partial multiple-value-call with defered evaluation."
  `(op multiple-value-call ,@args))
  
(defmacro pmultiple-value-call* (&rest args) 
  "Partial multiple-value-call with defered evaluation."
  `(op* multiple-value-call ,@args))
  
(defun flip (fn)
  "Switch the first two arguments of fn."
  (lambda (x y &rest args) (apply fn y x args)))
