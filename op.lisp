(in-package :cl-op)

(defun disjoin (&rest predicates)
  "Combine results of PREDICATES by OR."
  (lambda (&rest args) (some (lambda (p) (apply p args)) predicates)))
  
(defun conjoin (&rest predicates)
  "Combine results of PREDICATES by AND."
  (lambda (&rest args) (every (lambda (p) (apply p args)) predicates)))
  
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
      
(defun walk (fn form)
  "Walk FORM applying FN to each node."
  (multiple-value-bind (form end-walk-p) (funcall fn form)
    (cond (end-walk-p form)
          ((recurp form) (mapcar (lambda (form) (walk fn form)) form))
          (t form))))
            
(defun simple-slot-p (obj)
  "Is OBJ a simple slot designator?"
  (eq obj (intern "_")))

(defun rest-slot-p (obj)
  "Is OBJ a rest slot designator?"
  (eq obj (intern "__")))
  
(defun slotp (obj)
  "Is OBJ a slot designator?"
  (or (simple-slot-p obj) (rest-slot-p obj)))
                      
(defun collect-slots (form)
  "Assign names to slots."
  (let ((slots))
    (flet ((collect (arg)
             (cond ((slotp arg)
                    (when (rest-slot-p arg) (push '&rest slots))
                    (first (push (gensym "OP-") slots)))
                   (t arg))))
      (values (walk #'collect form) (reverse slots)))))

(defun liftablep (form)
  "Is FORM suitable for early evaluation?"
  (and (recurp form) (rnotany #'slotp form :recur-if #'recurp)))
    
(defun specialp (form)
  "Is FORM a special form or a macro call?"
  (and (consp form) 
       (funcall (disjoin #'special-operator-p #'macro-function) (first form))))

(defmacro with-rebinder (collector-fn &body body)
  "Locally define collector function for bindings named COLLECTOR-FN."
  (let ((bindings (gensym)))
    `(let ((,bindings ,(second collector-fn)))
       (flet ((,(first collector-fn) (&optional obj) 
                (if obj
                    (first (or (find obj ,bindings :test #'equal :key #'second)
                               (first (push (list (gensym) obj) ,bindings))))
                    (reverse ,bindings))))
         ,@body))))

(defun collect-invariants (form &key env)
  "Bind subforms suitable for early evaluation."
  (with-rebinder (bind)
    (flet ((collect (arg)             
             (if (liftablep (macroexpand arg env)) 
                 (bind arg) 
                 (values arg (specialp arg)))))
      (values (walk #'collect form) (bind)))))

(defun with-bindings (bindings form)
  "Lexically enclose FORM in BINDINGS."
  (if bindings `(let ,bindings ,form) form))
      
(defmacro op* (&rest args)
  "Make an anonymous function with implicit arguments. Defer evaluation."
  (multiple-value-bind (form slots) (collect-slots args)
    `(lambda ,slots ,form)))
            
(defmacro op (&rest args &environment env)
  "Make an anonymous function with implicit arguments."
  (multiple-value-bind (args invariants) (collect-invariants args :env env)
    (with-bindings invariants `(op* ,@args))))
   
(defun flip (fn)
  "Switch the first two arguments of FN."
  (lambda (x y &rest args) (apply fn y x args)))
