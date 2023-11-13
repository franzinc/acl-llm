(in-package :llm)

#|
The macro log-llm allows acl-llm to compile with ACL and Agraph.
|#

(defmacro log-llm (log-llm-control-string &rest log-llm-args)
  #-acl-llm-build`(db.agraph.log:log-info :llm ,log-llm-control-string ,@log-llm-args)
  #+acl-llm-build`(format t ,log-llm-control-string ,@log-llm-args))

#|
This next section was kind of fun to figure out.

The problem was to define a macro so that we could define functions
involving 3 macro substitutions
1. Replace ,@key-args-signature with the list of key parameters and their default values.
2. In the body of the function, replace @,key-args-signature with the calling signature as a list of :key val
3. In the body of the function, replace @,key-args-pushjso with the code to push the key-values onto the JSON object.
A simple evocation might be
  (key-args-fun 'ask-bot "Ask the bot a question"
     `(ask-llama2-completions prompt-or-messages ,@key-args-signature))
The function name is ask-bot and the body contains an instance of  ,@key-args-signature.
The macro should expand to

(defun ask-bot (prompt-or-messages &key ... many keyword params ...)
  (ask-llama2-completions prompt-or-message :key val ... calling signature with all keys...))

All of the functions defined this way take prompt-or-messages as their first argument, followed by the keyword arguments.
Optionally, the body of the function includes ,@key-args-signature and/or ,@key-args-pushjso.

The implementation involves a function and a macro.
The function key-args-fun calls a macro args-list-macro to create the form of the definition for the function named 'name'.
The macro returns a defun form which still possibly contains the unexpanded expressions ,@key-args-signature and/or @,key-args-pushjso)
The function key-args-fun macroexpands this form in order to expand the @,key-args-signature.
Finally we eval the expanded form to actually define the named function.

|#

(defvar key-args-list nil)
(defvar key-args-signature nil)
(defvar key-args-pushjso nil)


(defmacro args-list-macro (name body)
  ;; body must be a single s-expression
  `(defun ,name (,(intern "prompt-or-messages") &key ,@key-args-list &allow-other-keys)
     ,body))

(defmacro key-args-fun (name comment body)
  ;; body must be a single s-expression
  (declare (ignore comment))
  `(args-list-macro ,name ,(eval body)))


(defun read-lines-from-stream (stream &key ((:limit limit) 100000))
  "read lines of text from a stream (up to a limit) and append them together"
  (let ((result ""))
      (loop for line = (read-line stream nil)
            for x from 1
        while (and (< x limit) line)
        do
               (setf result (cond ((= x 1) line) (t (format nil "~a~%~a" result line)))))
    result))

(defun jso-val (jso key)
  "Look up value for key in json object"
  (let* ((alist (st-json::jso-alist  jso))
         (val (cdr (assoc key alist :test 'string=))))
    val))

(defun delete-jso-val (key jso)
  (let ((found (assoc key (st-json::jso-alist  jso) :test 'string=)))
;;;    (log-llm "found=~a~%" found)
    (when found (setf (cdr found) nil))
    jso))


(defun pushjso (key value jso)
  "Push a kev value pair into a json object"
  (let ((found (assoc key (st-json::jso-alist jso) :test 'string=)))
    (cond (found
           (setf (cdr found)
                 (cond ((consp (cdr found))
                        (cons value (cdr found)))
                       (t (list value (cdr found))))))
          (t (push (cons key value) (st-json::jso-alist  jso))))
    jso))

(defun json-string (jso)
  "Turn a json object into a string."
;;;  (log-llm "json-string ~S~%" (type-of jso))
  (let ((s (make-string-output-stream)))
    (write-json jso s)
    (get-output-stream-string s)))

;; no can do.
;; need some kind of fwrapper to temporarily change what this does
#+ignore (defmethod st-json::write-json-element ((element real) stream) (format stream "~,8f" element))
