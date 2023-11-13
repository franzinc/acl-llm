(in-package :gpt)

(defvar *ada-002-dimensions* 1536)

(eval-when (compile load eval)
  (setq key-args-list '((verbose nil)
                        (log-progress t)
                        (model "text-embedding-ada-002")
                        (timeout 120))

        key-args-signature '(:verbose verbose :log-progress log-progress :model model :timeout timeout)))


(key-args-fun ask-embedding ""
              '(let* ((jso (jso))
                      (start-time (get-internal-real-time)))
                (pushjso "input" prompt-or-messages jso)
                (pushjso "model" model jso)
                (let* ((response
                        (call-openai "embeddings" :method :post :timeout timeout :verbose verbose
                                                  :content (json-string jso)))
                       (error (jso-val response "error" ))
                       (error-message (when error (jso-val error "message"))))
                  (when error-message (log-llm "error-message=~a~%" error-message))
                  (when log-progress
                    (log-llm "Embed ~a ~a~%" prompt-or-messages (- (get-internal-real-time) start-time))
                    )
                  (values response error-message))))

(key-args-fun embed ""
              `(multiple-value-bind (jso error-message) (ask-embedding prompt-or-messages ,@key-args-signature)
                 (let* ((data (car (jso-val jso "data")))
                        (embedding (when data (jso-val data "embedding"))))
                   (cond (embedding (setf embedding (mapcar (lambda (u) (coerce u 'single-float)) embedding))
                                    (setf embedding (coerce embedding 'single-float-array)))
                         (t (setf embedding (make-array *ada-002-dimensions* :element-type 'single-float :initial-element 0.0))))
                   (values embedding error-message))))


#|
(defun sample-vector-database ()
  (declare (special *default-vector-database-name*))
  (let ((name *default-vector-database-name*)
        (dim *ada-002-dimensions*))
    (let* ((vector-database (make-vector-database :name *default-vector-database-name*
                                                  :embedder #'embed
                                                  :properties `(("dim" . ,dim)
                                                                ("name" . ,name))))
           (type-name "Historical Figure")
           (type-plural (ask-chat (format nil "pluralize ~a" type-name)))
           (elements (ask-for-list (format nil "List 100 members of the set of all ~a" type-plural))))
      (setf elements (remove-duplicates elements :test 'string-equal))
      (log-llm "~a:~%" type-plural)
      (dolist (elt elements)
        (let* ((vec (embed elt))
               (id (gentemp "id-"))
               (properties (list id elt type-name)))
          (log-llm "~a~%" elt)
          (push properties (vector-database-property-vectors vector-database))
          (push vec (vector-database-embedding-vectors vector-database))))
      (write-vector-database vector-database)
      vector-database)))

(defun t1 ()
  (let ((vector-database (sample-vector-database)))
    (nn vector-database "Famous Philosopher")))
|#
