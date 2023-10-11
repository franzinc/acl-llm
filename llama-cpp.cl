(in-package :llama2)

(defvar *llama-cpp-python-api-protocol*)
(defvar *llama-cpp-python-api-host*)
(defvar *llama-cpp-python-api-port*)
(defvar *llama-cpp-dimensions* 4096)

(defun call-llama.cpp_Python_API (cmd &key
                          (method :get)
                          (content nil)
                          (timeout 120)
                          (content-type "application/json")
                          (accept "application/json")
                          (extra-headers nil)
                          (query nil)
                          (verbose nil))
  "Generic interface to all llama.cpp Python v1 API functions using do-http-request."
  (let ((uri (format nil "~a://~a:~a/v1/~a"
                     *llama-cpp-python-api-protocol*
                     *llama-cpp-python-api-host*
                     *llama-cpp-python-api-port*
                     cmd)))
    (when verbose (format t "content=~S~%" content))
    (multiple-value-bind (body code headers page socket req)
        (net.aserve.client:do-http-request
          uri
          :headers `(,@extra-headers ("accept" . ,accept))
          :content content
          :content-type content-type
          :timeout timeout
          :query query
          :method method)
      (declare (ignore req socket page))
      (when verbose
        (format t "headers=~S~%" headers)
        (format t "body=~a~%" (length body))
        (format t "body=~S~%" body)
        (format t "code=~a~%" code))
      (let ((jso (handler-case (read-json body)
                   (error (e)
                     (format t "~a: Unable to read json: ~a~%" e body)
                     (jso)))))
        jso))))


(defun ask-llama2-completions-jso (prompt &key (stop `(,(format nil "~%") "###")))
  (let* ((jso (jso)))
    (setf prompt (format nil "~%~%### Instructions:~%~a~%~%### Response:~%" prompt))
    (pushjso "prompt" prompt jso)
       (pushjso "max_tokens" 4096 jso)
       (pushjso "stop" stop jso)
      (call-llama.cpp_Python_API "completions" :method :post :verbose nil
                 :content (json-string jso))))

(defun ask-llama2-models-jso ()
  (call-llama.cpp_Python_API "models" :method :get :verbose nil))

(defun ask-llama2-embeddings-jso (text)
  (let ((jso  (jso)))
    (pushjso "input" text jso)
    (call-llama.cpp_Python_API "embeddings" :method :post :content (json-string jso) :verbose nil)))

(defun ask-llama2-chat-completions-jso (messages)
  (let ((jso (jso))
        (message-array nil))
    (loop for (role . content) in (reverse messages)
          for n from 0 do
            (let ((message-jso (jso)))
              (pushjso "role" role message-jso)
              (pushjso "content" content message-jso)
              (push  message-jso message-array)))
    (pushjso "messages" message-array jso)
    (format t "~S~%" (json-string jso))
    (call-llama.cpp_Python_API "chat/completions" :method :post :verbose nil
                                                  :content (json-string jso))))

(defun ask-chat (prompt &key (verbose nil))
  (let ((jso (ask-llama2-completions-jso prompt)))
    (or (cond (jso
               (let* ((choices (jso-val jso "choices"))
                      (choice (car choices)))
                 (when verbose (format t "choices=~S~%" choices))
             (cond (choice (jso-val choice "text"))))))
        "No text")))


(defun ask-for-list (prompt &key (verbose nil))
  (let* ((response (ask-chat prompt :verbose verbose))
         (items (split-re ",[\\. ]*" response)))
    items))
        

(defun ask-llama2-chat-completions (messages  &key (verbose nil))
  (let ((jso (ask-llama2-chat-completions-jso messages)))
    (or (cond (jso
               (let* ((choices (jso-val jso "choices"))
                      (choice (car choices)))
                 (when verbose (format t "choices=~S~%" choices))
                 (cond (choice (let ((message (jso-val choice "message")))
                                 (cond (message (jso-val message "content")))))))))
        "No text")))



(defun ask-llama2-embeddings (input  &key (verbose nil))
  (declare (ignore verbose))
  (let ((jso (ask-llama2-embeddings-jso input)))
    ;(setf *jso* jso)
    (or (cond (jso
               (let* ((data (jso-val jso "data"))
                      (datum (car data)))
                 (cond (datum (let ((embedding (jso-val datum "embedding")))
                               ;(setf *embedding* embedding)
                               embedding))))))
       nil)))


(defun embed (text &key (normalize t))
  (let ((embedding (ask-llama2-embeddings text)))
    (cond (embedding
           (when normalize (let ((mag (mag embedding)))
                             (setf embedding (mapcar (lambda (u) (/ u mag)) embedding))))
           (setf embedding (coerce embedding 'single-float-array))
           embedding)
          (t (make-array *llama-cpp-dimensions* :element-type 'single-float :initial-element 0.0)))))


#|
(defun sample-vector-database ()
  (let* ((dim *llama-cpp-dimensions*)
         (name *default-vector-database-name*)
         (vector-database (make-vector-database :name name
                                                :embedder #'embed
                                                :properties `(("dim" . ,dim) ("name" . ,name))))
         (type-name "Historical Figure")
         (type-plural "Historical Figures") ;;; (ask-chat (format nil "pluralize ~a" type-name)))
         (elements (ask-for-list (format nil "List 100 members of the set of all ~a, comma-separated." type-plural))))
    (setf elements (remove-duplicates elements :test 'string-equal))
    (setf (vector-database-embedder vector-database) #'llama2::embed)
    (format t "~a:~%" type-plural)
    (dolist (elt elements)
      (let* ((vec (embed elt))
             (id (gentemp "id-"))
             (properties (list id elt type-name)))
        (format t "~a~%" elt)
        (push properties (vector-database-property-vectors vector-database))
        (push vec (vector-database-embedding-vectors vector-database))))
    (write-vector-database vector-database)
    vector-database))

(defun t2 () (let ((vector-database (sample-vector-database)))
   (nn vector-database "Famous scientist" :embedder 'embed :min-score 0.0)))
|#
