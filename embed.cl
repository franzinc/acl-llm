(in-package :gpt)

(defvar *ada-002-dimensions* 1536)

(defun ask-embedding (text &key (model "text-embedding-ada-002") (timeout 120))
  "Call OpenAI to ask for a JSON object that contains an embedding vector.
   May throw an exception if API call fails.  Suggest using long timeout."
   (let* ((jso (jso)))
    (pushjso "input" text jso)
    (pushjso "model" model jso)
      (call-openai "embeddings" :method :post :timeout timeout :verbose nil
                 :content (json-string jso))))

(defun embed (text &key (verbose nil) (model "text-embedding-ada-002") (timeout 120))
  "Embedding function to return a normalized unit vector of single precision floats."
  (when verbose (format t "Embed '~a'.~%" text))
  (let* ((data (car (jso-val (ask-embedding text :model model :timeout timeout) "data")))
         (embedding (when data (jso-val data "embedding"))))
    (cond (embedding (setf embedding (mapcar (lambda (u) (coerce u 'single-float)) embedding))
                     (coerce embedding 'single-float-array))
          (t (make-array *ada-002-dimensions* :element-type 'single-float :initial-element 0.0)))))


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
      (format t "~a:~%" type-plural)
      (dolist (elt elements)
        (let* ((vec (embed elt))
               (id (gentemp "id-"))
               (properties (list id elt type-name)))
          (format t "~a~%" elt)
          (push properties (vector-database-property-vectors vector-database))
          (push vec (vector-database-embedding-vectors vector-database))))
      (write-vector-database vector-database)
      vector-database)))

(defun t1 ()
  (let ((vector-database (sample-vector-database)))
    (nn vector-database "Famous Philosopher")))
