(in-package :llm)

(defvar *default-vector-database-dir* ".")
(defvar *default-vector-database-name* "sample-vector-database")

(defstruct (vector-database
            (:print-function
             (lambda (vector-database stream depth)
               (declare (ignore depth))               
               (format stream "#<vector-database ~a ~a ~a ~a>"
                       (vector-database-name vector-database)
                       (vector-database-properties vector-database)
                       (length (vector-database-property-vectors vector-database))
                       (length (vector-database-embedding-vectors vector-database))))))
  (name *default-vector-database-name* :type string)
  (embedder nil :type function)
  (properties nil :type list) ;; property list includes "dim" (# dimensions) and "name"
  (property-vectors nil :type list)
  (embedding-vectors nil :type list))


(defun describe-vector-database (vector-database) ;; describe vector database
  "for debugging"
  (let ((name (vector-database-name vector-database))
        (size (length (vector-database-embedding-vectors vector-database)))
        (dim (length (car (vector-database-embedding-vectors vector-database)))))
    (log-llm "~a ~a dimensions ~a vectors~%" name dim size)
    (values dim size name)))

(defun validate-vector-database (vector-database) ;; validate vector database
  "for debugging"
  (multiple-value-bind (dim size name) (describe-vector-database vector-database)
    (declare (ignore name dim))
    (let ((vectors (vector-database-embedding-vectors vector-database)))
      (dotimes (n size)
        (handler-case
        (let* ((vec (nth n vectors))
               (delta (abs (- (mag vec) 1.0))))
          (when (> delta 0.001)
            (log-llm "~a ~a~%" n (mag vec))))
        (error (e) (error "index ~a has error ~a~%" n e)))))))

(defun find-in-vector-database (vector-database text) ;;
  "find a term in the database"
  (loop for items in (vector-database-property-vectors vector-database) do
    (destructuring-bind (id original-text . properties) items
      (declare (ignore properties))
      (when (string= text original-text) (log-llm "~a~%" id)))))

(defun trim-vector-database (vector-database n) ;;
  "keep only the first n elements of a database"
  (setf (vector-database-embedding-vectors vector-database)
        (subseq (vector-database-embedding-vectors vector-database) 0 n))
  (setf (vector-database-property-vectors vector-database)
        (subseq (vector-database-property-vectors vector-database) 0 n))
  vector-database)

(defun rename-vector-database (vector-database name)
  "Change the name of the vector database"
  (setf (vector-database-name vector-database) name))

(defun write-all-elements (vec stream)
"Using write-vector, there is a chance that only part of the array will be written, especially if the system's buffers are clogged due to so many writes. This loop checks how much has been written and then goes back to write the rest will ensure that everything actually is written."
  (let ((start 0)
        (end (* 4 (length vec))))
    (loop while (< start end)
      do (setq start (+ start (write-vector vec stream :start start :end end))))
    (values start)))

(defun write-vector-database (vector-database &key (dir *default-vector-database-dir*))
  "Write the vector database struct in the specified directory dir."
  (let* ((dim (length (car (vector-database-embedding-vectors vector-database))))
         (name (vector-database-name vector-database))
         (vectors-filename (format nil "~a/~a.vec" dir name))
         (properties-filename (format nil "~a/~a.dat" dir name))
         (vectors-checkpoint-filename (format nil "~a.checkpoint" vectors-filename))
         (properties-checkpoint-filename (format nil "~a.checkpoint" properties-filename))
         (properties `(("dim" . ,dim) ("name" . ,name))))
    (setf (vector-database-properties vector-database) properties)
    ;;(log-llm "writing ~S~%" properties)
    (with-open-file (out vectors-checkpoint-filename :direction :output :if-exists :supersede)
      (dolist (item (vector-database-embedding-vectors vector-database))
        (write-all-elements item out)
        ))
    (with-open-file (out properties-checkpoint-filename :direction :output :if-exists :supersede)
      (fasl-write name out)
      (fasl-write (vector-database-properties vector-database) out)
      (fasl-write (vector-database-property-vectors vector-database) out))
    (when (probe-file vectors-filename) (delete-file vectors-filename))
    (when (probe-file properties-filename) (delete-file properties-filename))
    (rename-file vectors-checkpoint-filename vectors-filename)
    (rename-file properties-checkpoint-filename properties-filename)))


(defun read-vector-with-timeout (vector stream timeout)
  "With read-vector, depending on transmission line speed and data availability, there is a chance that you won't get a full buffer. This wrapper function loops on read-vectorr, but be aware that it may hang forever if the data never becomes available."
  (let* ((start 0)
         (end (* 4 (length vector))) ;; 4 for single-floats!
         (start-time (get-internal-real-time))
         (elapsed-time 0))
    (loop while (< elapsed-time timeout) do
      (let ((bytes-read (read-vector vector stream :start start :end end)))
        (setq start (+ start bytes-read))
        (setq elapsed-time (- (get-internal-real-time) start-time)))
      (when (= start end)
        (return-from read-vector-with-timeout end)))
    (error "Timeout occurred while reading vector from stream")))

(defun read-vector-database (name &key (dir *default-vector-database-dir*))
  "Read the vector database named name from the directory dir."
  (let ((vector-database nil)
        (embedding-vectors nil)
        (vectors-filename (format nil "~a/~a.vec" dir name))
        (properties-filename (format nil "~a/~a.dat" dir name)))
    (cond ((null (probe-file vectors-filename))
           (setf vector-database (make-vector-database :name name)))
          (t
           (destructuring-bind (name properties property-vectors)
               (fasl-read properties-filename)
             (let ((dim (cdr (assoc "dim" properties :test 'string=))))
               (setf vector-database
                     (make-vector-database :name name :properties properties :property-vectors property-vectors))
               (with-open-file (in vectors-filename)
                 (dotimes (i (length property-vectors))
                   (let ((arr (make-array dim :element-type 'single-float)))
                     (read-vector-with-timeout arr in 1000)
                     (push arr embedding-vectors))))
               (setf (vector-database-embedding-vectors vector-database) (reverse embedding-vectors))))))
    vector-database))

(defun sumsq (v w)
  (let ((sum 0.0))
    (dotimes (index (length v))
      (let ((diff (- (aref v index) (aref w index))))
        (setf sum (+ sum (* diff diff)))))
    sum))

(defun nn (vector-database text &key (min-score 0.8) (top-n 10) (similarity 'dotproduct)) ;;; #'simdot::simdot))
  "Find the top-n nearest neighbors in the database with match score at least min-score."
  (let ((shortq (init-shortq min-score top-n 'cadr))
        (embed (funcall (vector-database-embedder vector-database) text)))
    (loop for item in (vector-database-property-vectors vector-database)
          for vec in  (vector-database-embedding-vectors vector-database) do
            (assert (= (length vec) (length embed)))
            (destructuring-bind (id original-text . properties) item
              (let ((score (funcall similarity embed vec)))
;;;                (assert (<= score  1.0) (score label) (log-llm "score=~a text=~a~%" score text))
                (insert-shortq-if shortq `(,id ,score ,original-text ,@properties)))))
    (shortq-item-pq shortq)))

#|
(typep (aref (coerce (embed "Hello") 'array) 0) 'single-float)
*read-default-float-format*
|#

(defun mag (array-or-list)
  "Calculate the magnitde of a vector (array of floats)."
    (let ((sum 0.0)
            (array (coerce array-or-list 'array)))
    (dotimes (i (length array))  (incf sum (* (aref array i) (aref array i))))
    (sqrt sum)))


(defun test-read-write ()
  (let ((vector-database (read-vector-database "sample-vector-database")))
  (rename-vector-database vector-database "sample-vector-database.copy")
  (write-vector-database vector-database))
)
