(in-package :llm)

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

(defun pushjso (key value jso)
  "Push a kev value pair into a json object"
  (let ((found (assoc key (st-json::jso-alist  jso) :test 'string=)))
    (cond (found
           (setf (cdr found)
                 (cond ((consp (cdr found))
                        (cons value (cdr found)))
                       (t (list value (cdr found))))))
          (t (push (cons key value) (st-json::jso-alist  jso))))))

(defun json-string (jso)
  "Turn a json object into a string."
;;;  (format t "json-string ~S~%" (type-of jso))
  (let ((s (make-string-output-stream)))
    (write-json jso s)
    (get-output-stream-string s)))


