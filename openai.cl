;; See the file "LICENSE" for the full license governing this code.;;

(in-package :gpt)

;; call set-openapi-key before calling any of these functions
(defvar *openai-api-key* "missing")

(defvar *default-fine-tune-model*  "davinci")
(defvar *default-chat-model* "text-davinci-003")
(defvar *default-ask-chat-model* "gpt-3.5-turbo")

(defconstant *ignore-chars*
    (make-array 2 :element-type 'character
                :initial-contents '(#\newline #\space)))

(defun set-openai-api-key (key)
  (setf *openai-api-key*
    (string-trim *ignore-chars* key)))


(defun ask-chat (text-or-alist
                 &key
                   (model *default-ask-chat-model*)
                   (temperature 0.8)
                   (max-tokens 2048)
                   (top-p 1)
                   (timeout 90)
                   (presence-penalty 0.0)
                   (frequency-penalty  0.0)
                   (functions nil)
                   (output-format :text)
                   (function-call nil)
                   (stop "")
                   (verbose nil)
                   (n 1))
  "Use this function for ChatGPT API.
Model should be one of: gpt3-3.5-turbo, gpt3-3.5-turbo-0301 or gpt4.
text-or-alist can be either a simple string or a transcript in the form of an alist
(role . content) ...) where role is one of  \"user\", \"system\", \"assistant\" or \"function\"."
;;;  (format t "ask-chat timeout=~a~%" timeout)
  (let* ((jso (jso))
         (message-array nil))
    (when (stringp text-or-alist)
      (setf text-or-alist `(("user" . ,text-or-alist))))
    (loop  for (role . content) in text-or-alist
           for n from 0 do
             (let ((message-jso (jso)))
               (pushjso "role" role message-jso)
               (pushjso "content" content message-jso)
               (push  message-jso message-array)))

    (pushjso "model" model jso)
    (pushjso "messages" message-array jso)
    (pushjso "temperature" temperature jso)
    (pushjso "top_p" top-p jso)
    (pushjso "max_tokens" max-tokens jso)
    (pushjso "n" n jso)
    (pushjso "stop" stop jso)
    (pushjso "presence_penalty" presence-penalty jso)
    (pushjso "frequency_penalty" frequency-penalty jso)
    (pushjso "user" "anonymous" jso)
    (when functions (pushjso "functions" functions jso))
    (when function-call (pushjso "function_call" function-call jso))
;;;    (format t "~S~%" jso)
    (let* ((resp (call-openai "chat/completions" :timeout timeout :method :post :content (json-string jso) :verbose verbose))
           (choices (when resp (jso-val resp "choices")))
           (err (when resp (jso-val resp "error")))
           (messages (when choices (mapcar (lambda (choice) (jso-val choice "message")) choices)))
           (contents (when messages (mapcar (lambda (message) (string-trim (format nil " ~%")
                                                (jso-val message "content"))) messages)))
           )
      (setf resp
            (cond (err `(,(jso-val err "message")))
                  (contents contents)
                  (t '("No text"))))
      (cond ((and functions (string-equal "null" (car contents)))
             (extract-arguments (car messages)))
            ((equal output-format :text) (car resp)) (t resp)))))


(defun call-openai (cmd &key
                          (method :get)
                          (content nil)
                          (timeout 10)
                          (content-type "application/json")
                          (extra-headers nil)
                          (query nil)
                          (retries 3)
                          (delay 0.25)
                          (verbose nil))
  "Generic interface to all openai v1 API functions using do-http-request."
  (let ((uri (format nil "https://api.openai.com/v1/~a" cmd)))
    (when verbose (format t "content=~S~%" content))
    (multiple-value-bind (body code headers page socket req)
        (net.aserve.client:do-http-request
          uri
          :headers `(,@extra-headers ("Authorization" . ,(format nil "Bearer ~a" *openai-api-key*)))
          :content content
          :content-type content-type
          :timeout timeout
          :query query
          :method method)
      (declare (ignore req socket page))
      (when verbose
        (format t "headers=~S~%" headers)
        (format t "body=~S~%" body)
        (format t "code=~a~%" code))
      (cond ((and (= code 429) (> retries 0)) ;;; HTTP 429 API rate limit exceeded, retry with exponential backoff
             (sleep delay)
             (call-openai cmd :method method :content content :timeout timeout :content-type content-type
                              :extra-headers extra-headers :query query :retries (1- retries) :delay (* 2 delay) :verbose verbose))
            ((/= code 429)
             (let ((jso (handler-case (st-json::read-json body)
                          (error (e)
                            (format t "~a: Unable to read json: ~a~%" e body)
                            (jso)))))
              jso))
            (t (jso))))))


(defun chat (text &key
                    (model *default-chat-model*)
                    (max-tokens 2048)
                    (temperature 0.8)
                    (timeout 10)
                    (presence-penalty 0.0)
                    (frequency-penalty 0.0)
                    (separator "")
                    (stop "")
                    (logprobs nil)
                    (n 1)
                    (output-format :text)
                    (verbose nil)
                    )
  "Use this function for GPT-3 models ada, babbage, davinci.
Simple chatbot function: say (chat \"Hello.\")"
  (let* ((jso (jso))
         (resp nil)
         (choices nil)
         (responses nil))
    (pushjso "prompt" (format nil "~a~a" text separator) jso)
    (pushjso "model" model jso)
    (pushjso "max_tokens" max-tokens jso)
    (pushjso "temperature" temperature jso)
    (pushjso "presence_penalty" presence-penalty jso)
    (pushjso "frequency_penalty" frequency-penalty jso)
    (pushjso "stop" stop jso)
    (when logprobs (pushjso "logprobs" logprobs jso))
    (pushjso "n" n jso)
;;;    (format t "jso=~a~%" (json-string jso))
    (setf resp (call-openai "completions" :method :post :content (json-string jso)
                                          :timeout timeout
                                          :verbose verbose
                                          ))
    (setf choices (jso-val resp "choices"))
    (setf responses (or (mapcar (lambda (u) (string-trim (format nil " ~%") (jso-val u "text"))) choices) '("No text")))
    (when verbose (format t "jso=~S~%" resp))
    (cond ((equal output-format :text) (car responses))
          (t responses))))



(defun list-openai-models ()
  "Simple Lisp example of calling openai API to list available models."
(let* ((jso (call-openai "models")))
  (mapcar
   'print
   (sort
    (remove-if
     'null
     (mapcar
      (lambda (u)
        (let ((id (cdr (assoc "id" (st-json::jso-alist  u) :test 'string=))))
          id
          ))
      (cdr (assoc "data" (st-json::jso-alist  jso) :test 'string=))))
    'string<))
  nil))




(defconstant *70-years-in-seconds* (round (* 70 365.25 3600 24)))
(defconstant *tz-adjust* (- (* 3600 9)))
(defconstant *day-names* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defun date-string (ut)
    "Weird Common Lisp Universal time to Unix time adjustment"
    (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time (+ ut *70-years-in-seconds* *tz-adjust*))
      (declare (ignore tz dst-p))
      (format nil "~2,'0d:~2,'0d:~2,'0d on ~a, ~d/~2,'0d/~d ET"
          hour
          minute
          second
          (nth day-of-week *day-names*)
          month
          date
          year)))


(defun list-openai-files (&key (sort-key "created_at")
                          (stream t))
  "List all files in your openai directory"
  (let* ((file-list (call-openai "files"))
         (data (jso-val file-list "data")))
    (setf data (sort (copy-list data) (lambda (u v) (< (jso-val u sort-key) (jso-val v sort-key)))))
    (let ((collection
    (loop for item in data collect
          (progn
      (format stream "~a ~20,1,0a ~35,1,0@a ~16,1,0@a bytes~%"
              (jso-val item "id")
              (jso-val item "filename")
              (date-string (jso-val item "created_at"))
              (jso-val item "bytes"))
      (list
              (jso-val item "id")
              (jso-val item "filename")
              (date-string (jso-val item "created_at"))
              (jso-val item "bytes"))))))
    (or stream collection))))



;;; Delete the last n=7 uploaded files:
;(mapcar 'delete-openai-file (mapcar 'car (subseq (reverse (list-openai-files :stream nil)) 0 7)))

(defun delete-openai-file (file)
  "Delete a file"
  (call-openai (format nil "files/~a" file) :method :delete))

(defun delete-fine-tuned-model (model)
  "Delete a fine-tuned model"
  (call-openai (format nil "models/~a" model) :method :delete))


(defun fine-tune (file)
  "Start a fine-tuning process"
  (let ((jso (jso)))
    (pushjso "model" *default-fine-tune-model* jso)
    (pushjso "training_file" file jso)
    (call-openai "fine-tunes" :method :post :content (json-string jso))))


(defun fine-tune-status  (&key ((:full full) nil))
  "Get the status of fine-tuning processes."
  (let* ((full-status  (call-openai "fine-tunes" :method :get)))
    (cond (full full-status)
          (t (let* ((data (jso-val full-status "data"))
                    (curr-status (car (last data)))
                    (id (jso-val curr-status "id"))
                    (status (jso-val curr-status "status"))
                    (created-at (jso-val curr-status "created_at"))
                    (updated-at (jso-val curr-status "updated_at")))
               (format t "id=~a~%status=~a~%created:~S~%updated:~S~%" id status
                       (date-string created-at)
                       (date-string updated-at))
              (values id  curr-status))))))


(defun fine-tune-report (&key ((:ftid ftid) (fine-tune-status)))
  "Get the status of a specific fine-tuning process."
  (let* ((response (call-openai (format nil "fine-tunes/~a" ftid) :method :get))
;;;         (hyperparams (jso-val response "hyperparams"))
         (events (jso-val response "events")))
    (loop for event in events do
      (let ((message (jso-val event "message"))
            (created-at (jso-val event "created_at")))
        (format t "~a ~a~%" message (date-string created-at))))
   events))

(defun upload-openai-file (filename)
  "I executed the curl shell command for this one becuases I couldn't figure out how to construct the required
Content-Type: multipart/form-data
from do-http-request.  curl constructs the proper HTTP body with the -F flag:
curl https://api.openai.com/v1/files  -H \"Authorization: Bearer API-KEY\" -F purpose=\"fine-tune\" -F   file='@gpt3-data/mydata37.jsonl'
From the curl trace:
Content-Type: multipart/form-data; boundary=----------------------------3cb0d508edae
------------------------------3cb0d508edae
Content-Disposition: form-data; name=\"purpose\"
fine-tune
------------------------------3cb0d508edae
Content-Disposition: form-data; name=\"file\"; filename=\"mydata37.00d5: jsonl\"
Content-Type: application/octet-stream

The best I could do with do-http-request generated:
Content-Type: application/x-www-form-urlencoded
Authorization: API-KEY
[info] repl-thread: 01/30/23 - 12:47:13 - client sending content of 54 characters/bytes
[xmit-client-request-body] repl-thread: 01/30/23 - 12:47:13 - \"purpose=fine-tune&file='%40gpt3-data%2fmydata37.jsonl'\"
"
#+ignore(let* ((filename "gpt3-data/mydata37.jsonl")
              (query `(("purpose" . "fine-tune") ("file" . ,(format nil "'@~a'" filename)))))
    (format t "query=~S~%" query)
    (call-openai "files" :method :post :query query)
            )

  (let ((cmd
          (format nil
                  "curl https://api.openai.com/v1/files  -H \"Authorization: Bearer ~a\" -F purpose=\"fine-tune\" -F   file='@~a'"
                  *openai-api-key*
                  filename)))
;;;    (format t "~a~%" cmd)
    (multiple-value-bind (shell-stream error-stream process)
        (excl:run-shell-command cmd
                                :output :stream :error-output nil :wait nil)
      (declare (ignore error-stream))
      (let ((txt (read-lines-from-stream shell-stream)))

        (sys:reap-os-subprocess :pid process :wait nil)
        txt)

      )))


(defun read-lines-from-stream (stream &key ((:limit limit) 100000))
  "read lines of text from a stream (up to a limit) and append them together"
  (let ((result ""))
      (loop for line = (read-line stream nil)
            for x from 1
        while (and (< x limit) line)
        do
               (setf result (cond ((= x 1) line) (t (format nil "~a~%~a" result line)))))
    result))


(defun cancel-fine-tune (ftid)
  "Cancel a running or pending fine-tune"
  (call-openai (format nil "fine-tunes/~a/cancel" ftid) :method :post))


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
    (st-json::write-json jso s)
    (get-output-stream-string s)))

(defun extract-arguments (message)
;;;  (format t "--- message=~s~%" message)
;;;  (format t "--- function_call=~S~%" (jso-val message "function_call"))
;;;  (format t "--- arguments=~S~%" (jso-val (jso-val message "function_call") "arguments"))
;;;  (format t "content=~S~%" (jso-val message "content"))
  (let* ((function-call (or (jso-val message "function_call") (jso)))
         (name (jso-val function-call "name"))
         (arguments (or (jso-val function-call "arguments") "{}")))
;;;    (format t "--- name=~S~%" name)
;;;    (format t "--- arguments=~S~%" arguments)
    (values arguments name)))

(defun ask-for-list (text &key (model *default-ask-chat-model*) (verbose nil))
  (let ((function-call (jso))
        (function (jso))
        (parameters (jso))
        (properties (jso))
        (array (jso))
        (items (jso))
        )
    (pushjso "name" "array_of_strings" function-call)
    (pushjso "type" "string" items)
    (pushjso "items" items array)
    (pushjso "type" "array" array)
    (pushjso "description" "the list of items" array)
    (pushjso "array" array properties)
    (pushjso "properties" properties parameters)
    (pushjso "type" "object" parameters)
    (pushjso "parameters" parameters function)
    (pushjso "description" "function to list an array of specified items"  function)
    (pushjso "name" "array_of_strings" function)
    (multiple-value-bind (arguments name)
        (ask-chat text :model model :functions (list function) :verbose verbose :function-call function-call)
;;;      (format t "arguments=~S name=~S~%" arguments name)
      (handler-case
      (cond ((null name) (list arguments))
            (t
             (let* (
                    (jso (read-json arguments)) ;;; arguments is JSON text inside a JSON object
                    (response-list (jso-val jso "array")))
;;;        (format t "arguments=~S~%" arguments)
;;;        (format t "name=~S~%" name)
               response-list)))
        (error (e) (format t "~a~%" e) nil)))))

#+ignore(progn
(mapcar (lambda (film)
          (format t "~a~%" film)
          (list film
                (ask-chat (format nil "What year was ~a released?  State the year only."film))
                (ask-chat (format nil "Name the director of ~a.  State the name only."film))
                (ask-for-list (format nil "List the main performers in ~a."film))
                (ask-for-list (format nil "List the major awards won by ~a."film) )
                (ask-chat (format nil "Write a 50 word summary of the plot of ~a."film))))
        (subseq (ask-for-list "List the top 10 highest rated classic films.") 0 9))


(mapcar (lambda (scientist)
          (format t "~a~%" scientist)
          (list scientist
                (ask-for-list (format nil "List the birth date and death date of ~a.  State the dates in YYYY-MM-DD form." scientist))
                (ask-for-list (format nil "What are the main discoveries or inventions attributed to ~a?" scientist))))
        (ask-for-list "Name the top 25 most important scientists in history."))


(mapcar (lambda (state)
          (format t "~a~%" state)
          (list state
                (ask-chat (format nil "What is the population of ~a?  Answer with a pure integer verbatim with no commas or punctuation." state))
                (ask-chat (format nil "What is the area of ~a in square miles?  Answer with a pure integer verbatim with no commas or punctuation." state))
                (ask-chat (format nil "What is the capital of the state of ~a?  Answer with the city name only." state))
                ))
        (ask-for-list "List 10 US states"))
)
