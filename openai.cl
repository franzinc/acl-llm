;; See the file "LICENSE" for the full license governing this code.;;
(in-package :gpt)

;; call set-openapi-key before calling any of these functions
(defvar *openai-api-key* "missing")
(defvar *openai-default-ask-chat-model* "gpt-3.5-turbo")
(defvar *openai-default-chat-model* "text-davinci-003")
(defvar *openai-default-max-tokens* 2048)

(defvar *openai-default-fine-tune-model*  "davinci")
(defvar *openai-default-initial-delay* 0.25)
(defvar *openai-default-retries* 4)
(defvar *openai-default-n* 1)
(defvar *openai-default-stop* "")

(defvar *openai-default-best-of* nil)
(defvar *openai-default-echo* nil)
(defvar *openai-default-frequency-penalty* 0.0)
(defvar *openai-default-functions* nil)
(defvar *openai-default-function-call* nil)
(defvar *openai-default-logit-bias* nil)
(defvar *openai-default-logprobs* nil)
(defvar *openai-default-output-format* :text)
(defvar *openai-default-presence-penalty* 0.0)
(defvar *openai-default-stream* :false)
(defvar *openai-default-suffix* nil)
(defvar *openai-default-temperature* 0.8)
(defvar *openai-default-timeout* 120)
(defvar *openai-default-top-p* 0.95)
(defvar *openai-default-user* "anonymous")
(defvar *openai-default-min-score* 0.0)
(defvar *openai-default-top-n* 10)
(defvar *openai-api-url* "https://api.openai.com/v1")

(defconstant *ignore-chars*
    (make-array 2 :element-type 'character
                :initial-contents '(#\newline #\space)))

(defun set-openai-api-key (key)
  (setf *openai-api-key*
        (string-trim *ignore-chars* key)))

(defun call-openai (cmd &key
                          (method :get)
                          (content nil)
                          (timeout *openai-default-timeout*)
                          (content-type "application/json")
                          (extra-headers nil)
                          (query nil)
                          (retries *openai-default-retries*)
                          (delay *openai-default-initial-delay*)
                          (verbose nil))
  "Generic interface to all openai v1 API functions using do-http-request."
  (handler-case
      (let ((uri (format nil "~a/~a" *openai-api-url* cmd)))
        (when verbose (log-llm "content=~S~%" content))
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
            (log-llm "headers=~S~%" headers)
            (log-llm "body=~S~%" body)
            (log-llm "code=~a~%" code))
;;; See https://platform.openai.com/docs/guides/error-codes/api-errors
;;; 429 API rate limit exceeded, retry with exponential backoff
;;; 503 - The engine is currently overloaded, please try again later
;;; 401 - various errors, JSON contains [error][message] path.         
;;; 500 - I've seen this happen so we'll try again after this
          (cond ((and (member code '(429 503 500)) (> retries 0)) 
                 (log-llm "Error code returned from openAi: ~s, ~d retries left~%" code retries)
                 (sleep delay)
                 (call-openai cmd :method method :content content :timeout timeout :content-type content-type
                                  :extra-headers extra-headers :query query :retries (1- retries) :delay (* 2 delay) :verbose verbose))
                ((or (= code 401) (and (>= code 200) (< code 300))) (read-json body))
                (t (pushjso "error" (pushjso "message" (format nil "API call to ~a returned HTTP ~a." uri code) (jso)) (jso))))))
    (error (e) (pushjso "error" (pushjso "message" (princ-to-string e) (jso)) (jso)))))



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
    (pushjso "model" *openai-default-fine-tune-model* jso)
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
               (log-llm "id=~a~%status=~a~%created:~S~%updated:~S~%" id status
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
        (log-llm "~a ~a~%" message (date-string created-at))))
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
    (log-llm "query=~S~%" query)
    (call-openai "files" :method :post :query query)
            )

  (let ((cmd
          (format nil
                  "curl https://api.openai.com/v1/files  -H \"Authorization: Bearer ~a\" -F purpose=\"fine-tune\" -F   file='@~a'"
                  *openai-api-key*
                  filename)))
;;;    (log-llm "~a~%" cmd)
    (multiple-value-bind (shell-stream error-stream process)
        (excl:run-shell-command cmd
                                :output :stream :error-output nil :wait nil)
      (declare (ignore error-stream))
      (let ((txt (read-lines-from-stream shell-stream)))

        (sys:reap-os-subprocess :pid process :wait nil)
        txt)

      )))



(defun cancel-fine-tune (ftid)
  "Cancel a running or pending fine-tune"
  (call-openai (format nil "fine-tunes/~a/cancel" ftid) :method :post))



(defun extract-json-text-and-function-name (message)
;;;  (log-llm "--- message=~s~%" message)
;;;  (log-llm "--- function_call=~S~%" (jso-val message "function_call"))
;;;  (log-llm "--- arguments=~S~%" (jso-val (jso-val message "function_call") "arguments"))
;;;  (log-llm "content=~S~%" (jso-val message "content"))
  (let* ((function-call (or (jso-val message "function_call") (jso)))
         (function-name (jso-val function-call "name"))
         (json-text (or (jso-val function-call "arguments") "{}")))
;;;    (log-llm "--- name=~S~%" name)
;;;    (log-llm "--- arguments=~S~%" arguments)
    (values json-text function-name)))

(eval-when (compile load eval)
  (setq key-args-list
    '(
      (best-of *openai-default-best-of*)
      (echo *openai-default-echo*)
      (frequency-penalty *openai-default-frequency-penalty*)
      (functions *openai-default-functions*)
      (function-call *openai-default-function-call*)
      (logit-bias *openai-default-logit-bias*)
      (logprobs *openai-default-logprobs*)
      (max-tokens *openai-default-max-tokens*)
      (model *openai-default-ask-chat-model*)
      (n *openai-default-n*)
      (output-format *openai-default-output-format*)
      (presence-penalty *openai-default-presence-penalty*)
      (stop *openai-default-stop*)
      (stream *openai-default-stream*)
      (suffix *openai-default-suffix*)
      (temperature *openai-default-temperature*)
      (timeout *openai-default-timeout*)
      (top-p *openai-default-top-p*)
      (user  *openai-default-user*)
      (verbose nil)
      (top-n *openai-default-top-n*)
      (min-score *openai-default-min-score*)
      (vector-database-name #+acl-llm-build llm::*default-vector-database-name*))
      
    key-args-signature
    '(
      :best-of best-of
      :echo echo
      :frequency-penalty frequency-penalty
      :functions functions
      :function-call function-call
      :logit-bias logit-bias
      :logprobs logprobs
      :max-tokens max-tokens
      :model model
      :n n
      :presence-penalty presence-penalty
      :stop stop
      :stream stream
      :suffix suffix
      :temperature temperature
      :timeout timeout
      :top-p top-p
      :user user
      :verbose verbose
      :vector-database-name vector-database-name
      :top-n top-n
      :min-score min-score
      )

    key-args-pushjso
    '(
      (when best-of (pushjso "best_of" best-of jso))
      (when echo (pushjso "echo" best-of jso))
      (pushjso "frequency_penalty" frequency-penalty jso)
      (when functions (pushjso "functions" functions jso))
      (when function-call (pushjso "function_call" function-call jso))
      (when logit-bias (pushjso "logit_bias" logit-bias jso))
      (when logprobs (pushjso "logprobs" logprobs jso)) ;;; small bug uncovered while documentating
      (pushjso "max_tokens" max-tokens jso)
      (pushjso "model" model jso)
      (pushjso "n" n jso)
      (pushjso "presence_penalty" presence-penalty jso)
      (pushjso "stop" stop jso)
      (pushjso "stream" stream jso)
      (when suffix (pushjso "suffix" suffix jso))
      (pushjso "temperature" temperature jso)
      (pushjso "top_p" top-p jso)
      (pushjso "user" user jso))))




(key-args-fun ask-chat
"Use this function for ChatGPT API.
 Model should be one of: gpt3-3.5-turbo, gpt3-3.5-turbo-0301 or gpt-4.
 text-or-alist can be either a simple string or a transcript in the form of an alist
 (role . content) ...) where role is one of  \"user\", \"system\", \"assistant\" or \"function\"."
              `(handler-case
                   (let* ((jso (st-json::jso))
                       (message-array nil))
                  min-score  ;; unused var
                  top-n      ;; unused var
                  vector-database-name ;; unused var
                  (when (stringp prompt-or-messages)
                    (setf prompt-or-messages `(("user" . ,prompt-or-messages))))
                  (loop  for (role . content) in (reverse prompt-or-messages)
                         for n from 0 do
                           (let ((message-jso (jso)))
                             (pushjso "role" role message-jso)
                             (pushjso "content" content message-jso)
                             (push  message-jso message-array)))
                  (pushjso "messages" message-array jso)
                  ,@key-args-pushjso

;;;    (log-llm "~S~%" jso)
                  (let* ((resp (call-openai "chat/completions" :timeout timeout :method :post :content (json-string jso) :verbose verbose))
                         (choices (when resp (jso-val resp "choices")))
                         (err (when resp (jso-val resp "error")))
                         (error-message (when err (jso-val err "message")))
                         (messages (when choices (mapcar (lambda (choice) (jso-val choice "message")) choices)))
                         (contents (when messages (mapcar (lambda (message) (string-trim (format nil " ~%")
                                                                                         (jso-val message "content"))) messages)))
                         )
                    (setf resp
                          (cond (error-message (handle-llm-error "ask-chat" error-message (list error-message)))
                                (contents contents)
                                (t '("No text"))))
                    (cond ((and functions (string-equal "null" (car contents)))
                           (extract-json-text-and-function-name (car messages)))
                          ((equal output-format :text) (car resp))
                          (t resp))))
                 (error (e) (handle-llm-error "ask-chat" (princ-to-string e) (list (princ-to-string e))))))



(key-args-fun chat
              "Use this function for GPT-3 models ada, babbage, davinci.
 Simple chatbot function: say (chat \"Hello.\")"
              `(let* ((jso (jso))
                      (resp nil)
                      (choices nil)
                      (responses nil))
                 vector-database-name ;; unused
                 min-score ;; unused
                 top-n ;; unused
                 (setf model *openai-default-chat-model*)
                 (pushjso "prompt" prompt-or-messages jso)
                 ,@key-args-pushjso
;;;    (log-llm "jso=~a~%" (json-string jso))
                 (setf resp (call-openai "completions" :method :post :content (json-string jso)
                                                       :timeout timeout
                                                       :verbose verbose
                                                       ))
                 (setf choices (jso-val resp "choices"))
                 (setf responses (or (mapcar
                                      (lambda (u) (string-trim (format nil " ~%") (jso-val u "text"))) choices) '("No text")))
                 (when verbose (log-llm "jso=~S~%" resp))
                 (cond ((equal output-format :text) (car responses))
                       (t responses))))








(key-args-fun ask-for-list ""
              `(progn
                 output-format ;;unused
                 (setf functions (read-json "[
{'name':'array_of_strings',
'description':'function to list an array of specified items',
'parameters':
  {'type':'object',
   'properties':
    {'array':
      {'description':'the list of items',
       'type':
       'array',
       'items':
         {'type':'string'}
}}}}]"))
                 (setf function-call (read-json "{'name':'array_of_strings'}"))
                 (handler-case                 
                     (multiple-value-bind (json-text function-name)
                         (ask-chat prompt-or-messages ,@key-args-signature)
                       (cond ((null function-name) (handle-llm-error "ask-for-list" json-text (list json-text))) ;;; this should not happen
                             (t
                              (let* (
                                     (jso (read-json json-text)) ;;; json-text is JSON text inside a JSON object
                                     (response-list (jso-val jso "array")))
                                response-list))))
                   (error (e) (handle-llm-error "ask-for-list" (princ-to-string e) (list (princ-to-string e)))))))


(key-args-fun ask-for-map ""
              `(progn
                 output-format ;; unused
                 (setf function-call (read-json "{'name':'array_of_key_val'}"))
                 (setf functions (read-json "[
{'name':'array_of_key_val',
'description':'function to list an array of key-value pairs.',
'parameters':
  {'type':'object',
   'properties':
    {'array':
      {'description':'the list of key-value pairs',
       'type':
       'array',
       'items':
        {
        'type': 'object',
        'properties': {
          'key': {
            'type': 'string',
            'description': 'Unique identifier of the object.'
          },
          'value': {
            'type': 'string',
            'description': 'Value associated with the object.'
          }
        }
}}}}}]"))
                 (handler-case                 
                     (multiple-value-bind (json-text function-name)
                         (gpt::ask-chat prompt-or-messages ,@key-args-signature)
                       (when verbose (log-llm "ask-for-map: json-text=~a name=~a~%" json-text function-name))
                       (cond ((null function-name) (list (list json-text nil)))
                             (t
                              (let* (
                                     (jso (read-json json-text)) ;;; json-text is JSON text inside a JSON object
                                     (response-list (jso-val jso "array")))
                                (mapcar (lambda (u) (list (jso-val u "key") (jso-val u "value") )) response-list)
                                ))))
                   (error (e) (list (list (princ-to-string e) nil))))))




(defun format-ask-my-documents-prompt (query id-content)
  (let* ((formatted-content (mapcar (lambda (u) (format nil "citation-id:~a content:'~a'" (car u) (cadr u))) id-content))
         (prompt (format nil "Here is a list of citation IDs and content related to the query '~a':~%
~{~a~%~}.
Respond to the query '~a' as though you wrote the content.  Be brief.  You only have 20 seconds to reply.
Place your response to the query in the 'response' field.
Insert the list of citations whose content informed the response into the 'citation_ids' array.
" query formatted-content query)))
  (setf prompt (remove-if (lambda (ch) (> (char-code ch) 127)) prompt))
  prompt))



#+acl-llm-build
(key-args-fun ask-my-documents

              "The purpose of this function is to search a local knowledge base of documents for content that matches the query,
then formulate a big prompt that combines this 'background info' with the original query, and return a response along with citations to the documents that
              contributed to the query (but not necessarily all the documents that matched).

ask-my-documents implements the concept known as Retrieval Augmented Generation (RAG).

This function creates a JSON object to tell OpenAI how we want its response structured.  Confusingly this feature is called 'function-calling' in the OpenAI documentation.  Basically it allows us to tell OpenAI that we want a JSON object of the form

{'response': 'the response to the query',
 'citation_ids':[uri, uri1, ...]
}
              The function format-ask-my-documents-prompt formats the 'big prompt' from the matching documents plus the original query.
              It's broken out as a separate function in case we want to customize it in the initfile.
"
              `(handler-case
                   (progn
                     (when verbose (log-llm "ask-my-documents dir=~a~%" llm::*default-vector-database-dir*))
                     (let* ((query prompt-or-messages)
                            (side-effect (when verbose (log-llm "database-name=~S dir=~S=~%" vector-database-name llm::*default-vector-database-dir*)))
                            (vector-database (llm::read-vector-database vector-database-name :dir llm::*default-vector-database-dir* ))
                            (side-effect (setf (llm::vector-database-embedder vector-database) 'gpt::embed))
                            (matches (nn vector-database query :top-n top-n :min-score min-score :verbose verbose))
                            (side-effect (when verbose (log-llm "database=~S matches=~S top-n=~a min-score=~a~%" vector-database matches top-n min-score)))
                            (score-table (make-hash-table :test 'string=))
                            (original-text-table (make-hash-table :test 'string=))
                            (id-content (mapcar (lambda (u)
                                                  (setf (gethash (car u) score-table) (cadr u))
                                                  (setf (gethash (car u) original-text-table) (caddr u))
                                                  (list (car u) (caddr u))) matches))
                            (prompt (format-ask-my-documents-prompt query id-content)))
                       (declare (ignore side-effect)
                                (ignore output-format)) ;; unused
                       (setf function-call (read-json "{'name':'response_citations'}"))
                       (setf functions (read-json "[
{'name':'response_citations',
'description':'function to provide a response and a list of IDs of any content contributing to the reponse.',
'parameters':
  {'type':'object',
   'properties':
   {
    'response': {
            'type': 'string',
            'description': 'The response to the query.'
          },

    'citation_ids':
      {
       'type':
       'array',
       'description':'the IDs of the content contributing to the response',
       'items':
        {
          'type': 'string',
          'description': 'an ID of content that contributed to the response'
}}}}}]"))

                       (let ((response
                               (cond ((string= (caar matches) "error")
                                      (handle-llm-error "ask-my-documents"
                                                        (nth 2 (car matches))
                                                        (list (list (nth 2 (car matches)) 0.0 "error" "error"))))
                                     (t
                                      (let* ((json-string-response
                                               (gpt::ask-chat prompt ,@key-args-signature))
                                             (json-response (read-json json-string-response))
                                             (text-response (jso-val json-response "response"))
                                             (citation-ids (jso-val json-response "citation_ids")))
                                        (mapcar (lambda (u) (list text-response (gethash u score-table) u (gethash u original-text-table)))
                                                citation-ids)
                                        )))))
;;;(log-llm "ask-my-documents response: ~a~%" response)
                         response)))
                 (error (e) (handle-llm-error "ask-my-documents"
                                              (princ-to-string e)
                                              (list (list (princ-to-string e) 0.0 "error" "error"))))))

#-acl-llm-build
(key-args-fun ask-my-documents

              "The purpose of this function is to search a local knowledge base of documents for content that matches the query,
then formulate a big prompt that combines this 'background info' with the original query, and return a response along with citations to the documents that
              contributed to the query (but not necessarily all the documents that matched).

ask-my-documents implements the concept known as Retrieval Augmented Generation (RAG).

This function creates a JSON object to tell OpenAI how we want its response structured.  Confusingly this feature is called 'function-calling' in the OpenAI documentation.  Basically it allows us to tell OpenAI that we want a JSON object of the form

{'response': 'the response to the query',
 'citation_ids':[uri, uri1, ...]
}
              The function format-ask-my-documents-prompt formats the 'big prompt' from the matching documents plus the original query.
              It's broken out as a separate function in case we want to customize it in the initfile.
"
              `(handler-case
                   (progn
                 
                     (let* ((query prompt-or-messages)
                            (matches)
                            (score-table (make-hash-table :test 'string=))
                            (original-text-table (make-hash-table :test 'string=))
                            (id-content)
                            (prompt)
                            (db)
                            (*db*))
                   
                       ;; unused vars we can't declare unused due to how this form is generated
                       output-format
                   
                       
                       (setq db (db.agraph:open-triple-store vector-database-name))

                       
                       (unwind-protect
                           (progn
                           
                             ;; the next step (ask-chat) will only work for openai at the moment
                             ;; so make sure the vdb is setup for openai and get
                             ;; the api key
                           
                           
                             (setq matches (db.agraph:vector-store-nearest-neighbor query  
                                                                                    :db db 
                                                                                    :min-score min-score 
                                                                                    :top-n top-n))
                           

                             
                             (setq id-content (mapcar #'(lambda (u)
                                                          (destructuring-bind (id score text pred type) u
                                                            (declare (ignore type pred))
                                                            (setf (gethash id score-table) score)
                                                            (setf (gethash id original-text-table) text)
                                                            (list id text)))
                                                      matches))
                           
                             (setq prompt (format-ask-my-documents-prompt query id-content))
                           
                           
                             (setf function-call (read-json "{'name':'response_citations'}"))
                             (setf functions (read-json "[
{'name':'response_citations',
'description':'function to provide a response and a list of IDs of any content contributing to the reponse.',
'parameters':
  {'type':'object',
   'properties':
   {
    'response': {
            'type': 'string',
            'description': 'The response to the query.'
          },

    'citation_ids':
      {
       'type':
       'array',
       'description':'the IDs of the content contributing to the response',
       'items':
        {
          'type': 'string',
          'description': 'an ID of content that contributed to the response'
}}}}}]"))
                           
                             (handler-case
                                 (let* ((json-string-response
                                         (gpt::ask-chat prompt ,@key-args-signature))
                                        (json-response (read-json json-string-response))
                                        (text-response (jso-val json-response "response"))
                                        (citation-ids (jso-val json-response "citation_ids")))
                                   
                                   #+ignore 
                                   (with-open-file (p "/usr/tmp/debugit" 
                                                    :direction :output
                                                    :if-exists :supersede)
                                     (format p "prompt: ~%~a~3%" prompt)
                                     (format p "json-response:~%~a~3%" json-response)
                                     (format p "text-response:~%~a~3%" text-response)
                                     (format p "citation-ids:~%~a~3%" citation-ids)
                                     )
                                   
                                   (mapcar #'(lambda (oid) 
                                               ;; openai strips the <>'s from the citations 
                                               (let ((u (format nil "<~a>" oid)))
                                                 (list text-response 
                                                       (gethash u score-table) 
                                                       (vector-store-object-property-value oid "id" :db db)
                                                       (gethash u original-text-table))))
                                           citation-ids))
                               (error (e) (handle-llm-error "ask-my-documents"
                                                            (princ-to-string e)
                                                            (list (list (princ-to-string e) 0.0 "error" "error"))))))
                       
                         ;; cleanup
                         (db.agraph:close-triple-store :db db)
                       
                         )))
                 
                 ;; handler-case:
                 (error (e) (handle-llm-error "ask-my-documents"
                                              (princ-to-string e)
                                              (list (list (princ-to-string e) 0.0 "error" "error"))))))



;;;(ask-for-table "Create a table of letters, the order of each letter, and indicate whether it is a vowel or not.")
;;;(ask-for-table "10 common English verbs and their infinitive, past, present, future, past perfect and gerund forms.")

(key-args-fun ask-for-table "ask-for-table uses function calling to return table of values as a list of lists, where each inner list represents one row of the table."
              `(progn
                 output-format ;; unused
                 (setf function-call (read-json "{'name':'table'}"))
                 (setf functions (read-json "[
{'name':'table',
'description':'function to return tabular data.',
'parameters':
  {'type':'object',
   'properties':
    {'rows':
      {'description':'the list of table rows',
       'type':
       'array',
       'items':
        {
        'type': 'array',
        'items': {
           'type': 'string',
           'description' : 'the value in each column'}
}}}}}]"))

                 (handler-case
                     (multiple-value-bind (json-text function-name)
                         (gpt::ask-chat prompt-or-messages ,@key-args-signature)
                       (when verbose (log-llm "ask-for-table: json-text=~a function-name=~a~%" json-text function-name))
                       (cond ((null function-name) (handle-llm-error json-text "ask-for-table" (list (list json-text))))
                             (t
                              (let* (
                                     (jso (read-json json-text)) ;;; json-text is JSON text inside a JSON object
                                     (response-list (jso-val jso "rows")))
                                response-list
                                ))))
                   (error (e) (handle-llm-error "ask-for-table" (princ-to-string e) (list (list (princ-to-string e))))))))
