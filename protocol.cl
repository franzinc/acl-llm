;;;; protocol.cl
;;;; See the file LICENSE for the full license governing this code.
(in-package #:cl-user)

(defpackage #:acl-llm.protocol
  (:use #:cl
        #:excl
        #:util.string
        #:net.aserve.client)
  (:nicknames #:llm.protocol)
  ;; LLM request
  (:export #:*llm-request-timeout*
           #:llm-request-sync)
  ;; Classes and accessors
  (:export #:llm-standard-vendor
           #:llm-standard-chat-vendor
           #:llm-standard-chat-vendor-default-chat-temperature
           #:llm-standard-chat-vendor-default-chat-max-tokens
           #:llm-standard-chat-vendor-default-chat-non-standard-params
           #:llm-standard-full-vendor
           #:llm-chat-prompt
           #:llm-chat-prompt-context
           #:llm-chat-prompt-examples
           #:llm-chat-prompt-exchanges
           #:llm-chat-prompt-functions
           #:llm-chat-prompt-temperature
           #:llm-chat-prompt-max-tokens
           #:llm-chat-prompt-non-standard-params
           #:llm-chat-prompt-exchange
           #:llm-chat-prompt-exchange-role
           #:llm-chat-prompt-exchange-content
           #:llm-chat-prompt-exchange-function-call-result
           #:llm-chat-prompt-function-call-result
           #:llm-chat-prompt-function-call-result-call-id
           #:llm-chat-prompt-function-call-result-function-name
           #:llm-chat-prompt-function-call-result-result
           #:llm-function-call
           #:llm-function-call-function
           #:llm-function-call-name
           #:llm-function-call-description
           #:llm-function-call-args
           #:llm-function-arg
           #:llm-function-arg-name
           #:llm-function-arg-description
           #:llm-function-arg-type
           #:llm-function-arg-required)
  ;; Generic functions
  (:export #:llm-capabilities
           #:llm-chat-token-limit
           #:llm-cancel-request
           #:llm-vendor-request-prelude
           #:llm-vendor-headers
           #:make-llm-chat-prompt
           #:llm-chat-prompt-append-response
           #:llm-chat
           #:llm-chat-async
           #:llm-chat-streaming
           #:llm-vendor-chat-url
           #:llm-vendor-chat-streaming-url
           #:llm-vendor-chat-timeout
           #:llm-vendor-chat-request
           #:llm-vendor-chat-extract-error
           #:llm-vendor-chat-extract-result
           #:llm-vendor-append-to-prompt
           #:llm-vendor-streaming-media-handler
           #:llm-vendor-extract-function-calls
           #:llm-vendor-populate-function-calls
           #:llm-vendor-collect-streaming-function-data
           #:llm-embedding
           #:llm-embedding-async
           #:llm-embedding-url
           #:llm-vendor-embedding-request
           #:llm-vendor-embedding-extract-error
           #:llm-vendor-embedding-extract-result)
  ;; Utilities
  (:export
   #:llm-vendor-utils-get-system-prompt
   #:llm-vendor-utils-combine-to-system-prompt
   #:llm-vendor-utils-combine-to-user-prompt
   #:llm-vendor-utils-collapse-history
   #:llm-vendor-utils-model-token-limit
   #:llm-vendor-utils-append-to-prompt
   #:llm-vendor-utils-function-call
   #:llm-vendor-utils-function-call-id
   #:llm-vendor-utils-function-call-name
   #:llm-vendor-utils-function-call-args
   #:llm-vendor-utils-process-result
   #:llm-vendor-utils-populate-function-results
   #:llm-vendor-utils-execute-function-calls))

(in-package #:acl-llm.protocol)

;;; LLM request
(defvar *llm-request-timeout* nil
  "The number of seconds to wait for a response from a HTTP server.

When set to nil, don't timeout while receiving a response. Request timings are
depending on the request. Requests that need more output may take more time, and
there is other processing besides just token generation that can take a while.
Sometimes the LLM can get stuck, and you don't want it to take too long. This
should be balanced to be good enough for hard requests but not very long so that
we can end stuck requests.")

(defun llm-request-success-p (status)
  "Return non-nil if `status' is a successful HTTP status code."
  (<= 200 status 299))

(defun llm-reqeust-sync (url &key headers content timeout)
  "Make a request to URL. The raw text response will be parsed to a `st-json:jso'
and then returned.

`headers' will be added in the Authorization header, in addition to standard
json header. This is optional.

`content' is the string of `st-json:jso' and sent as the request body. This is
required.

`timeout' is the number of seconds to wait for a response."
  (multiple-value-bind (in status)
      (do-http-request url
        :method :post
        :content content
        :headers (append headers '(("Content-Type" . "application/json")))
        :external-format :utf-8
        :timeout (or timeout *llm-request-timeout*)
        :return :stream)
    (when (not (llm-request-success-p status))
      (error "LLM request of ~s returns non-successful status code: ~d" url status))
    (unwind-protect (st-json:read-json in)
      (close in))))

;;; Classes
(defclass llm-standard-vendor ()
  ()
  (:documentation "An abstract class indicating that this is a standard LLM vendor.
This represents any vendor, regardless of what it implements."))

(defclass llm-standard-chat-vendor (llm-standard-vendor)
  ((default-chat-temperature :initform nil :initarg :default-chat-temperature :accessor llm-standard-chat-vendor-default-chat-temperature)
   (default-chat-max-tokens :initform nil :initarg :default-chat-max-tokens :accessor llm-standard-chat-vendor-default-chat-max-tokens)
   (default-chat-non-standard-params :initform nil :initarg :default-chat-non-standard-params :accessor llm-standard-chat-vendor-default-chat-non-standard-params))
  (:documentation "A struct for indicating a vendor that implements chat.

`default-chat-temperature' is the default temperature for chats with the vendor.
Any `temperature' specified in the chat prompt will override this. This is
optional, and if not set, when not overridden, the default value chosen by the
vendor will be used.

`default-chat-max-tokens' is the default maxmimum number of tokens for chats
with the vendor. Any value for `max-tokens' specified in the chat prompt will
override this. This is optional, and if not set, when not overriden, no maximum
will be specified to the vendor.

`default-chat-non-standard-params' are per-vendor params that will override and
`non-standard-params' that are part of the prompt. This is an alist of
parameters, whose name and possible values will be different for each vendor.
The overriding here is on a per-parameter basis, so the final value used in the
chat can be a mix of these default parameters and others in the prompt.

These values will be set as parameters on the prompt, so changing values after
the initial call in the chat will not have an effect. New values will have an
effect, however."))

(defclass llm-standard-full-vendor (llm-standard-chat-vendor)
  ()
  (:documentation "A class for LLM vendors that implements both chat and embeddings."))

(defclass llm-chat-prompt ()
  ((context :initarg :context :accessor llm-chat-prompt-context)
   (examples :initarg :examples :accessor llm-chat-prompt-examples)
   (exchanges :initarg :exchanges :accessor llm-chat-prompt-exchanges)
   (functions :initarg :functions :accessor llm-chat-prompt-functions)
   (temperature :initarg :temperature :accessor llm-chat-prompt-temperature)
   (max-tokens :initarg :max-tokens :accessor llm-chat-prompt-max-tokens)
   (non-standard-params :initarg :non-standard-params :accessor llm-chat-prompt-non-standard-params))
  (:documentation "This stores all the information needed for a structured chat prompt."))

(defclass llm-chat-prompt-exchange ()
  ((role :initarg :role :type (member :user :asistant :function) :accessor llm-chat-prompt-exchange-role)
   (content :initarg :content :accessor llm-chat-prompt-exchange-content)
   (function-call-result :initarg :function-call-result :accessor llm-chat-prompt-exchange-function-call-result))
  (:documentation "This defines a single exchange given as part of a chat prompt.

`role' can be a keyword, of either `user', `assistant', or `function'.

`function-call-results' is a struct of type
`llm-chat-prompt-function-call-results', which is only populated if `role' is
`function'. It stores the results of just one function call."))

(defclass llm-chat-prompt-function-call-result ()
  ((call-id :initarg :call-id :accessor llm-chat-prompt-function-call-result-call-id)
   (function-name :initarg :function-name :accessor llm-chat-prompt-function-call-result-function-name)
   (result :initarg :result :accessor llm-chat-prompt-function-call-result-result))
  (:documentation "This defines the result from a function call.

`call-id' is an ID for this function call, if available.

`function-name' is the name of the function. This is required.

`result' is the result of the function call. This is required."))

(defclass llm-function-call ()
  ((function :initarg :function :accessor llm-function-call-function)
   (name :initarg :name :accessor llm-function-call-name)
   (description :initarg :description :accessor llm-function-call-description)
   (args :initarg :args :accessor llm-function-call-args))
  (:documentation "This is a class to represent a function call the LLM can make.

`function' is a function to call.

`name' is a human readable name of the function.

`description' is a human readable description of the function.

`args' is a list of `llm-function-arg' instances."))

(defclass llm-function-arg ()
  ((name :initarg :name :accessor llm-function-arg-name)
   (description :initarg :description :accessor llm-function-arg-description)
   (type :initarg :type :accessor llm-function-arg-type)
   (required :initarg :required :accessor llm-function-arg-required))
  (:documentation "An argument to an `llm-function-call'.

`name' is the name of the argument.

`description' is a human readable description of the argument. It can be nil for
enums.

`type' is the type of the argument. It can be one of `string', `integer',
`float', `boolean' or a more complicated type specifier, e.g. `(or <type1>
<type2> ... <typen>)'.

`required' is whether this is required or not."))

;;; Generic functions
(defgeneric llm-capabilities (vendor)
  (:documentation "Return a list of the capabilities of `vendor'.

This possible values are only those things that are not the bare minimum of
functionality to be included in this package, which is non-streaming chat:

`streaming': the LLM can actually stream responses in the streaming call. Calls
to `llm-chat-streaming' will work regardless even if the LLM doesn't support
streaming, it just won't have any partial responses, so basically just operates
like `llm-chat-async'.

`embeddings': the LLM can return vector embeddings of text.

`function-calls': the LLM can call functions.")
  (:method (vendor)
    (declare (ignore vendor))
    nil))

(defgeneric llm-chat-token-limit (vendor)
  (:documentation "Return max number of tokens that can be sent to the LLM.
For many models we know this number, but for some we don't have enough
information to know. In those cases we return a default value that should be a
reasonable lower bound.

`vendor': the vendor struct that would be used for a LLM call.")
  (:method (vendor)
    (declare (ignore vendor))
    2048))

(defgeneric llm-cancel-request (request)
  (:documentation "Cancel `request', stopping any further communication.
`request' is the same object return by the async or streaming methods.")
  (:method (request)
    (declare (ignore request))
    (warn "Canceling a request is not supported for this LLM.")))

(defgeneric llm-vendor-request-prelude (vendor)
  (:documentation "Execute any prelude code necessary before running a request.
`vendor' is the vendor that will be used to make the request.")
  (:method ((vendor llm-standard-vendor))
    "Do nothing for the standard vendor."
    nil))

(defgeneric llm-vendor-headers (vendor)
  (:documentation "Return the headers (an alist) for the `vendor'.")
  (:method ((vendor llm-standard-vendor))
    "By default, the standard vendor has no headers."
    nil))

(defun make-llm-chat-prompt (text &key context examples functions
                                    temperature max-tokens
                                    non-standard-params)
  "Create a `llm-chat-prompt' with `text' sent to the LLM vendor.

This is the most correct and easy way to create an `llm-chat-prompt', and should
suffice for almost all uses.

Note that this should be called just once per interactive session with an LLM,
and the prompt re-used for all subsequent exchanges. The reason for this is
that some LLMs may store data about previous exchanges in opaque ways, so
they can only be populated once. Therefore, if `previous-exchanges' is
populated, a best effort is made to do something reasonable, but it may not be
quite the same on all vendors as the prompt mutating in terms of an actual
conversation.

`text' is the latest user input to the LLM, the thing to be responded to. This
is required. This can also be a string, in which case it represents the chat
history, starting with the user's initial chat, followed by the response, and so
on. If it is a list, it MUST be an odd number, since the presumption is that it
ends with the user's latest input to the LLM.

`context' is a string given to the LLM as context for the entire exchange,
such as instructions to the LLM on how to reply, persona, information on the
user, or anything else that applies to the chat as a whole. This is optional.

`examples' is a list of conses, where the car is an example inputs, and cdr is
the corresponding example outputs. This is optional.

`functions' is a list of `llm-function-call' structs. These may be called IF the
LLM supports them. If the LLM does not support them, a `not-implemented' signal
will be thrown. This is optional. When this is given, the LLM will either call
the function or return text as normal, depending on what the LLM decides.

`temperature' is a floating point number with a minimum of 0, and maximum of 1,
which controls how predictable the result is, with 0 being the most
predicatable, and 1 being the most creative. This is not required.

`max-tokens' is the maximum number of tokens to generate. This is optional.

`context', `examples', `functions', `temperature', and `max-tokens' are usually
turned into part of the exchange, and if so, they will be put in the first
exchange of the prompt (before anything in `previous-exchanges').

`non-standard-params' is an alist of other options that the vendor may or may
not know how to handle. These are expected to be vendor specific. Don't use this
if you want the prompt to be used amongst different vendors, because it is
likely to cause a request error. The cars of the alist are strings and the cdrs
can be strings or numbers. This is optional."
  (when (null text)
    (error "TEXT must NOT be nil"))
  (when (and (listp text)
             (zerop (mod (length text) 2)))
    (error "TEXT, when given as a list, must have an odd number of elements"))
  (make-instance 'llm-chat-prompt
                 :context context
                 :examples examples
                 :exchanges (loop for i from 0
                                     for s in (if (listp text) text (list text))
                                     collect (make-instance 'llm-chat-prompt-exchange
                                                            :role (if (zerop (mod i 2)) 'user 'assistant)
                                                            :content s))
                 :functions functions
                 :temperature temperature
                 :max-tokens max-tokens
                 :non-standard-params non-standard-params))

(defun llm-chat-prompt-append-response (prompt response &optional role)
  "Append a new `response' to `prompt', to continue a conversation.
`role' default to `user', which should almost always be what is needed."
  (setf (llm-chat-prompt-exchanges prompt)
        (append (llm-chat-prompt-exchanges prompt)
                (list (make-instance 'llm-chat-prompt-exchange :role (or role 'user)
                                                                  :content response)))))

(defgeneric llm-chat (vendor prompt)
  (:documentation "Return a response to PROMPT from VENDOR.
PROMPT is a `llm-chat-prompt'.

The response is a string response by the LLM when functions are not called. If
functions are called, the response is a list of conses of the function named
called (as a symbol), and the corresponding result from calling it.

The prompt's exchanges list will be updated to encode the conversation so
far.")
  (:method ((vendor (eql nil)) prompt)
    (declare (ignore prompt))
    (error "LLM vendor was nil. Please set the vendor in the application you are using."))
  (:method ((vendor llm-standard-chat-vendor) prompt)
    (llm-vendor-request-prelude vendor)
    (let* ((response (llm-reqeust-sync
                      (llm-vendor-chat-url vendor)
                      :headers (llm-vendor-headers vendor)
                      :content (llm-vendor-chat-request vendor prompt nil)
                      :timeout (llm-vendor-chat-timeout vendor)))
           (err (llm-vendor-chat-extract-error vendor response)))
      (if* err
         then (error err)
         else (llm-vendor-utils-process-result vendor
                                               prompt
                                               (llm-vendor-chat-extract-result vendor response)
                                               (llm-vendor-extract-function-calls vendor response))))))

(defgeneric llm-chat-async (vendor prompt response-callback error-callback)
  (:documentation "Call `response-callback' with a response to `prompt' from `vendor'.

The response is a string response by the LLM when functions are not called. If
functions are called, the response is a list of conses of the function named
called (as a symbol), and the corresponding result from calling it.

`prompt' is a `llm-chat-prompt'.

`response-callback' receives the final text.

`error-callback' receives the error response.

The prompt's exchanges list will be updated to encode the conversation so
far.

This returns an object representing the async request, which can
be passed to `llm-cancel-request'.")
  (:method (vendor prompt response-callback error-callback)
    ;; By default, we can turn a streaming call into an async call, so we can
    ;; fall back to streaming if async is not populated.
    (flet ((response-cb (text)
             (funcall response-callback text))
           (error-cb (err msg)
             (funcall error-callback err msg)))
      (llm-chat-streaming vendor prompt nil #'response-cb #'error-cb))))

(defgeneric llm-chat-streaming (vendor prompt partial-callback response-callback error-callback)
  (:documentation "Stream a response to `prompt' from vendor.
`prompt' is a `llm-chat-prompt'.

The response is a string response by the LLM when functions are not called. If
functions are called, the response is a list of conses of the function named
called (as a symbol), and the corresponding result from calling it.

`partial-callback' is called with the output of the string response as it is
built up. The callback is called with the entire response that has been
received, as it is streamed back. It is not guaranteed to be called with the
complete response before `response-callback' is called. This can be nil, so that
implementations can just define this method which can be called by
`llm-chat-async', but with a nil value here to never get partial callbacks.

`response-callback' receives the each piece of the string response. It is called
once after the response has been completed, with the final text.

`error-callback' receives the error response.

The prompt's exchanges list will be updated to encode the conversation so
far.

This returns an object representing the async request, which can
be passed to `llm-cancel-request'."))

(defgeneric llm-vendor-chat-url (vendor)
  (:documentation "Return the URL for chat for the `vendor'."))

(defgeneric llm-vendor-chat-streaming-url (vendor)
  (:documentation "Return the URL for streaming chat for the `vendor'.")
  (:method ((vendor llm-standard-chat-vendor))
    "By default, use the same URL as normal chat.

`vendor' is the standard chat vendor that is used to make the request."
    (llm-vendor-chat-url vendor)))

(defgeneric llm-vendor-chat-timeout (vendor)
  (:documentation "Return the seconds of timeout for `vendor'.
Return nil for the standard timeout.")
  (:method ((vendor llm-standard-vendor))
    "By default, the standard vendor has the standard timeout."
    nil))

(defgeneric llm-vendor-chat-request (vendor prompt streaming)
  (:documentation "Return the request for the `vendor' for `prompt'.
`streaming' is true if this is a streaming request.")
  (:method :before ((vendor llm-standard-chat-vendor) prompt streaming)
    "Set `vendor' default parameters where they do not existe in the `prompt'."
    (declare (ignore streaming))
    (setf (llm-chat-prompt-temperature prompt)
          (or (llm-chat-prompt-temperature prompt)
              (llm-standard-chat-vendor-default-chat-temperature vendor))
          (llm-chat-prompt-max-tokens prompt)
          (or (llm-chat-prompt-max-tokens prompt)
              (llm-standard-chat-vendor-default-chat-max-tokens vendor))
          (llm-chat-prompt-non-standard-params prompt)
          ;; We need to merge the parameteres individually.
          (union (llm-chat-prompt-non-standard-params prompt)
                 (llm-standard-chat-vendor-default-chat-non-standard-params vendor)
                 :test (lambda (a b)
                         (equal (car a) (car b))))))
  (:method :around ((vendor llm-standard-chat-vendor) prompt streaming)
    (declare (ignore prompt streaming))
    (let ((jso (call-next-method)))
      (st-json:write-json-to-string jso))))

(defgeneric llm-vendor-chat-extract-error (vendor response)
  (:documentation "Return an error message from `response' for the `vendor'.")
  (:method ((vendor llm-standard-chat-vendor) response)
    "By default, the standard vendor has no error extractor."
    (declare (ignore response))
    nil))

(defgeneric llm-vendor-chat-extract-result (vendor response)
  (:documentation "Return the result from `response' for the `vendor'."))

(defgeneric llm-vendor-append-to-prompt (vendor prompt result &optional function-results)
  (:documentation "Append `result' to `prompt' for the `vendor'.

`prompt' is the prompt that was already sent to the vendor.

`function-results' is a list of function results, if any.")
  (:method ((vendor llm-standard-chat-vendor) prompt result &optional function-results)
    "By default, we just append to the prompt."
    (llm-vendor-utils-append-to-prompt prompt result function-results)))

(defgeneric llm-vendor-streaming-media-handler (vendor msg-receiver fc-receiver err-receiver)
  (:documentation "Define how to handle streaming media for the `vendor'.

This should return a cons of the media type and an instance that handle objects
of that type.

The handlers defined can call `msg-receiver' when they receive part of a text
message for the client (a chat response). If they receive a function call, they
should call `fc-receiver' with the function call. If they receive an error, they
should call `err-receiver' with the error message.")
  (:method ((vendor llm-standard-chat-vendor) msg-receiver fc-receiver err-receiver)
    "By default, the standard vendor has no streaming media handler."
    (declare (ignore msg-receiver fc-receiver err-receiver))
    nil))

(defgeneric llm-vendor-extract-function-calls (vendor response)
  (:documentation "Return the function call results from `response' for the `vendor'.

If there are no function call results, return nil. If there are function call
results, return a list of `llm-vendor-utils-function-call'.")
  (:method ((vendor llm-standard-chat-vendor) response)
    "By default, the standard vendor has no function call extractor."
    (declare (ignore response))
    nil))

(defgeneric llm-vendor-populate-function-calls (vendor prompt calls)
  (:documentation "For `vendor', in `prompt', record function call execution.
This is the recording before the calls were executed. `calls' are a list of
`llm-vendor-utils-function-call'."))

(defgeneric llm-vendor-collect-streaming-function-data (vendor data)
  (:documentation "Transform a list of streaming function call `data' responses.

`vendor' is the instance specifying the LLM vendor and its configuration.

The `data' responses are a list of whatever is sent to the function call handler
in `llm-vendor-streaming-media-handler'. This should return a list of
`llm-chat-function-call' instances.")
  (:method ((vendor llm-standard-chat-vendor) data)
    "By default, there is no function calling."
    (declare (ignore data))
    nil))

(defgeneric llm-embedding (vendor text)
  (:documentation "Return a vector embedding of `text' from `vendor'.")
  (:method ((vendor llm-standard-full-vendor) text)
    (llm-vendor-request-prelude vendor)
    (let* ((response (llm-reqeust-sync
                      (llm-vendor-embedding-url vendor)
                      :headers (llm-vendor-headers vendor)
                      :content (llm-vendor-embedding-request vendor text)
                      :timeout (llm-vendor-chat-timeout vendor)))
           (err-msg (llm-vendor-embedding-extract-error vendor response)))
      (if* err-msg
         then (error err-msg)
         else (llm-vendor-embedding-extract-result vendor response)))))

(defgeneric llm-embedding-async (vendor text vector-callback error-callback)
  (:documentation "Calculate a vector embedding of `text' from `vendor'.

`vector-callback' will be called with the vector embedding.

`error-callback' will be called in the event of an error, with an error signal
and a string message.

This returns an object representing the async request, which can be passed to
`llm-cancel-request'.")
  (:method ((vendor (eql nil)) text vector-callback error-callback)
    (declare (ignore text vector-callback error-callback))
    (error "LLM vendor was nil. Please set the vendor in the application you are using")))

(defgeneric llm-vendor-embedding-url (vendor)
  (:documentation "Return the URL for embeddings for the `vendor'."))

(defgeneric llm-vendor-embedding-request (vendor text)
  (:documentation "Return the request for the `vendor' for `text'.")
  (:method :around (vendor text)
    (declare (ignore vendor text))
    (let ((jso (call-next-method)))
      (st-json:write-json-to-string jso))))

(defgeneric llm-vendor-embedding-extract-error (vendor response)
  (:documentation "Return an error message from `response' for the `vendor'.

`response' is a parsed JSON object.

Return nil if there is no error.")
  (:method ((vendor llm-standard-full-vendor) response)
    "By default, the standard vendor has no error extractor."
    (declare (ignore response))
    nil))

(defgeneric llm-vendor-embedding-extract-result (vendor response)
  (:documentation "Return the result from `response' for the `vendor'."))

;;;; Utilities
(defun llm-vendor-utils-get-system-prompt (prompt &optional example-prelude)
  "From `prompt', turn the context and examples into a string.
`example-prelude' is a string to prepend to the examples."
  (string+
   (llm-chat-prompt-context prompt)
   (when (llm-chat-prompt-context prompt) "\n")
   (when (llm-chat-prompt-examples prompt)
     (or example-prelude
         (string+
          (if (= (length (llm-chat-prompt-examples prompt)) 1)
              "Here is an example"
              (format nil "Here are ~d examples"
                      (length (llm-chat-prompt-examples prompt))))
          " of how to respond:\n")))
   (when (llm-chat-prompt-examples prompt) "\n")
   (format nil "~{~a~^~%~}"
           (mapcar (lambda (example)
                     (format nil "User: ~a\nAssistant: ~a"
                             (car example)
                             (cdr example)))
                   (llm-chat-prompt-examples prompt)))))

(defun llm-vendor-utils-combine-to-system-prompt (prompt &optional example-prelude)
  "Add context and examples to a system prompt in `prompt'.

This should be used for vendors that have a notion of a system prompt.
If there is a system prompt, and no assistant response, add to it.
If there is no system prompt, create one.
If there is an assistance response, do nothing.

`example-prelude' is the text to introduce any examples with."
  (let ((system-prompt (find-if
                        (lambda (exchange)
                          (eq (llm-chat-prompt-exchange-role exchange) 'system))
                        (llm-chat-prompt-exchanges prompt)))
        (system-content (llm-vendor-utils-get-system-prompt prompt example-prelude)))
    (when (and system-content (> (length system-content) 0))
      (if* system-prompt
         then (setf (llm-chat-prompt-exchange-content system-prompt)
                    (string+ (llm-chat-prompt-exchange-content system-prompt)
                             "\n"
                             system-content))
         else (push (make-instance 'llm-chat-prompt-exchange
                                    :role 'system
                                    :content system-content)
                    (llm-chat-prompt-exchanges prompt))
              (setf (llm-chat-prompt-context prompt) nil
                    (llm-chat-prompt-examples prompt) nil)))))

(defun llm-vendor-utils-combine-to-user-prompt (prompt &optional example-prelude)
  "Add context and examples to a user prompt in `prompt'.
This should be used for vendors that do not have a notion of a system prompt.

`example-prelude' is the text to introduce any examples with."
  (let ((system-content (llm-vendor-utils-get-system-prompt prompt example-prelude)))
    (when system-content
      (setf (llm-chat-prompt-exchange-content (car (llm-chat-prompt-exchanges prompt)))
            (string+ system-content
                     "\n"
                     (llm-chat-prompt-exchange-content (car (llm-chat-prompt-exchanges prompt))))
            (llm-chat-prompt-context prompt) nil
            (llm-chat-prompt-examples prompt) nil))))

(defun llm-vendor-utils-collapse-history (prompt &optional history-prelude)
  "Collapse history to a single `prompt'.

This is useful for vendors that cannot handle conversations. Essentially it's a
way to fake conversation. Caution: tokens will eventually run out, though, so
this isn't a sustainable way to do things. Vendors should probably issue a
warning when using this.

`history-prelude' is the text to use to tell the LLM that conversation history
will follow."
  (when (> (length (llm-chat-prompt-exchanges prompt)) 1)
    (setf (llm-chat-prompt-exchanges prompt)
          (list (make-instance 'llm-chat-prompt-exchange
                               :role 'user
                               :content
                               (string+ (or history-prelude "Previous exchanges:")
                                        "\n\n"
                                        (format nil "~{~a~^~%~}"
                                                (mapcar (lambda (exchange)
                                                          (format nil "~a: ~a" (case (llm-chat-prompt-exchange-role exchange)
                                                                                 (user "User")
                                                                                 (assistant "Assistant"))
                                                                  (llm-chat-prompt-exchange-content exchange)))
                                                        (butlast (llm-chat-prompt-exchanges prompt))))
                                        "\n\nThe current conversation follows:\n\n"
                                        (llm-chat-prompt-exchange-content (car (last (llm-chat-prompt-exchanges prompt))))))))))

(defun llm-vendor-utils-model-token-limit (model)
  "Return the token limit for `model'."
  (let ((model (string-downcase model)))
    (cond
      ((match-re "mistral-7b" model) 8192)
      ((match-re "mistral" model) 8192)
      ((match-re "mixtral-45b" model) 131072)
      ((match-re "mixtral" model) 131072)
      ((match-re "falcon" model) 2048)
      ((match-re "orca 2" model) 4096)
      ((match-re "orca" model) 2048)
      ((match-re "llama\s*2" model) 4096)
      ((match-re "llama" model) 2048)
      ((match-re "starcoder" model) 8192))))

(defun llm-vendor-utils-append-to-prompt (prompt output &optional func-results role)
  "Append `output' to `prompt' as an assistant exchange.

`output' can be a string or a structure in the case of function calls.

`role' will be `assistant' by default, but can be passed in for other roles."
  (setf (llm-chat-prompt-exchanges prompt)
        (append (llm-chat-prompt-exchanges prompt)
                (list (make-instance 'llm-chat-prompt-exchange
                                     :role (if* func-results
                                              then 'function
                                              else (or role 'assistant))
                                     :content output
                                     :function-call-result func-results)))))

(defclass llm-vendor-utils-function-call ()
  ((id :initarg :id :accessor llm-vendor-utils-function-call-id)
   (name :initarg :name :accessor llm-vendor-utils-function-call-name)
   (args :initarg :args :accessor llm-vendor-utils-function-call-args))
  (:documentation "A class to hold information about a function call.

`id' is a call ID, which is optional.

`name' is the function name.

`arg' is an alist of arguments to values."))

(defun llm-vendor-utils-process-result (vendor prompt text funcalls)
  "Process the `response' from the vendor for `prompt'.
This execute function calls if there are any, does any result appending to the
prompt, and returns an appropriate response for the client.

`vendor' is the struct that configures the use of the LLM.

`funcalls' is a list of function calls, if any.

`text' is the text output from the vendor, if any.  There should
be either FUNCALLS or TEXT."
  (if* funcalls
     then (llm-vendor-utils-execute-function-calls vendor prompt funcalls)
     else (when text
            (llm-vendor-append-to-prompt vendor prompt text))
          text))

(defun llm-vendor-utils-populate-function-results (vendor prompt func result)
  "Append the `result' of `func' to `prompt'.

`func' is a `llm-vendor-utils-function-call' struct.

`vendor' is the struct that configures the user of the LLM."
  (llm-vendor-append-to-prompt
   vendor
   prompt
   result
   (make-instance 'llm-chat-prompt-function-call-result
                  :call-id (llm-vendor-utils-function-call-id func)
                  :function-name (llm-vendor-utils-function-call-name func)
                  :result result)))

(defun llm-vendor-utils-execute-function-calls (vendor prompt funcalls)
  "Execute `funcalls', a list of `llm-vendor-utils-function-calls'.

A response suitable for returning to the client will be returned.

`vendor' is the vendor that supplied the response.

`prompt' was the prompt given to the vendor, which will get updated with the
response from the LLM, and if there is a function call, the result.

This returns the response suitable for output to the client; a
cons of functions called and their output."
  (llm-vendor-populate-function-calls vendor prompt funcalls)
  (loop for func in funcalls
        collect (let* ((name (llm-vendor-utils-function-call-name func))
                       (arguments (llm-vendor-utils-function-call-args func))
                       (function (find-if
                                  (lambda (f) (string= name (llm-function-call-name f)))
                                  (llm-chat-prompt-functions prompt))))
                  (cons name
                        (let* ((args (loop for arg in (llm-function-call-args function)
                                           collect (cdr (find-if (lambda (a)
                                                                   (eq (intern
                                                                        (llm-function-arg-name arg))
                                                                       (car a)))
                                                                 arguments))))
                               (result (apply (llm-function-call-function function) args)))
                          (llm-vendor-utils-populate-function-results vendor prompt func result)
                          result)))))
