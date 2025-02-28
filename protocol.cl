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
  ;; Classes, Structs, Constructors and Accessors
  (:export #:llm-standard-vendor
           #:llm-standard-chat-vendor
           #:llm-standard-chat-vendor-default-chat-temperature
           #:llm-standard-chat-vendor-default-chat-max-tokens
           #:llm-standard-chat-vendor-default-chat-non-standard-params
           #:llm-standard-full-vendor
           ;; llm-chat-prompt
           #:llm-chat-prompt
           #:make-llm-chat-prompt
           #:copy-llm-chat-prompt
           #:llm-chat-prompt-context
           #:llm-chat-prompt-examples
           #:llm-chat-prompt-exchanges
           #:llm-chat-prompt-tools
           #:llm-chat-prompt-temperature
           #:llm-chat-prompt-max-tokens
           #:llm-chat-prompt-response-format
           #:llm-chat-prompt-non-standard-params
           ;; llm-chat-prompt-exchange
           #:llm-chat-prompt-exchange
           #:make-llm-chat-prompt-exchange
           #:copy-llm-chat-prompt-exchange
           #:llm-chat-prompt-exchange-role
           #:llm-chat-prompt-exchange-content
           #:llm-chat-prompt-exchange-tool-results
           ;; llm-chat-prompt-tool-result
           #:llm-chat-prompt-tool-result
           #:make-llm-chat-prompt-tool-result
           #:copy-llm-chat-prompt-tool-result
           #:llm-chat-prompt-tool-result-call-id
           #:llm-chat-prompt-tool-result-tool-name
           #:llm-chat-prompt-tool-result-result
           ;; llm-tool
           #:llm-tool
           #:make-llm-tool
           #:llm-tool-function
           #:llm-tool-name
           #:llm-tool-description
           #:llm-tool-args)
  ;; Generic functions
  (:export #:llm-capabilities
           #:llm-chat-token-limit
           #:llm-cancel-request
           #:llm-vendor-request-prelude
           #:llm-vendor-headers
           ;; Chat
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
           #:llm-vendor-extract-tool-uses
           #:llm-vendor-populate-tool-uses
           #:llm-vendor-collect-streaming-function-data
           ;; Embeddings
           #:*llm-embedding-default-float-format*
           #:llm-embedding
           #:llm-embedding-async
           #:llm-batch-embeddings
           #:llm-embedding-url
           #:llm-vendor-embedding-request
           #:llm-vendor-batch-embeddings-request
           #:llm-vendor-embedding-extract-error
           #:llm-vendor-embedding-extract-result
           #:llm-vendor-batch-embeddings-extract-result)
  ;; Utilities
  (:export
   #:llm-vendor-utils-get-system-prompt
   #:llm-vendor-utils-combine-to-system-prompt
   #:llm-vendor-utils-combine-to-user-prompt
   #:llm-vendor-utils-collapse-history
   #:llm-vendor-utils-model-token-limit
   #:llm-vendor-utils-convert-plist-to-jso
   #:llm-vendor-utils-openai-arguments
   #:llm-vendor-utils-openai-tool-spec
   #:llm-vendor-utils-append-to-prompt
   #:llm-vendor-utils-tool-use
   #:make-llm-vendor-utils-tool-use
   #:copy-llm-vendor-utils-tool-use
   #:llm-vendor-utils-tool-use-id
   #:llm-vendor-utils-tool-use-name
   #:llm-vendor-utils-tool-use-args
   #:llm-vendor-utils-process-result
   #:llm-vendor-utils-populate-tool-uses
   #:llm-vendor-utils-execute-tool-uses))

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

(defun llm-request-sync (url &key headers content timeout)
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
    (declare (ignore status))
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

(defstruct (llm-chat-prompt (:constructor %make-llm-chat-prompt))
  "This stores all the information needed for a structured chat prompt."
  context
  examples
  exchanges
  tools
  temperature
  max-tokens
  response-format
  non-standard-params)

(defstruct llm-chat-prompt-exchange
  "This defines a single exchange given as part of a chat prompt.

`role' can be a keyword, of either `user', `assistant', or `tool-results'.

`content' is the content of the interaction. It should be either string, or a
list of tool uses.

`tool-results' is a list of structs of type `llm-chat-prompt-tool-result', which
is only populated if `role' is `tool-results'. It stores the results of the
function calls."
  role
  content
  tool-results)

(defstruct llm-chat-prompt-tool-result
  "This defines the result from a tool use.

`call-id' is an ID for this tool call, if available.

`tool-name' is the name of the tool. This is required.

`result' is the result of the tool call. This is required."
  call-id
  tool-name
  result)

(defclass llm-tool ()
  ((function :initform nil :initarg :function :accessor llm-tool-function)
   (name :initform nil :initarg :name :accessor llm-tool-name)
   (description :initform nil :initarg :description :accessor llm-tool-description)
   (args :initform nil :initarg :args :accessor llm-tool-args)
   (async :initform nil :initarg :async :accessor llm-tool-async))
  (:documentation "This is a class to represent a singal tool available to the LLM.

`function' is a Lisp function to call.

`name' is a human readable name of the tool.

`description' is a human readable description of the tool.

`args' is a list of plists, each plist having the keys `:name', `:type',
`:description', and `:optional'. `:type' is a string, and the same set of types
as in `response-format' arg in `llm-make-chat-prompt': `string', `integer',
`boolean', `float', or `array'. There can be an `:enum' field as well, with a
vector of possible values.

`async', if non-nil, means the function will be passed a callback which takes
the return value, otherwise the callback is not passed, and the function's
return value will be used."))

(defun make-llm-tool (&key function name description args async)
  (make-instance 'llm-tool
                 :function function
                 :name name
                 :description description
                 :args args
                 :async async))

;;; Generic functions
(defgeneric llm-capabilities (vendor)
  (:documentation "Return a list of the capabilities of `vendor'.

This possible values are only those things that are not the bare minimum of
functionality to be included in this package, which is non-streaming chat:

`:streaming': the LLM can actually stream responses in the streaming call. Calls
to `llm-chat-streaming' will work regardless even if the LLM doesn't support
streaming, it just won't have any partial responses, so basically just operates
like `llm-chat-async'.

`:embeddings': the LLM can return vector embeddings of text.

`:embeddings-batch': the LLM can return many vector embeddings at the same time.

`:tool-uses': the LLM can call functions.")
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

(defun make-llm-chat-prompt (content &key context examples tools
                                       temperature max-tokens response-format
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

`content' is the latest user input to the LLM, the thing to be responded to, in
form of a string containing text or an `llm-multipart' object containing both
text and media. This is required. This can also be a list, in which case it
represents the chat history, starting with the user's initial chat, followed by
the response, and so on. If it is a list, it MUST be an odd number, since the
presumption is that it ends with the user's latest input to the LLM.

 `context' is a string given to the LLM as context for the entire exchange, such
as instructions to the LLM on how to reply, persona, information on the user, or
anything else that applies to the chat as a whole. This is optional.

`examples' is a list of conses, where the car is an example inputs, and cdr is
the corresponding example outputs. This is optional.

`tools' is a list of `llm-tool' structs. These may be called IF the LLM supports
them. If the LLM does not support them, a `not-implemented' signal will be
thrown. This is optional. When this is given, the LLM will either call the
function or return text as normal, depending on what the LLM decides.

`temperature' is a floating point number with a minimum of 0, and maximum of 1,
which controls how predictable the result is, with 0 being the most
predicatable, and 1 being the most creative. This is not required.

`max-tokens' is the maximum number of tokens to generate. This is optional.

If `response-format' is `:json' (the currently only accepted keyword value), we
will attempt to force ouput to fit the format. This should not be used with
function calling. If this is the instructions to the LLM should tell the model
about the format, for example with JSON format by including examples or
describing the schema. This can also be a structure defining the JSON schema,
which will be passed directly to `llm-vendor-utils-convert-plist-to-jso',
following the JSON schema rules (see http://json-schema.org). The structure is
plist that can be either `(:type <type> <additional-properties...>)', or in the
case of enums `(:enum [val1 val2 ... valn])'. All types and values used as the
values in plists and vectors should be strings, not symbols. LLMs will often
require the top-level schema passed in to be an object: `(:type \"object\"
:properties (:val <schema> :other-val <other-schema>) :required [\"val\"
\"other-val\"])'. Often, all properties must be required. Arrays can be
specified with `(:type \"array\" :items <schema>)'.

`context', `examples', `functions', `temperature', and `max-tokens' are usually
turned into part of the exchange, and if so, they will be put in the first
exchange of the prompt (before anything in `previous-exchanges').

`non-standard-params' is an alist of other options that the vendor may or may
not know how to handle. These are expected to be vendor specific. Don't use this
if you want the prompt to be used amongst different vendors, because it is
likely to cause a request error. The cars of the alist are strings and the cdrs
can be strings or numbers. This is optional."
  (when (null content)
    (error "TEXT must NOT be nil"))
  (when (and (listp content)
             (zerop (mod (length content) 2)))
    (error "TEXT, when given as a list, must have an odd number of elements"))
  (%make-llm-chat-prompt
   :context context
   :examples examples
   :exchanges (loop for i from 0
                    for s in (if (listp content) content (list content))
                    collect (make-llm-chat-prompt-exchange
                             :role (if (zerop (mod i 2)) 'user 'assistant)
                             :content s))
   :tools tools
   :temperature temperature
   :max-tokens max-tokens
   :response-format response-format
   :non-standard-params non-standard-params))

(defun llm-chat-prompt-append-response (prompt response &optional role)
  "Append a new `response' to `prompt', to continue a conversation.
`role' default to `user', which should almost always be what is needed."
  (setf (llm-chat-prompt-exchanges prompt)
        (append (llm-chat-prompt-exchanges prompt)
                (list (make-llm-chat-prompt-exchange :role (or role 'user)
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
    (let* ((response (llm-request-sync
                      (llm-vendor-chat-url vendor)
                      :headers (llm-vendor-headers vendor)
                      :content (llm-vendor-chat-request vendor prompt nil)
                      :timeout (llm-vendor-chat-timeout vendor)))
           (final-result nil)
           (err (llm-vendor-chat-extract-error vendor response)))
      (if* err
         then (error err)
         else (llm-vendor-utils-process-result
               vendor
               prompt
               (llm-vendor-chat-extract-result vendor response)
               (llm-vendor-extract-tool-uses vendor response)
               (lambda (result)
                 (setq final-result result)))
              ;; In most cases, final-result will be available immediately.  However, when
              ;; executing tools, we need to wait for their callbacks, and only after
              ;; those are called with this be ready.
              (while (not final-result)
                (sleep 0.1))
              final-result))))

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

(defgeneric llm-vendor-append-to-prompt (vendor prompt result &optional tool-results)
  (:documentation "Append `result' to `prompt' for the `vendor'.

`prompt' is the prompt that was already sent to the vendor.

`tool-results' is a list of tool results, if any.")
  (:method ((vendor llm-standard-chat-vendor) prompt result &optional tool-results)
    "By default, we just append to the prompt."
    (llm-vendor-utils-append-to-prompt prompt result tool-results)))

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

(defgeneric llm-vendor-extract-tool-uses (vendor response)
  (:documentation "Return the tool uses results from `response' for the `vendor'.

If there are no tool uses, return nil. If there are tool uses, return a list of
`llm-vendor-utils-tool-use'.")
  (:method ((vendor llm-standard-chat-vendor) response)
    "By default, the standard vendor has no function call extractor."
    (declare (ignore response))
    nil))

(defgeneric llm-vendor-populate-tool-uses (vendor prompt tool-uses)
  (:documentation "For `vendor', in `prompt', record `tool-uses'.
This is the recording before the function calls were executed, in the prompt.
`tool-uses' are a list of `llm-vendor-utils-tool-use'."))

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

(defparameter *llm-embedding-default-float-format* 'single-float
  "By default, read floating numbers as 'single-float (32 bit).")

(defgeneric llm-embedding (vendor text)
  (:documentation "Return a vector embedding of `text' from `vendor'.")
  (:method ((vendor llm-standard-full-vendor) text)
    (llm-vendor-request-prelude vendor)
    (let* ((response (llm-request-sync
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

(defgeneric llm-batch-embeddings (vendor text-sequence)
  (:documentation "Return a list of embedding vectors of `text-sequence'.

The list of vectors is in an order corresponding to the order of
`text-sequence'.

`vendor' is the vendor struct that will be used for an LLM call.")
  (:method ((vendor llm-standard-full-vendor) text-sequence)
    (llm-vendor-request-prelude vendor)
    (let* ((response (llm-request-sync (llm-vendor-embedding-url vendor)
                                       :headers (llm-vendor-headers vendor)
                                       :content (llm-vendor-batch-embeddings-request vendor text-sequence)
                                       :timeout (llm-vendor-chat-timeout vendor)))
           (err-msg (llm-vendor-embedding-extract-error vendor response)))
      (if* err-msg
         then (error err-msg)
         else (llm-vendor-batch-embeddings-extract-result vendor response)))))

(defgeneric llm-vendor-embedding-url (vendor)
  (:documentation "Return the URL for embeddings for the `vendor'."))

(defgeneric llm-vendor-embedding-request (vendor text)
  (:documentation "Return the request for the `vendor' for `text'.")
  (:method :around (vendor text)
    (declare (ignore vendor text))
    (let ((jso (call-next-method)))
      (st-json:write-json-to-string jso))))

(defgeneric llm-vendor-batch-embeddings-request (vendor text-sequence)
  (:documentation "Return the request for the `vendor' for `text-sequence'.")
  (:method :around (vendor text-sequence)
    (declare (ignore vendor text-sequence))
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
  (:documentation "Return the result from `response' for the `vendor'.")
  (:method :around (vendor response)
    (declare (ignore vendor response))
    (let ((st-json:*json-read-default-float-format* *llm-embedding-default-float-format*))
      (call-next-method))))

(defgeneric llm-vendor-batch-embeddings-extract-result (vendor response)
  (:documentation "Return the result from `response' for the `vendor' for a batch request.")
  (:method :around (vendor response)
    (declare (ignore vendor response))
    (let ((st-json:*json-read-default-float-format* *llm-embedding-default-float-format*))
      (call-next-method))))

;;;; Utilities
(defun llm-vendor-utils-get-system-prompt (prompt &optional example-prelude)
  "From `prompt', turn the context and examples into a string.
`example-prelude' is a string to prepend to the examples."
  (string+
   (llm-chat-prompt-context prompt)
   (when (llm-chat-prompt-context prompt) #\Newline)
   (when (llm-chat-prompt-examples prompt)
     (or example-prelude
         (string+
          (if (= (length (llm-chat-prompt-examples prompt)) 1)
              "Here is an example"
              (format nil "Here are ~d examples"
                      (length (llm-chat-prompt-examples prompt))))
          " of how to respond:"
          #\Newline)))
   (when (llm-chat-prompt-examples prompt) #\Newline)
   (format nil "~{~a~^~%~}"
           (mapcar (lambda (example)
                     (format nil "User: ~a~%Assistant: ~a"
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
                             #\Newline
                              system-content))
         else (push (make-llm-chat-prompt-exchange
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
                     #\Newline
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
          (list (make-llm-chat-prompt-exchange
                 :role 'user
                 :content
                 (string+ (or history-prelude "Previous exchanges:")
                          #\Newline #\Newline
                          (format nil "~{~a~^~%~}"
                                  (mapcar (lambda (exchange)
                                            (format nil "~a: ~a" (case (llm-chat-prompt-exchange-role exchange)
                                                                   (user "User")
                                                                   (assistant "Assistant"))
                                                    (llm-chat-prompt-exchange-content exchange)))
                                          (butlast (llm-chat-prompt-exchanges prompt))))
                          #\Newline #\Newline
                          "The current conversation follows:"
                          #\Newline #\Newline
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

(defun llm-vendor-utils-convert-plist-to-jso (plist)
  "Convert `plist' to a `st-json:jso' instance.

The expectation is that any symbol values will be converted to strings for plist
and any nested plists."
  (loop with jso = (st-json:jso)
        for (k v) on plist by #'cddr
        do (setf (st-json:getjso (string+ k) jso)
                 (typecase v
                   (symbol (string+ v))
                   (list (llm-vendor-utils-convert-plist-to-jso v))
                   (t v)))
        finally (return jso)))

(defun llm-vendor-utils-openai-arguments (args)
  "Convert `args' to the OpenAI function calling spec.
`args' is a list of llm argument plists.
Each plist has the structure:
  (:name STRING
   :type KEYWORD
   :description STRING
   :optional BOOLEAN
   :properties PLIST
   :enum VECTOR
   :items (PLIST :type SYMBOL :enum VECTOR :properties PLIST))

`:type' is followed by a keyword, one of `:string', `:number', `:boolean',
`integer', `:object', `:array', or `:enum'.

See OpenAI's documentation for more details:
https://platform.openai.com/docs/guides/structured-outputs?lang=python#supported-schemas"
  (loop with properties = (st-json:jso)
        with required-names = nil
        for arg in args
        for arg-name = (getf arg :name)
        for type = (string+ (getf arg :type))
        for description = (getf arg :description)
        for required = (not (getf arg :optional))
        for enum = (getf arg :enum)
        for items = (getf arg :items)
        for obj-properties = (llm-vendor-utils-convert-plist-to-jso (getf arg :properties))
        for schema = (st-json:jso "type" type)
        do (progn
             (when description
               (setf (st-json:getjso "description" schema) description))
             (when enum
               (setf (st-json:getjso "enum" schema) enum))
             (when items
               (setf (st-json:getjso "items" schema) (llm-vendor-utils-convert-plist-to-jso items)))
             (when obj-properties
               (setf (st-json:getjso "properties" schema) obj-properties))
             (when required
               (push (if (symbolp arg-name) (string+ arg-name) arg-name)
                     required-names))
             (setf (st-json:getjso arg-name properties) schema))
        finally (let ((spec (st-json:jso
                             "type" "object"
                             "properties" properties)))
                  (when required-names
                    (setf (st-json:getjso "required" spec)
                          (nreverse required-names)))
                  (return spec))))

(defgeneric llm-vendor-utils-openai-tool-spec (tool)
  (:documentation "Convert `tool' to an Open AI function spec.")
  ;; The Open AI tool spec follows the JSON schema spec.  See
  ;; https://json-schema.org/understanding-json-schema.
  (:method ((tool llm-tool))
    "Convert `tool' to an Open AI function spec.
Open AI's function spec is a standard way to do this, and will be applicable to
many endpoints.

This returns a `st-json:jso' object."
    (st-json:jso
     "type" "function"
     "function" (st-json:jso
                 "name" (llm-tool-name tool)
                 "description" (llm-tool-description tool)
                 "parameters" (llm-vendor-utils-openai-arguments
                               (llm-tool-args tool))))))

(defun llm-vendor-utils-append-to-prompt (prompt output &optional tool-results role)
  "Append `output' to `prompt' as an assistant exchange.

`output' can be a string or a structure in the case of function calls.

`tool-results' is a list of results from the LLM output, if any.

`role' will be `assistant' by default, but can be passed in for other roles."
  (setf (llm-chat-prompt-exchanges prompt)
        (append (llm-chat-prompt-exchanges prompt)
                (list (make-llm-chat-prompt-exchange
                       :role (or role (if tool-results 'tool-results 'assistant))
                       ;; If it is a structure, it will get converted to JSON,
                       ;; otherwise make sure it is a string.  For tool uses, we
                       ;; want it to be nil.
                       :content (if* (or (not output)
                                         (and (not (stringp output))
                                              (not tool-results)))
                                   then output
                                   else output)
                       :tool-results tool-results)))))

(defstruct llm-vendor-utils-tool-use
  "A class to hold information about a tool use.

`id' is a call ID, which is optional.

`name' is the function name.

`arg' is an alist of arguments to values."
  id
  name
  args)

(defun llm-vendor-utils-process-result (vendor prompt text tool-uses success-callback)
  "Process the `response' from the vendor for `prompt'.
This execute function calls if there are any, does any result appending to the
prompt, and returns an appropriate response for the client.

`vendor' is the struct that configures the use of the LLM.

`tool-uses' is a list of function calls, if any.

`text' is the text output from the vendor, if any. There should be either
`tool-uses' or `text'.

`success-callback' is the callback that will be run when all functions complete."
  (if* tool-uses
     then (llm-vendor-utils-execute-tool-uses vendor prompt tool-uses success-callback)
     else (when text
            (llm-vendor-append-to-prompt vendor prompt text))
          (funcall success-callback text)))

(defun llm-vendor-utils-populate-tool-uses (vendor prompt results-alist)
  "Append the results in RESULTS-ALIST to the prompt.

`vendor' is the struct that configures the user of the LLM.

`prompt' is the prompt to populate into.

`results-alist' is a list of cons of function
calls (`llm-vendor-utils-function-call' structs) and their
results."
  (llm-vendor-append-to-prompt
   vendor
   prompt
   nil
   (loop for (tool-use . result) in results-alist
         collect (make-llm-chat-prompt-tool-result
                  :call-id (llm-vendor-utils-tool-use-id tool-use)
                  :tool-name (llm-vendor-utils-tool-use-name tool-use)
                  :result result))))

(defun llm-vendor-utils-execute-tool-uses (vendor prompt tool-uses success-callback)
  "Execute `tool-uses', a list of `llm-vendor-utils-tool-uses'.

A response suitable for returning to the client will be returned.

`vendor' is the vendor that supplied the response.

`prompt' was the prompt given to the vendor, which will get updated with the
response from the LLM, and if there is a function call, the result.

`success-callback' is the callback that will be run when all functions have
returned results."
  (llm-vendor-populate-tool-uses vendor prompt tool-uses)
  (loop with results = (list)
        with tool-use-and-results = (list)
        for tool-use in tool-uses
        for name = (llm-vendor-utils-tool-use-name tool-use)
        for arguments = (llm-vendor-utils-tool-use-args tool-use)
        for tool = (find-if
                    (lambda (tool-name) (string= name tool-name))
                    (llm-chat-prompt-tools prompt)
                    :key #'llm-tool-name)
        for call-args = (loop for arg in (llm-tool-args tool)
                              collect (st-json:getjso (getf arg :name) arguments))
        for end-func = (lambda (result)
                         (push (cons name result) tool-use-and-results)
                         (push (cons tool-use result) results)
                         (when (= (length results) (length tool-uses))
                           (llm-vendor-utils-populate-tool-uses vendor prompt results))
                         (funcall success-callback tool-use-and-results))
        do (if* (llm-tool-async tool)
              then (apply (llm-tool-function tool)
                          (append (list end-func) call-args))
              else (funcall end-func (apply (llm-tool-function tool) call-args)))))
