(in-package :llama2)

(defvar *llama-cpp-python-api-protocol*)
(defvar *llama-cpp-python-api-host*)
(defvar *llama-cpp-python-api-port*)
(defvar *llama-cpp-dimensions* 4096)
(defvar *llama-cpp-default-max-tokens* 2048)
(defvar *llama-cpp-default-model* "llama2_7b_chat_uncensored.ggmlv3.q3_K_L.bin")
(defvar *llama-cpp-default-n* 1)
(defvar *llama-cpp-default-stop-list* '("###"))
(defvar *llama-cpp-default-temperature* 0.8)
(defvar *llama-cpp-default-timeout* 1200)
(defvar *llama-cpp-default-best-of* 1)
(defvar *llama-cpp-default-echo* :false)
(defvar *llama-cpp-default-frequency-penalty* 0)
(defvar *llama-cpp-default-logit-bias* :null)
(defvar *llama-cpp-default-logit-bias-type* "tokens")
(defvar *llama-cpp-default-logprobs* :null)
(defvar *llama-cpp-default-mirostat-eta* 0.1)
(defvar *llama-cpp-default-mirostat-mode* 0)
(defvar *llama-cpp-default-mirostat-tau* 5)
(defvar *llama-cpp-default-presence-penalty* 0.0)
(defvar *llama-cpp-default-repeat-penalty* 1.1)
(defvar *llama-cpp-default-stream* :false)
(defvar *llama-cpp-default-suffix* :null)
(defvar *llama-cpp-default-top-k* 40)
(defvar *llama-cpp-default-top-p* 0.95)
(defvar *llama-cpp-default-user* "anonymous")


(defun call-llama.cpp_Python_API (cmd &key
                          (method :get)
                          (content nil)
                          (timeout *llama-cpp-default-timeout*)
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
    (when verbose (log-llm "content=~S~%" content))
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
        (log-llm "headers=~S~%" headers)
        (log-llm "body=~a~%" (length body))
        (log-llm "body=~S~%" body)
        (log-llm "code=~a~%" code))
      (let ((jso (handler-case (read-json body)
                   (error (e)
                     (log-llm "~a: Unable to read json: ~a~%" e body)
                     (jso)))))
        jso))))


(defun ask-llama2-models-jso ()
  (call-llama.cpp_Python_API "models" :method :get :verbose nil))


(eval-when (compile load eval)
  (setq key-args-list
    '((best-of *llama-cpp-default-best-of*)
      (echo *llama-cpp-default-echo*)
      (frequency-penalty *llama-cpp-default-frequency-penalty*)
      (logit-bias *llama-cpp-default-logit-bias*)
      (logit-bias-type *llama-cpp-default-logit-bias-type*)
      (logprobs *llama-cpp-default-logprobs*)
      (max-tokens *llama-cpp-default-max-tokens*)
      (mirostat_eta *llama-cpp-default-mirostat-eta*)
      (mirostat_mode *llama-cpp-default-mirostat-mode*)
      (mirostat_tau *llama-cpp-default-mirostat-tau*)
      (model *llama-cpp-default-model*)        
      (n *llama-cpp-default-n*)
      (presence-penalty *llama-cpp-default-presence-penalty*)
      (repeat-penalty *llama-cpp-default-repeat-penalty*)
      (stop *llama-cpp-default-stop-list*)
      (stream *llama-cpp-default-stream*)
      (suffix *llama-cpp-default-suffix*)
      (temperature *llama-cpp-default-temperature*)
      (timeout *llama-cpp-default-timeout*)
      (top-k *llama-cpp-default-top-k*)
      (top-p *llama-cpp-default-top-p*)
      (user  *llama-cpp-default-user*)
      (verbose nil)
      )
    
    key-args-signature 
    '(
      :best-of best-of
      :echo echo
      :frequency-penalty frequency-penalty
      :logit_bias logit-bias
      :logit-bias-type logit-bias-type
      :logprobs logprobs
      :max-tokens max-tokens                           
      :mirostat_eta mirostat_eta
      :mirostat_mode mirostat_mode
      :mirostat_tau mirostat_tau
      :model model
      :n n
      :presence-penalty presence-penalty
      :repeat-penalty repeat-penalty
      :stop stop
      :stream stream
      :suffix suffix
      :temperature temperature
      :timeout timeout
      :top-p top-p
      :top-k top-k                           
      :user user
      :verbose verbose)
    
    key-args-pushjso 
    '(
      (pushjso "best_of" best-of jso)
      (pushjso "echo" echo jso)
      (pushjso "frequency_penalty" frequency-penalty jso)
      (pushjso "logit_bias" logit-bias jso)
      (pushjso "logit_bias_type" logit-bias-type jso)
      (pushjso "logprobs" logprobs jso)
      (pushjso "max_tokens" max-tokens jso)                         
      (pushjso "mirostat_eta" mirostat_eta jso)
      (pushjso "mirostat_mode" mirostat_mode jso)
      (pushjso "mirostat_tau" mirostat_tau jso)                         
      (pushjso "model" model jso)
      (pushjso "n" n jso)
      (pushjso "presence_penalty" presence-penalty jso)
      (pushjso "repeat_penalty" repeat-penalty jso)                         
      (pushjso "stop" stop jso)
      (pushjso "suffix" suffix jso)                         
      (pushjso "temperature" temperature jso)
      (pushjso "top_k" top-k jso)
      (pushjso "top_p" top-p jso)                         
      (pushjso "user" user jso))))



(key-args-fun chat "" `(ask-chat prompt-or-messages ,@key-args-signature))

(key-args-fun ask-chat "" 
              `(cond ((stringp prompt-or-messages)
                            (ask-llama2-completions prompt-or-messages ,@key-args-signature))
                           (t (ask-llama2-chat-completions prompt-or-messages ,@key-args-signature))))



(key-args-fun ask-llama2-chat-completions-jso "" 
              `(let ((jso (jso))
                     (message-array nil))
                 stream ; to avoid unused var error
                 (loop for (role . content) in (reverse prompt-or-messages)
                       for n from 0 do
                         (let ((message-jso (jso)))
                           (pushjso "content" content message-jso)
                           (pushjso "role" role message-jso)
                           (push  message-jso message-array)))
                 (pushjso "messages" message-array jso)
                 ,@key-args-pushjso
;;;    (log-llm "~a~%" (json-string jso))
                 (call-llama.cpp_Python_API "chat/completions" :method :post :verbose verbose
                                                               :timeout timeout
                                                               :content (json-string jso))))



(key-args-fun  ask-llama2-completions ""
               `(let ((jso (ask-llama2-completions-jso prompt-or-messages ,@key-args-signature)))
                  stream ;; to avoid unused var error
                  (or (cond (jso
                             (let* ((choices (jso-val jso "choices"))
                                    (error-val (jso-val jso "error"))
                                    (choice (car choices)))
                               (when verbose (log-llm "choices=~S~%error=~S~%" choices error-val))
                               (cond (error-val (jso-val error-val "message"))
                                     (choice
                                      (jso-val choice "text"))))))

                      "No text")))



(key-args-fun ask-llama2-chat-completions "" 
              `(let ((jso (ask-llama2-chat-completions-jso prompt-or-messages ,@key-args-signature)))
                 (or (cond (jso
                            (let* ((choices (jso-val jso "choices"))
                                   (error-val (jso-val jso "error"))
                                   (choice (car choices)))
                              (when verbose (log-llm "choices=~S~%error=~S~%" choices error-val))
                              (cond (error-val (jso-val error-val "message"))
                                    (choice
                                     (let ((message (jso-val choice "message")))
                                       (cond (message (jso-val message "content")))))))))
                     "No text")))



(key-args-fun ask-llama2-completions-jso "" 
              `(let* ((jso (jso))
                      (prompt (format nil "\\n\\n### Instructions:\\n~a\\n\\n### Response:\\n" prompt-or-messages)))
                 
                 stream ;; to avoid unused var error
                 ;;;    (log-llm "~a~%" prompt)
                 (pushjso "prompt" prompt jso)
                 ,@key-args-pushjso
                 (call-llama.cpp_Python_API "completions" :method :post
                                            :verbose verbose
                                            :timeout timeout
                                            :content (json-string jso))))



(key-args-fun ask-for-list "" 
              `(let ((response (ask-llama2-chat-completions
                                   `(

                                     ("system" .
                                               "You are a helpful assistant who can respond only with a JSON array of double-quoted strings.")

                                     ("user" . "List 5 colors.")
                                     ("system" . "[\"red\", \"green\", \"blue\", \"yellow\", \"purple\"]")

                                     ("user" . "Name the New England States.")
                                     ("system" . "[\"Maine\", \"New Hampshire\", \"Vermont\", \"Massachusetts\", \"Rhode Island\", \"Connecticut\"]")

                                     ("user" . "Suggest 10 famous artists.")
                                     ("system"  . "[\"Pablo Picasso\", \"Leonardo da Vinci\", \"Michelangelo\", \"Eugene Delacroix\", \"Claude Monet\", \"Camille Pissarro\", \"Gustav Klimt\", \"Henri Matisse\", \"Georges Seurat\", \"Paul Cezanne\"]")

                                     ("user" . "Enumerate the causes of the US Civil War.")
                                     ("system" . "[\"Slavery\", \"State's Rights vs. Federal Authority\", \"Economic Causes\", \"Abolitionism and Free Soil Movement\", \"Sectionalism\"]")

                                     #+ignore
                                     ("user" . "Name the 100 biggest cities in the world.")
                                     #+ignore
                                     ("system" . "[\"Tokyo\", \"Delhi\", \"Shanghai\", \"Sao Paulo\", \"Mumbai\", \"Beijing\"
 \"Mexico City\", \"Osaka\", \"Cairo\", \"New York City\", \"Dhaka\", \"Karachi\"
 \"Buenos Aires\", \"Istanbul\", \"Kolkata\", \"Kinshasa\", \"Manila\"
 \"Rio de Janeiro\", \"Lahore\", \"Guangzhou\", \"Shenzhen\", \"Bangalore\", \"Moscow\"
 \"Tianjin\", \"Jakarta\", \"London\", \"Lima\", \"Bangkok\", \"Chennai\"
 \"New Taipei City\", \"Ho Chi Minh City\", \"Hyderabad\", \"Bogota\", \"Hong Kong\"
 \"Ahmedabad\", \"Baghdad\", \"Hangzhou\", \"Rio de Janeiro\", \"Santiago\", \"Riyadh\"
 \"Singapore\", \"Shijiazhuang\", \"Wuhan\", \"Toronto\", \"Los Angeles\", \"Lahore\"
 \"Paris\", \"Chengdu\", \"Lima\", \"Nagoya\", \"Bangkok\", \"Belo Horizonte\"
 \"Shenyang\", \"Riyadh\", \"Miami\", \"Chennai\", \"Dallas\", \"Madrid\", \"Nanjing\"
 \"Houston\", \"Bogota\", \"Toronto\", \"Ho Chi Minh City\", \"Atlanta\"
 \"Washington, D.C.\", \"Khartoum\", \"Barcelona\", \"Bangalore\", \"Houston\"
 \"Kuala Lumpur\", \"Belo Horizonte\", \"Khartoum\", \"Seattle\", \"Tehran\"
 \"Nairobi\", \"Pune\", \"Brisbane\", \"Berlin\", \"Kabul\", \"Madrid\", \"Sydney\"
 \"Denver\", \"Bangkok\", \"Ankara\", \"Pyongyang\", \"Boston\", \"Athens\", \"Chennai\"
 \"Ankara\", \"Phoenix\", \"San Francisco\", \"Perth\", \"Chiang Mai\", \"Barcelona\"
 \"Cape Town\", \"Las Vegas\"]")
                                     ("user" . ,prompt-or-messages))
                                   ,@key-args-signature
                                   )))
                    (handler-case
                        (read-json response)
                      (error (e) (declare (ignore e)) (jso)))))



(eval-when (compile load eval)
  (setq key-args-list
    '((verbose nil)
      (model *llama-cpp-default-model*)
      (timeout *llama-cpp-default-timeout*)
      (normalize t))
        
    key-args-signature 
    '(:verbose verbose :model model :timeout timeout :noralize normalize)))



(key-args-fun ask-llama2-embeddings-jso "" `
              (let ((jso  (jso)))
                normalize ;; to avoid unused var warning
                timeout    ;; to avoid unused var warning 
                model      ;; to avoid unused var warning 
                (pushjso "input" prompt-or-messages jso)
                (call-llama.cpp_Python_API "embeddings" :method :post :content (json-string jso) :verbose verbose)))

(key-args-fun ask-llama2-embeddings "" 
                `(let ((jso (ask-llama2-embeddings-jso prompt-or-messages ,@key-args-signature)))
                                        ;(setf *jso* jso)
                  (or (cond (jso
                             (let* ((data (jso-val jso "data"))
                                    (datum (car data)))
                               (cond (datum (let ((embedding (jso-val datum "embedding")))
                                        ;(setf *embedding* embedding)
                                              embedding))))))
                      nil)))

(key-args-fun embed "" 
              `(progn (when verbose (log-llm "Embed '~a'.~%" prompt-or-messages))
                      (let ((error-message nil)
                            (embedding nil))
                        (setf embedding (handler-case (llama2::ask-llama2-embeddings prompt-or-messages ,@key-args-signature)
                                          (error (e) (setf error-message (format nil "API error on ~a://~a:~a: ~a~%"
                                                                                 *llama-cpp-python-api-protocol*
                                                                                 *llama-cpp-python-api-host*
                                                                                 *llama-cpp-python-api-port*
                                                                                 e))
                                            nil)))
                        (cond (embedding
                               (cond (normalize (let ((mag (mag embedding)))
                                                  (setf embedding (mapcar (lambda (u) (/ u mag)) embedding))))
                                     (t (setf embedding (mapcar (lambda (u) (coerce u 'single-float)) embedding))))
                               (setf embedding (coerce embedding 'single-float-array)))
                              (t (setf embedding (make-array llama2::*llama-cpp-dimensions* :element-type 'single-float :initial-element 0.0))
                                 (when (null error-message) (setf  error-message (format nil "Something went wrong with llama2-cpp on ~a://~a:~a: null embedding~%"
                                                                                         *llama-cpp-python-api-protocol*
                                                                                         *llama-cpp-python-api-host*
                                                                                         *llama-cpp-python-api-port*)))))
                        (values embedding error-message))))
                          





#|
(defun sample-llama2-vector-database ()
  (let* ((name "historicalFigures4096")
         (vector-database (read-vector-database name))
         (side-effect (setf (vector-database-embedder vector-database) #'llama2::embed))
         (type-name "Historical Figure")
         (type-plural "Historical Figures")
         (visited (mapcar 'cadr (vector-database-property-vectors vector-database)))
         (elements (ask-for-list (format nil "List 100 ~a." type-plural))))
    (declare (ignore side-effect))
    (setf elements (remove-if (lambda (u) (null (stringp u))) elements))
    (log-llm "~a elements~%" (length elements))
    (setf elements (remove-duplicates elements :test 'string-equal))
    (log-llm "~a elements without duplicates~%" (length elements))
    (setf elements (set-difference elements visited :test 'string=))
    (log-llm "~a elements without already visited~%" (length elements))
    (log-llm "~a total ~a~%" (+ (length elements) (length visited)) type-plural)
;;;    (setf (vector-database-embedder vector-database) #'llama2::embed)
    (log-llm "~a:~%" type-plural)
    (dolist (elt elements)
      (let* ((vec (embed elt))
             (id (gentemp "id-"))
             (properties (list id elt type-name)))
        (log-llm "~a~%" elt)
        (push properties (vector-database-property-vectors vector-database))
        (push vec (vector-database-embedding-vectors vector-database))))
    (write-vector-database vector-database)
    vector-database))

(defun t2 () (let ((vector-database (sample-llama2-vector-database)))
   (nn vector-database "Famous scientist" :min-score 0.0)))



|#
         

