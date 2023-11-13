;; See the file "LICENSE" for the full license governing this code.;;
(in-package :llm)

(defvar *serp-api-key* nil)
(defvar *serp-default-timeout* 120)
(defvar *serp-default-retries* 4)
(defvar *serp-default-initial-delay* 0.25)
(defvar *serp-default-top-n* 10)
(defvar *serp-api-url* "https://serpapi.com")

(defun call-serp (cmd &key
                        (method :get)
                        (content nil)
                        (timeout *serp-default-timeout*)
                        (content-type "application/json")
                        (extra-headers nil)
                        (query nil)
                        (retries *serp-default-retries*)
                        (delay *serp-default-initial-delay*)
                        (verbose nil))
  "Generic interface to all openai SERP API functions using do-http-request."
  (handler-case  
      (let ((uri (format nil "~a/~a" *serp-api-url* cmd)))
;;;        (log-llm "uri=~a~%" uri)
        (when verbose (log-llm "content=~S~%" content))
        (multiple-value-bind (body code headers page socket req)
            (net.aserve.client:do-http-request
              uri
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
          (cond ((and (= code 429) (> retries 0)) ;;; HTTP 429 API rate limit exceeded, retry with exponential backoff
                 (sleep delay)
                 (call-serp cmd :method method :content content :timeout timeout :content-type content-type
                                :extra-headers extra-headers :query query :retries (1- retries) :delay (* 2 delay) :verbose verbose))
                ((or (= code 401) (and (>= code 200) (< code 300))) (read-json body))                
                (t (pushjso "error" (format nil "API call to ~a returned HTTP ~a." uri code) (jso))))))
    (error (e) (pushjso "error" (princ-to-string e) (jso)))))



(defun key-index (key keys &key ((:index index) 0))
  "Get the index of a macthing key from the list of keys"
  (cond ((null keys) nil)
        ((search (format nil "[~a]" (car keys)) key) index)
        (t (key-index key (cdr keys) :index (incf index)))))

(defun insert-row-in-branch-order (item table-rows ordered-fields &key ((:key-index key-index) nil))
  "Insertion sort step by key index of path branch,
e.g. a path-branch ending in [description] comes before one ending in [snippet]"
  (let (
        (path-branch (nth 2 item)))
    (when (null key-index) (setf key-index (key-index path-branch ordered-fields)))
    (cond ((null table-rows) (list item))
          (t
           (let ((compare-index (key-index (nth 2 (car table-rows)) ordered-fields)))
             (cond ((>= key-index compare-index)
                    (cons (car table-rows) (insert-row-in-branch-order  item (cdr table-rows) ordered-fields :key-index key-index)))
                   (t
                    (cons item table-rows))))))))



(defvar *serp-exclude-fields*
  '(
    "about_page_link" "about_page_serpapi_link" "ads" "cached_page_link"
    "favicon" "high" "inline" "inline_images"
    "inline_people_also_search_for" "knowledge_graph_search_link" "low"
    "menu_items"
    "organic_results_state" "pagination" "people_also_search_for"
    "position" "price_movement" "query_displayed" "questions_and_answers"
    "related_questions" "related_searches" "search_metadata"
    "search_parameters" "see_more_serpapi_link"
    "serpapi_knowledge_graph_search_link" "serpapi_link"
    "serpapi_pagination" "siteLinks" "snippet_highlighted_words" "tabs"
    "twitter_results"))

(defvar *serp-ordered-include-fields* '("answer" "definitions" "price" "result" "description" "snippet" "title"))

(defun traverse-serp-jso (jso
                          &key
                            ((:table-rows table-rows) nil)
                            ((:path-regex path-regex) ".")
                            ((:ordered-include-fields ordered-include-fields) nil)
                            ((:ordered-error-fields ordered-error-fields) nil)
                            ((:exclude-fields exclude-fields) nil)
                            ((:path path) ""))
  "Walk the JSON object tree returned by SERP API to collect a list (value citation path-branch).  Skip any paths matching exclude-fields."
    (let ((key-value-pairs (st-json::jso-alist jso)))
      (dolist (key-values key-value-pairs)
        (let* ((key (car key-values))
               (values (cdr key-values))
               (value-list (cond ((null (listp values)) (list values)) (t values)))
               (path-branch (format nil "~a[~a]" path key)))
          (cond
            ((member key exclude-fields :test 'string=))
            ((member key ordered-error-fields :test 'string=)
             (dolist (value value-list)
               (handle-llm-error "traverse-serp-jso" value (setf table-rows (insert-row-in-branch-order (list value "https://serpapi.com" path-branch) table-rows ordered-error-fields)))
               ))
            ((member key ordered-include-fields :test 'string=)
             (when (match-re path-regex path-branch)
               (let ((citation (cdr (assoc "link" key-value-pairs :test 'string=))))
                 (when (or (null citation) (match-re "^https://www.google.com" citation)) (setf citation "https://www.google.com"))
                 (dolist (value value-list)
                   (when (null (stringp value)) (setf value (format nil "~a" value)))
                   (setf value (remove-if (lambda (ch) (> (char-code ch) 127)) value))
                   (setf table-rows (insert-row-in-branch-order (list value citation path-branch) table-rows ordered-include-fields))
                   ))))
            (t (let ((index 0)) (dolist (value value-list)
                                  (let ((index-str (cond ((null (cdr value-list)) "") (t (format nil "[~a]" index)))))
                                    (incf index)
                                    (when (typep value 'st-json::jso)
                                      (setf table-rows
                                            (traverse-serp-jso value
                                                               :table-rows table-rows
                                                               :path (format nil "~a~a" path-branch index-str)
                                                               :path-regex path-regex
                                                               :ordered-include-fields ordered-include-fields
                                                               :ordered-error-fields ordered-error-fields
                                                               :exclude-fields exclude-fields)))))))))))
    table-rows)


;; (mapcar 'car (ask-serp "What is the top news headline today?" :top-n 5 :path-regex "top_stories"))
(defun ask-serp (query-phrase &key ((:verbose verbose) nil)
                                ((:path-regex path-regex) ".")
                                ((:timeout timeout) *serp-default-timeout*)
                                ((:top-n top-n) *serp-default-top-n*)
                                ((:ordered-include-fields ordered-include-fields) llm::*serp-ordered-include-fields*)
                                ((:ordered-error-fields ordered-error-fields) '("error"))
                                ((:exclude-fields exclude-fields) llm::*serp-exclude-fields*))
  (let* ((jso (call-serp "search.json"
                         :timeout timeout
                         :query
                         `(("q" . ,query-phrase)
                           ("api_key" . ,*serp-api-key*)
                           ("google_domain" . "google.com"))
                         :verbose verbose))
         (table-rows  (traverse-serp-jso jso
                                         :path-regex path-regex
                                         :ordered-include-fields ordered-include-fields
                                         :ordered-error-fields ordered-error-fields
                                         :exclude-fields exclude-fields)))
    (when top-n (setf table-rows (subseq table-rows 0 (min top-n (length table-rows)))))
    table-rows))


(defun set-serp-api-key (key)
  (setf *serp-api-key* key))


(defun autochat ()
(let ((gpt-says "Are you sentient?")
      (serp-says nil))
  (dotimes (i 10)
    (log-llm "~%GPT: ~a~%" gpt-says)
    (setf serp-says (ask-serp gpt-says))
    (setf serp-says (car (nth (random (length serp-says)) serp-says)))
    (setf serp-says (remove-if (lambda (ch) (> (char-code ch) 127)) serp-says))
    (log-llm "~%SERP: ~a~%" serp-says)
    (setf gpt-says (gpt::ask-chat (format nil "Respond to this statement like a compassionate listener and insightful interviewer: ~a. Be brief.  You have only 15 seconds to reply." serp-says))))
    (setf gpt-says (remove-if (lambda (ch) (> (char-code ch) 127)) gpt-says))
  (log-llm "GPT: ~a~%" gpt-says)    ))
