;; See the file "LICENSE" for the full license governing this code.;;

(eval-when (compile load eval)
  (require :datetime)
  (require :aserve)
  (require :st-json)
  (defpackage :llm 
    (:use :cl :cl-user :excl :st-json :prof :util.date-time
          :net.aserve :net.html.generator)))
    
(let* ((llm-package (find-package "llm"))
       (packages-to-use (cons llm-package
			      (package-use-list llm-package))))
  "This idiom lets the user use any symbol we use"  
  (defpackage :llm-user)
  (use-package packages-to-use (find-package :llm-user))

  (defpackage :llm.gpt (:use :llm-user :st-json)))

(let* ((gpt-package (find-package "llm.gpt"))
       (packages-to-use (cons gpt-package
			      (package-use-list gpt-package))))
  (defpackage :llm.gpt-user
    (:nicknames :gpt))
  (use-package packages-to-use (find-package :llm.gpt-user)))

(load (compile-file "openai.cl"))
