;;;; See the file LICENSE for the full license governing this code.
(defpackage #:acl-llm.protocol
  (:use #:lift))

(in-package #:acl-llm.protocol)

(lift:deftestsuite llm-tests ()
  ())

(lift:deftestsuite llm-vendor-tests (llm-tests)
  ()
  (:documentation "Test different LLM vendors e.g. OpenAI, Ollama etc."))

(defun ensure-same-jso (lhs rhs)
  (ensure-same (st-json:jso-normalize lhs)
               (st-json:jso-normalize rhs)
               :test 'equalp))
