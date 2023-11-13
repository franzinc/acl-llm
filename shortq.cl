
;;; Small-size priority queue of fixed lengt top-N:
;;; Useful for finding the "Top N" elements in a set.
;;; Items added to the queue when value >= min-score
;;; until the queue reaches the size "top".
;;; Then items added only when value >= smallest value in queue,
;;; replacing the smallest value item.
;;; Maintain the queue length of top-N.

;;; The items in the queue are lists.  The accessor function
;;; determines which element of the list is the score.

(in-package :llm)


(defstruct shortq            
           (item-pq nil :type list) 
           (accessor 'cadr :type function)
           (min-score 95 :type number)
           (top 4 :type integer))


(defun init-shortq (lb tp ac)
  "create and initialize a shortq with 'lower bound' lb, 'top' tp an 'acccessor' ac"
    (make-shortq :item-pq nil :min-score lb :top tp :accessor ac))


(defun print-shortq (shortq)
  "Min score [smallest - largest] size: (list of 'caddr)"
    (log-llm "~a [~a - ~a] ~a:~S~%"
            (shortq-min-score shortq)
            (threshold-shortq-value shortq)
            (largest-shortq-value shortq)
            (length (shortq-item-pq shortq))
            (mapcar 'caddr (shortq-item-pq shortq))))

(defun threshold-shortq-value (shortq)
  "The threshold for entry into the queue is
  the smallest value in shortq or min-score when length < top"
  (let ((item-shortq (shortq-item-pq shortq))
        (min-score (shortq-min-score shortq))
        (top (shortq-top shortq))
        (accessor (shortq-accessor shortq)))
    (cond ((< (length item-shortq) top) min-score)
          (t (funcall accessor (car (last item-shortq)))))))

(defun largest-shortq-value (shortq)
  "largest value in shortq or 0.0"
  (let ((item-shortq (shortq-item-pq shortq))
        (accessor (shortq-accessor shortq)))
    (cond ((null item-shortq) 0.0)
          (t (funcall accessor (car item-shortq))))))

(defun insert-shortq-if (shortq item)
  (let ((top (shortq-top shortq))        
        (accessor (shortq-accessor shortq)))
    (when (>= (funcall accessor item) (threshold-shortq-value shortq))
      (setf (shortq-item-pq shortq) (insert-shortq accessor item (shortq-item-pq shortq)))
      (when (> (length (shortq-item-pq shortq)) top)
        (setf (shortq-item-pq shortq) (subseq (shortq-item-pq shortq) 0 top)))
      (assert (<= (length (shortq-item-pq shortq)) top))
      shortq)))

(defun insert-shortq (accessor item item-pq)
  (if (null item-pq)
      (list item)
    (if (>= (funcall accessor item) (funcall accessor (car item-pq)))
        (cons item item-pq) 
      (cons (car item-pq) (insert-shortq  accessor item (cdr item-pq))))))

(defun test-shortq ()
  (let ((*shortq* (init-shortq 90 4 'cadr)))
    (progn 
      (dotimes (i 100) (insert-shortq-if *shortq* (list (gentemp "id-")
                                                        (random 100)
                                                        (format nil "item-~a" i))))
      (print-shortq *shortq*))))


