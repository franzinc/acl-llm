(in-package :llm)





(defun dotproduct (bm1 bm2)
  (declare (optimize (speed 3) (safety 1))
	   (single-float-array bm1 bm2))
  (let ((term1 (make-array 1 :element-type 'single-float :initial-element 0.0)))
    (declare (single-float-array term1))
    (dotimes (i (length bm1))
      #+ignore
      (print (list (aref bm1 i)(aref bm2 i) (* (aref bm1 i)(aref bm2 i))))
      (incf (aref term1 0) (the single-float
			     (* (the single-float (aref bm1 i))
				(the single-float (aref bm2 i))))))
    (aref term1 0)))

(defun cosine-similarity (bm1 bm2)
  (declare (optimize (speed 3) (safety 1))
	   (single-float-array bm1 bm2))
  (let ((term1 (make-array 1 :element-type 'single-float :initial-element 0.0))
	(term2 (make-array 1 :element-type 'single-float :initial-element 0.0))
	(term3 (make-array 1 :element-type 'single-float :initial-element 0.0)))
    (declare (single-float-array term1 term2 term3))
    (dotimes (i (length bm1))
      (incf (aref term1 0) (the single-float
			        (* (the single-float (aref bm1 i))
				   (the single-float (aref bm2 i)))))
      (incf (aref term2 0) (the single-float
			        (* (the single-float (aref bm1 i))
				   (the single-float (aref bm1 i)))))
      (incf (aref term3 0) (the single-float
			        (* (the single-float (aref bm2 i))
				   (the single-float (aref bm2 i)))))
      )
    (/ (aref term1 0)
       (* (sqrt (aref term2 0)) (sqrt (aref term3 0))))))

