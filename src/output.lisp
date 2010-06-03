
(in-package #:core-gp)

;;;
;;; output
;;;

(defgeneric output-stats (stats run-best new-best-p output streams)
  (:documentation "Output the stats of a single iteration."))

(defmethod output-stats ((stats fitness-stats) run-best new-best-p output streams)
  "Outputs the computed stats according to the type of output."
  (unless (eql output :none)
    (when (member output '(:screen :screen+files))
      (format t "generation ~a~%raw: ~a ~a ~a ~a ~a ~a~%fit: ~a ~a ~a ~a ~a ~a~%" 
	      (iteration stats) 
	      (run-best-raw-score stats) (best-raw-score stats) (worst-raw-score stats)
	      (mean-raw-score stats) (median-raw-score stats) (deviation-raw-score stats)
	      (run-best-fitness-score stats) (best-fitness-score stats) 
	      (worst-fitness-score stats) (mean-fitness-score stats) 
	      (median-fitness-score stats) (deviation-fitness-score stats)))
    (when (member output '(:files :screen+files))
      (format (first streams) "generation ~a~%raw: ~a ~a ~a ~a ~a ~a~%fit: ~a ~a ~a ~a ~a ~a~%" 
	      (iteration stats) 
	      (run-best-raw-score stats) (best-raw-score stats) (worst-raw-score stats)
	      (mean-raw-score stats) (median-raw-score stats) (deviation-raw-score stats)
	      (run-best-fitness-score stats) (best-fitness-score stats) 
	      (worst-fitness-score stats) (mean-fitness-score stats) 
	      (median-fitness-score stats) (deviation-fitness-score stats))
      (when new-best-p
	(format (second streams) "~a ~%" (list (iteration stats) run-best))))))


(defmethod output-stats ((stats tree-stats) run-best new-best-p output streams)
  "Outputs the computed stats according to the type of output." 
  (unless (eql output :none)
    (call-next-method)
    (when (member output '(:screen :screen+files))
      (format t "depth: ~a ~a ~a ~a ~a~%nodes: ~a ~a ~a ~a ~a~%" 
	      (run-best-depth stats) (best-depth stats) (worst-depth stats)
	      (mean-depth stats) (deviation-depth stats)
	      (run-best-nodes-count stats) (best-nodes-count stats) (worst-nodes-count stats)
	      (mean-nodes-count stats) (deviation-nodes-count stats)))
    (when (member output '(:files :screen+files))
      (format (first streams) "depth: ~a ~a ~a ~a ~a~%nodes: ~a ~a ~a ~a ~a~%" 
	      (run-best-depth stats) (best-depth stats) (worst-depth stats)
	      (mean-depth stats) (deviation-depth stats)
	      (run-best-nodes-count stats) (best-nodes-count stats) (worst-nodes-count stats)
	      (mean-nodes-count stats) (deviation-nodes-count stats)))))
