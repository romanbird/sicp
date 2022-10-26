;;; MCE with some interesting stuff traced
;;;
;;; Requires trace.scm and mceval.scm
;;;

(load "trace.scm")
(load "mceval.scm")

(trace mc-eval mpp mpp)
(trace mc-apply mpp mpp)
(trace eval-if mpp mpp)
(trace eval-assignment mpp mpp)
(trace eval-definition mpp mpp)
(trace make-lambda)
(trace cond->if)
(trace make-procedure mpp mpp)
(trace extend-environment mpp mpp)
(trace lookup-variable-value mpp mpp)
(trace set-variable-value! mpp mpp)
(trace define-variable! mpp mpp)
