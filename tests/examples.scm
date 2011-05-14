;; 
;; Author: David Krentzlin
;; 
;; Created: Mi Aug 25 21:57:42 2010 (CEST)
;; Last-Updated: Mi Aug 25 22:02:47 2010 (CEST)
;;           By:

(use log5scm syslog)

(define-category info)
(define-category debug)
(define-category warn)
(define-category warn+ (or info debug warn))

(define-category controller)
(define-category model)
(define-category app (or controller model))

(define-output model-out "model-out")
(define-output controller-out "controller-out")
(define-output app-out "app-out")

;;start port senders
(start-sender model-sender (port-sender (current-error-port)) (output (model-out message)) (category model))
(start-sender controller-sender (port-sender (current-error-port)) (output (controller-out message)) (category controller))
(start-sender app-sender (port-sender (current-error-port))   (category app))

(start-sender lazy-port-sender (port-sender "test2.log" lazy: #t) (category model))

;;additionally send every warning to syslog
(start-sender syslog-warn (syslog-sender "test" opt/pid facility/local0 prio/warning) (category warn))

(with-context "Testcontext"
 (log-for (model warn) "just a ~A" "Test")
 (with-context "Nested Context" 
   (log-for (app) "nother Test"))
 (log-for (app) "Final test"))

