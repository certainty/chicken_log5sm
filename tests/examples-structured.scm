(use log5scm syslog)

(define-category struct)

;;structured logging
(start-sender struct-sender (structured-sender (lambda (msg) (print msg))) (output (<structured)) (category struct))

(log-for (struct) '((foo . bar) (bar . baz)))
