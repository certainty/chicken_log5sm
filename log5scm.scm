;; Description:
;; A logging library based on the ideas of CL's log5 http://common-lisp.net/project/log5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (c) <2010> David Krentzlin <david@lisp-unleashed.de>
;;
;;   Permission is hereby granted, free of charge, to any person
;;   obtaining a copy of this software and associated documentation
;;   files (the "Software"), to deal in the Software without
;;   restriction, including without limitation the rights to use,
;;   copy, modify, merge, publish, distribute, sublicense, and/or sell
;;   copies of the Software, and to permit persons to whom the
;;   Software is furnished to do so, subject to the following
;;   conditions:
;;
;;   The above copyright notice and this permission notice shall be
;;   included in all copies or substantial portions of the Software.
;;
;;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;   OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;   OTHER DEALINGS IN THE SOFTWARE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(module log5scm
  (default-logical-connective
   *defined-categories*
   *defined-senders*
   *defined-outputs*
   current-category
   current-message
   define-category
   default-output-format
   dump-categories
   add-sender
   start-sender
   expand-category-spec
   port-sender
   syslog-sender
   dump-senders
   make-sender
   add-output
   define-output
   find-and-apply-senders
   active-contexts
   current-context
   push-context
   pop-context
   call-with-context
   with-context
   log-for)


  (import scheme chicken extras)
  (require-library defstruct srfi-69 srfi-1 srfi-13 syslog log5scm-lolevel)
  (import syslog)
  (import defstruct)
  (import srfi-69)
  (import (only data-structures identity atom?))
  (import (only srfi-13 string-join))
  (import (only srfi-1 any every))

  (begin-for-syntax (require-extension log5scm-lolevel))



;; Simple syntax to add categories to our categories-container
;; It allows basically two forms: simple and complex
;; Simple categories are just a symbol ( a name)
;; Complex categories are logically connected categories
(define-syntax define-category
  (syntax-rules ()
    ((_ name)
     (hash-table-set! (*defined-categories*) (quote name) (quote name)))
    ((_ name (spec more-spec ...))
     (hash-table-set! (*defined-categories*) (quote name) (expand-category-spec (quote (spec more-spec ...)))))))


;;print a list of all currently defined categories to standard-output
(define (dump-categories)
  (hash-table-map (*defined-categories*) (lambda (k v) (sprintf "~A -> ~A" k v))))


 ;; 2) Senders
 ;; Senders are basically filtered sinks for messages. Any message
 ;; that comes in will be analyzed against the category-specification
 ;; and only if a match is found the message  send to its destination.
 ;; Furthermore senders decide where the output goes. The most
 ;; commonly used senders will be port-senders. But you could as well
 ;; send messages to syslog or to a port or via email or
 ;; whatever. You're free to define a custom sender-type and use it for
 ;; logging

 ;; Again senders are assiciated with an eq' hash-table
 (define *defined-senders* (make-parameter (make-hash-table)))
 (defstruct sender name output-format category-spec handler)

 (define (add-sender sender)
   (let ((cats (sender-category-spec sender))
         (outputs (sender-output-format sender)))
     (when (and cats (not (list? cats)))
       (sender-category-spec-set! sender (list cats)))
     (when (and outputs (not (list? outputs)))
       (sender-output-format-set! sender (list outputs)))
     (hash-table-set! (*defined-senders*) (sender-name sender) sender)))

 ;;apply proc to all senders that match the given categories
 (define (matching-senders-for-each proc category-spec)
   (let ((exp-cat-spec (expand-category-spec category-spec)))
     (hash-table-for-each (*defined-senders*)
                          (lambda (name sender)
                            (if (sender-matches-spec? (expand-category-spec (sender-category-spec sender)) exp-cat-spec)
                                (proc name sender))))))

 ;; a sender-type is a lambda that returns a procedure that accepts a
 ;; single argument, the message.
 ;; See the predefined sender for an example
 ;; This is the probably most often used sender
 ;; a simple port sender.
 (define (port-sender port-or-path #!key (lazy #f))
   (if lazy
       (let ((port #f))
         (lambda (message)
           (if (or (not port) (port-closed? port))
             (set! port (if (port? port-or-path) port-or-path (open-output-file port-or-path #:append))))
           (fprintf port "~A~%" message)))
       (let ((port (if (port? port-or-path) port-or-path (open-output-file port-or-path #:append))))
         (lambda (message)
           (fprintf port "~A~%" message)))))

 (define (syslog-sender ident options facility prio)
   (lambda (msg)
     (openlog ident options facility)
     (syslog prio msg)
     (closelog)))

 ;; To stard a sender we provide a syntax that looks like this
 ;; (start-sender name (sender-ctor) (category-spec) (output-format))
 ;; (start-sender name (sender-ctor) (category cat-spec))
 ;; (start-sender name (sender-ctor) (category cat-spec) (output output-format))
 (define-syntax start-sender
   (syntax-rules (output category)
     ((_ name (sender-type arg1 ...))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...))))
     ((_ name (sender-type arg1 ...) (category cat-spec))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...) category-spec: (quote  cat-spec))))
     ((_ name (sender-type arg1 ...) (output output-format))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...) output-format: (quote output-format))))
     ((_ name (sender-type arg1 ...) (output output-format) (category cat-spec))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...) category-spec: (quote cat-spec) output-format: (quote output-format))))
     ((_ name (sender-type arg1 ...) (category cat-spec) (output output-format))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...) category-spec: (quote  cat-spec) output-format: (quote output-format))))))

 ;;dump currently defined senders to current-output-port
 (define (dump-senders)
   (hash-table-map (*defined-senders*) (lambda (k v) (sprintf "~A -> ~A | ~A " k (sender-category-spec v) (sender-output-format v)))))


 ;; 3) outputs
 ;; Outputs are just a way to format your message
 ;; Technically an output is a procedure that returns a string
 ;; Multiple outputs can be combined into a list of outputs that are
 ;; processed in order when a message is generated

;; we need to remember some things about the current environment
(define current-message  (make-parameter ""))
(define current-category (make-parameter #f))



 ;; as with senders and categories, outputs are stored in a hashtable
 ;; so that we can reference them by name
 (define *defined-outputs* (make-parameter (make-hash-table)))

 (define (add-output name proc)
   (hash-table-set! (*defined-outputs*) name proc))

 ;; a simple way to define outputs is provided
 ;; by the define-output syntax
 (define-syntax define-output
   (syntax-rules ()
     ((_ name body more-body ...)
      (add-output (quote name) (lambda () (begin body more-body ...))))))


 ;; the following are standard outputters
(define-output <message (current-message))
(define-output <category (sprintf "~A" (current-category)))
(define-output <context (let ((ctx (current-context)))
                         (if ctx (sprintf "~A > " ctx) "")))



 ;; by default we output the category followed by the message
(define default-output-format (make-parameter '(<context <category <message)))


 ;; contexts
(define active-contexts (make-parameter '()))
(define (current-context)
  (if (and (list? (active-contexts)) (not (null? (active-contexts))))
      (car (active-contexts))
      #f))

(define (push-context context)
  (active-contexts (cons context (active-contexts))))

(define (pop-context)
  (if (and (list? (active-contexts)) (not (null? (active-contexts))))
      (active-contexts (cdr (active-contexts)))))

(define (call-with-context context thunk)
  (dynamic-wind
      (lambda () (push-context context))
      thunk
      (lambda () (pop-context))))

(define-syntax with-context
  (syntax-rules ()
    ((_ context body more-body ...)
     (call-with-context context (lambda () body more-body ...)))))


 ;; this is the heart of the framework. It tries to determine any
 ;; senders that match the given category-spec and applies the
 ;; sender's handler to the passed message
 (define (find-and-apply-senders category-spec fmt . args)
   (parameterize ((current-message (apply sprintf fmt args))
                  (current-category (string-join (map symbol->string category-spec) "::")))
     (matching-senders-for-each
      (lambda (name sender)
        (let ((outputs (map (lambda (o)
                              ((hash-table-ref/default (*defined-outputs*) o (lambda () ""))))
                            (or (sender-output-format sender) (default-output-format)))))
          ((sender-handler sender)
           (string-join outputs " ")))) category-spec)))


 ;; Finally we can define our logging macro
 (define-syntax log-for
   (ir-macro-transformer
    (lambda (expression inject compare)
      (let* ((spec (second expression))
             (spec (if (list? spec) spec (list spec))))
        (if (or (not *ignore-category-spec*)
                (sender-matches-spec? *ignore-category-spec* (expand-category-spec (strip-syntax spec))))
            `(find-and-apply-senders ',spec . ,(cddr expression))
            '(void))))))
 )
