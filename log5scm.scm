;; Description:
;; A logging library based on the ideas of CL's log5 http://common-lisp.net/project/log5
;;
;; Author: David Krentzlin <david@lisp-unleashed.de>
;; Maintainer: 
;; Created: Di Dez  1 08:17:16 2009 (CET)
;; Last-Updated: Di Dez 14 21:11:42 2010 (CET)
;;           By: David Krentzlin
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
  (*default-logical-connective*
   *categories*
   *senders*
   *outputs*
   define-category
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
   *current-contexts*
   current-context
   push-context
   pop-context
   call-with-context
   with-context
   log-for)
   
  (import scheme chicken extras)
  (require-library defstruct srfi-69 srfi-1 srfi-13 syslog)
  (import syslog)
  (import defstruct)
  (import srfi-69)
  (import (only data-structures identity atom?))
  (import (only srfi-13 string-join))
  (import (only srfi-1 any every))


  ;; 1) Categories
  ;; Categories are just a way to organize your logmessages. You may
  ;; arrange as many and as complex categories as you wish. They're,
  ;; as the name suggests, a way to express buckets for
  ;; log-messages. Those buckets may later be bound to senders and thus
  ;; enable the program to put messages at the right places.
  (define (logical-connective? x)
    (member x '(and not or)))
  
  ;;by default all categories are or'ed together  
  (define *default-logical-connective* (make-parameter 'or))

  ;;we need to store defined categories for late use
  ;;NOTE: all categories inside this container are already expanded
  (define *categories* (make-parameter (make-hash-table)))

  (define (name->category name)
    (hash-table-ref/default (*categories*) name #f))
  
  ;; Find the category with the given name and expand it
  ;; if needed
  (define (expand-category name)
    (let ((spec (name->category name)))
      (if spec  (if (list? spec) (expand-category-spec spec) spec) name)))

  ;; Expansion is straight forward.
  ;; Any occurence of a mere name is replaced by its expanded form.
  ;; This is recursivly applied until the entire category is expanded
  ;; Example:
  ;; (define-category controller)
  ;; (define-category model)
  ;; (define-category app (or controller model))
  ;; (define-category foo (not app (or app)))
  ;;
  ;; (expand-category-spec '(not app (or controller))) #=> (not (or controller model) (or controller))
  (define (expand-category-spec spec)
    (cond
     ((null? spec) '())
     ((atom? spec) (expand-category spec))
     ((list? spec)
      `(,@(if (logical-connective? (car spec))
              `(,(car spec) ,@(map expand-category-spec (cdr spec)))
              `(,(*default-logical-connective*) ,@(map expand-category-spec spec)))))))
  
  ;; Simple syntax to add categories to our categories-container
  ;; It allows basically two forms: simple and complex
  ;; Simple categories are just a symbol ( a name)
  ;; Complex categories are logically connected categories
  (define-syntax define-category
    (syntax-rules ()
      ((_ name)
       (hash-table-set! (*categories*) (quote name) (quote name)))
      ((_ name (spec more-spec ...))
       (hash-table-set! (*categories*) (quote name) (expand-category-spec (quote (spec more-spec ...)))))))
  
  ;;print a list of all currently defined categories to standard-output
  (define (dump-categories)
    (hash-table-map (*categories*) (lambda (k v) (sprintf "~A -> ~A" k v))))
  

 (define (determine-variables spec)
   (let ((positive '()) (negative '()))
     (define (walk spec)
       (cond
        ((null? spec) #t)
        ((atom? spec) (unless (logical-connective? spec)
                        (set! positive (cons spec positive))))
        ((eq? (car spec) 'not)
         (set! negative (cons (cadr spec) negative)))
        (else
         (walk (car spec))
         (walk (cdr spec)))))
     (walk spec)
     (values positive negative)))


 ;;does the sender-spec match the cat-spec?
 ;;sender and cat-spec should both be expanded
 (define (sender-matches-spec?  sender-spec cat-spec)
   (receive (pos neg) (determine-variables cat-spec)
     (and (category-spec-matches? pos sender-spec) (not (category-spec-matches? neg sender-spec )))))

 ;;We determine if the current specification of the sender matches the
 ;;category.
 ;;We simply decide if we shall use this sender to send the message
 (define (category-spec-matches? cat spec) 
   (define (bool-walk spec)
     (cond
      ((null? spec) #f)
      ((atom? spec) (list? (member spec cat)))
      ((list? spec)
       (case (car spec)
         ((or) (any identity (map bool-walk (cdr spec))))
         ((and) (every identity (map bool-walk (cdr spec))))
         ((not) (not (every identity (map bool-walk (cdr spec)))))
         (else (map bool-walk spec))))))
   (bool-walk spec))


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
 (define *senders* (make-parameter (make-hash-table)))
 (defstruct sender name output-spec category-spec handler)
 
 (define (add-sender sender)
   (let ((cats (sender-category-spec sender))
         (outputs (sender-output-spec sender)))
     (when (and cats (not (list? cats)))
       (sender-category-spec-set! sender (list cats)))
     (when (and outputs (not (list? outputs)))
       (sender-output-spec-set! sender (list outputs)))
     (hash-table-set! (*senders*) (sender-name sender) sender)))

 ;;apply proc to all senders that match the given categories
 (define (matching-senders-for-each proc category-spec)
   (let ((exp-cat-spec (expand-category-spec category-spec)))
     (hash-table-for-each (*senders*)
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
 ;; (start-sender name (sender-ctor) (category-spec) (output-spec))
 ;; (start-sender name (sender-ctor) (category cat-spec))
 ;; (start-sender name (sender-ctor) (category cat-spec) (output output-spec))
 (define-syntax start-sender
   (syntax-rules (output category)
     ((_ name (sender-type arg1 ...))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...))))
     ((_ name (sender-type arg1 ...) (category cat-spec))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...) category-spec: (quote  cat-spec))))
     ((_ name (sender-type arg1 ...) (output output-spec))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...) output-spec: (quote output-spec))))
     ((_ name (sender-type arg1 ...) (output output-spec) (category cat-spec))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...) category-spec: (quote cat-spec) output-spec: (quote output-spec))))
     ((_ name (sender-type arg1 ...) (category cat-spec) (output output-spec))
      (add-sender (make-sender name: (quote name) handler: (sender-type arg1 ...) category-spec: (quote  cat-spec) output-spec: (quote output-spec))))))
 
 ;;dump currently defined senders to current-output-port
 (define (dump-senders)
   (hash-table-map (*senders*) (lambda (k v) (sprintf "~A -> ~A | ~A " k (sender-category-spec v) (sender-output-spec v)))))


 ;; 3) outputs
 ;; Outputs are just a way to format your message
 ;; Technically an output is a procedure that returns a string
 ;; Multiple outputs can be combined into a list of outputs that are
 ;; processed in order when a message is generated

;; we need to remember some things about the current environment 
(define *current-message*  (make-parameter ""))
(define *current-category* (make-parameter #f))



 ;; as with senders and categories, outputs are stored in a hashtable
 ;; so that we can reference them by name
 (define *outputs* (make-parameter (make-hash-table)))
 
 (define (add-output name proc)
   (hash-table-set! (*outputs*) name proc))

 ;; a simple way to define outputs is provided
 ;; by the define-output syntax
 (define-syntax define-output
   (syntax-rules ()
     ((_ name body more-body ...)
      (add-output (quote name) (lambda () (begin body more-body ...))))))


 ;; the following are standard outputters
(define-output message (*current-message*))
(define-output category (sprintf "~A" (*current-category*)))
(define-output context (let ((ctx (current-context)))
                         (if ctx (sprintf "~A > " ctx) "")))


 ;; by default we output the category followed by the message
 (define *default-output-spec* (make-parameter '(context category message)))


 ;; contexts
(define *current-contexts* (make-parameter '()))
(define (current-context)
  (if (and (list? (*current-contexts*)) (not (null? (*current-contexts*))))
      (car (*current-contexts*))
      #f))

(define (push-context context)
  (*current-contexts* (cons context (*current-contexts*))))

(define (pop-context)
  (if (and (list? (*current-contexts*)) (not (null? (*current-contexts*))))
      (*current-contexts* (cdr (*current-contexts*)))))

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
   (parameterize ((*current-message* (apply sprintf fmt args))
                  (*current-category* (string-join (map symbol->string category-spec) "::")))
     (matching-senders-for-each
      (lambda (name sender)
        (let ((outputs (map (lambda (o)
                              ((hash-table-ref/default (*outputs*) o (lambda () ""))))
                            (or (sender-output-spec sender) (*default-output-spec*)))))
          ((sender-handler sender)
           (string-join outputs " ")))) category-spec)))

 ;; Finally we can define our logging macro
 (define-syntax log-for
   (syntax-rules ()
     ((_ (extended more ...) fmt args ...)
      (find-and-apply-senders (quote (extended more ...)) fmt args ...))
     ((_ simple-spec fmt args ...)
      (find-and-apply-senders (quote (simple-spec)) fmt args ...))))

 )
