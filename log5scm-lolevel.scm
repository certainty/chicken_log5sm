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


(module log5scm-lolevel
  (default-logical-connective *defined-categories* *ignore-category-spec* expand-category-spec sender-matches-spec?)

(import chicken scheme)
(use extras data-structures ports srfi-1 srfi-69)

;; 1) Categories
;; Categories are just a way to organize your logmessages. You may
;; arrange as many and as complex categories as you wish. They're,
;; as the name suggests, a way to express buckets for
;; log-messages. Those buckets may later be bound to senders and thus
;; enable the program to put messages at the right places.

;;by default all categories are or'ed together
(define default-logical-connective (make-parameter 'or))

;;we need to store defined categories for late use
;;NOTE: all categories inside this container are already expanded
(define *defined-categories* (make-parameter (make-hash-table)))

;; This variable can be set to a category spec that makes log-for
;; calls expand into (void) when it matches.
(define *ignore-category-spec*
  (let ((spec (get-environment-variable "LOG5SCM_IGNORE_CATEGORIES")))
    (and spec (with-input-from-string spec read))))

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
            `(,(default-logical-connective) ,@(map expand-category-spec spec)))))))

(define (expand-category name)
  (let ((spec (name->category name)))
    (if spec  (if (list? spec) (expand-category-spec spec) spec) name)))

(define (name->category name)
  (hash-table-ref/default (*defined-categories*) name #f))

(define (logical-connective? x)
  (member x '(and not or)))

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

(define (sender-matches-spec?  sender-spec cat-spec)
  (receive (pos neg) (determine-variables cat-spec)
    (and (category-spec-matches? pos sender-spec)
         (or (null? neg)
             (not (category-spec-matches? neg sender-spec))))))

(define (category-spec-matches? cat spec)
  (define (bool-walk spec)
    (cond
     ((null? spec)  #f)
     ((eq? spec '*) #t)
     ((atom? spec) (list? (member spec cat)))
     ((list? spec)
      (case (car spec)
        ((or)
         (any identity (map bool-walk (cdr spec))))
        ((and) (every identity (map bool-walk (cdr spec))))
        ((not) (not (every identity (map bool-walk (cdr spec)))))
        (else (map bool-walk spec))))))
  (bool-walk spec))

)
