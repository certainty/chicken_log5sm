;;
;; Author: David Krentzlin
;;
;; Created: So Aug  8 18:46:54 2010 (CEST)
;; Last-Updated: Fr Nov 19 10:43:54 2010 (CET)
;;           By: David Krentzlin


(use test)

(load "../log5scm-lolevel.scm")
(load "../log5scm.scm")

(import log5scm)
(import log5scm-lolevel)

(test-group "categories"
            (test "Expansion: does work"
                  '(and cat5 (or cat1 cat2))
                  (parameterize ((*defined-categories* (make-hash-table)))
                    (define-category cat3 (or cat1 cat2))
                    (define-category cat4 (and cat5 cat3))
                    (expand-category-spec 'cat4)))
            (test "Expansion: default logical connective"
                  '(and cat5 (and cat1 cat2))
                  (parameterize ((*defined-categories* (make-hash-table))
                                 (default-logical-connective 'and))
                    (define-category cat3 (cat1 cat2))
                    (define-category cat4 (and cat5 cat3))
                    (expand-category-spec 'cat4)))
            (test "Expansion: (identity) does work"
                  'cat1
                  (parameterize ((*defined-categories* (make-hash-table)))
                    (expand-category-spec 'cat1)))


;            (test "Match: Simple" #t #f)
;            (test "Match: Complex" #t #f)
;            (test "Match: Negated" #t #f)


            )

(test-group "sender matching"
     (test "matches one of spec"
           #t
           (sender-matches-spec? '(or app error) '(app)))
     (test "doesn't match if not at least one matches"
           #f
           (sender-matches-spec? '(or app error) '(info)))
     (test "matches all of spec"
           #t
           (sender-matches-spec? '(and app error) '(app error)))
     (test "doesn't match if one doesn't match"
           #f
           (sender-matches-spec? '(and app error) '(app)))
     (test "wildcard match"
           #t
           (sender-matches-spec? '(or *) '(anything))))

(test-group "senders")

(test-group "output")

(test-group "context"
            (test "push context"
                  (list "test")
                  (parameterize ((active-contexts '()))
                    (push-context "test")
                    (active-contexts)))

            (test "pop context"
                  (list)
                  (parameterize ((active-contexts '()))
                    (push-context "test")
                    (pop-context)
                    (active-contexts)))

            (test "access current context"
                  "test"
                  (parameterize ((active-contexts '()))
                    (push-context "test")
                    (current-context)))

            )

(test-group "integration")

(unless (zero? (test-failure-count)) (exit 1))
