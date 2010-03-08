;; Minimal Scheme unit-test framework
;; Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; Example of usage
;; ----------------
;;
;; The following program:
;;
;;   (use-modules (unit-test))
;;   (begin-test 'SuccessfulTest
;;     (assert-true #t)
;;     (assert-equal 1 1))
;;   (begin-test 'FailTest
;;     (assert-equal #t "string"))
;;   (report-tests)
;;
;; Produces the output:
;;
;;   SuccessfulTest... passed
;;   FailTest... failed
;;     assert-equal: expected: #t got: "string"
;;   Test summary
;;   Passed: 1
;;   Failed: 1
;;

(define-module (unit-test)
  #:use-module (ice-9 pretty-print)
  #:use-syntax (ice-9 syncase)
  #:export (assert-true
            assert-equal
            tests-passed?
            report-tests
            %begin-test
            run-tests-on-command-line)
  #:export-syntax (begin-test))

(define *failed-tests* '())
(define *passed-tests* '())

(define (assert-true result)
  (if result
      #t
      (throw 'test-failed-exception
             (with-output-to-string
              (lambda ()
                (display "  assert-true: ")
                (display "got: ")
                (write result))))))

(define (assert-equal expected result)
  (if (equal? expected result)
      #t
      (throw 'test-failed-exception
             (with-output-to-string
              (lambda ()
                (display "  assert-equal: expected: ")
                (write expected)
                (display " got: ")
                (write result))))))

(define (%begin-test name test-thunk)
  (let ((test-success #t)
        (test-fail-msg #f))
    (display name) (display "... ")

    (catch #t test-thunk
           (lambda (key . args)
             (set! test-success #f)
             (set! test-fail-msg
             (if (eqv? key 'test-failed-exception)
                 (car args)
                 (with-output-to-string
                  (lambda ()
                    (display "  unexpected exception: ")
                    (write (cons key args))))))))

    (if test-success
        (begin
          (display "passed")
          (set! *passed-tests* (cons name *passed-tests*)))
        (begin
          (display "failed")
          (if test-fail-msg
              (begin
                (newline)
                (display test-fail-msg)))
          (set! *failed-tests* (cons name *failed-tests*))))
    (newline)))

(define-syntax begin-test
    (syntax-rules ()
      ((_ name . test-forms)
       (%begin-test name (lambda () . test-forms)))))

(define (tests-passed?) (null? *failed-tests*))

(define (report-tests)
  (display "Test summary")(newline)
  (display "Passed: ") (display (length *passed-tests*)) (newline)
  (display "Failed: ") (display (length *failed-tests*)) (newline))

(define (run-tests-on-command-line)
  (let lp ((lst (cdr (command-line))))
    (if (null? lst)
        (begin
          (report-tests)
          (exit (if (tests-passed?) 0 1)))
        (begin
          (load (car lst))
          (lp (cdr lst))))))
