(require 'cl)

;;;; Support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro tester:defstruct (name &rest attributes)
  `(tester:define-struct ',name ',attributes))

(defun tester:define-struct (name attributes)
  (tester:define-constructor name attributes)
  (mapc (lambda (attribute)
          (tester:define-getter name attribute)
          (tester:define-setter name attribute)
          (tester:define-adder  name attribute))
        attributes))

(defun tester:define-constructor (name attributes)
  (let ((constructor-name (intern (concat "tester:make-" (symbol-name name))))
        (assignments (mapcar (lambda (attribute)
                               `(puthash ',attribute ,attribute table))
                             attributes)))
    (eval `(defun ,constructor-name (&optional ,@attributes)
             (let ((table (make-hash-table)))
               ,@assignments
               table)))))

(defun tester:define-getter (struct attribute)
  (let ((getter (intern (concat "tester:" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,getter (self)
       (gethash ',attribute self)))))

(defun tester:define-setter (struct attribute)
  (let ((setter (intern (concat "tester:set-" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,setter (self value)
       (puthash ',attribute value self)))))

(defun tester:define-adder (struct attribute)
  (let ((adder (intern (concat "tester:add-to-" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,adder (self value)
       (puthash ',attribute (cons value (gethash ',attribute self)) self)))))

;;;; Test structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tester:defstruct scene name wrappers tests)
(tester:defstruct test scene name function result)

(defvar tester:scenes nil
  "List of all test scenes.")

(defun tester:make-wrapper (args forms)
  (eval `(lambda ,args ,@(tester:expand-run-calls forms))))

(defun tester:expand-run-calls (forms)
  (mapcar (lambda (form)
            (if (listp form)
                (if (equal form '(run))
                    '(funcall run)
                  (tester:expand-run-calls form))
              form))
          forms))

(defmacro scene (name &rest forms)
  `(tester:define-scene ',name ',forms))

(defun tester:define-scene (name forms)
  (let ((scene (tester:make-scene)))
    (tester:set-scene-name scene name)
    (mapc (lambda (form)
            (case (car form)
              ('test
               (let ((test (tester:make-test)) function)
                 (tester:set-test-scene test scene)
                 (tester:set-test-name test (nth 1 form))
                 (setq function (eval `(lambda () ,@(cddr form))))
                 (tester:set-test-function test function)
                 (tester:add-to-scene-tests scene test)))
              ('wrap
               (let ((wrapper (tester:make-wrapper () (cdr form))))
                 (tester:add-to-scene-wrappers scene wrapper)))
              ('setup
               (let ((wrapper (tester:make-wrapper () (append (copy-list (cdr form)) (list '(run))))))
                 (tester:add-to-scene-wrappers scene wrapper)))
              ('teardown
               (let ((wrapper (tester:make-wrapper (cons '(run) (copy-list (cdr form))))))
                 (tester:add-to-scene-wrappers scene wrapper)))))
          forms)
    (add-to-list 'tester:scenes scene)))

(defmacro check (form)
  `(or ,form
       (signal 'tester:failed nil)))

(put 'tester:failed 'error-conditions '(tester:failed error))

;;;; Running ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: since emacs lisp only does dynamic binding, all locals and
;; function parameters in this section need to be prefixed with
;; 'tester:' so they do not affect client code.

(defun tester:run ()
  "Run all tests.

This runs the tests and prints the results to `standard-output'.

To run the tests from a command line, do:

  emacs -batch -l tester.el -l <test-file> ... -f tester:run"
  (mapc (lambda (tester:current-scene)
          (mapc (lambda (tester:current-test)
                  (tester:run-current-test))
                (tester:scene-tests tester:current-scene)))
        tester:scenes))

(defun tester:run-current-test ()
  (tester:run-current-test-with-wrappers
   (tester:test-function tester:current-test)
   (reverse (tester:scene-wrappers tester:current-scene)))
  (unless (tester:test-result tester:current-test)
    (tester:test-passed)))

(defun tester:run-current-test-with-wrappers (run tester:wrappers)
  (if (null tester:wrappers)
      (tester:call-safely run)
    (let* ((tester:outer-run run)
           (run (lambda () (tester:run-current-test-with-wrappers
                            tester:outer-run (cdr tester:wrappers)))))
      (tester:call-safely (car tester:wrappers)))))

(defun tester:call-safely (function)
    (condition-case e
        (funcall function)
      (tester:failed
       (unless (tester:test-result tester:current-test)
         (tester:test-failed)))
      (error
       (unless (tester:test-result tester:current-test)
         (tester:test-erred)))))

(defun tester:test-passed ()
  (tester:set-test-result tester:current-test 'pass)
  (message "."))

(defun tester:test-failed ()
  (tester:set-test-result tester:current-test 'fail)
  (message "F"))

(defun tester:test-erred ()
  (tester:set-test-result tester:current-test 'error)
  (message "E"))

;;;; Provide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tester)

;;;; Example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(scene "name of scene"
       (wrap (run))
       (wrap (run))
       (test "name of test" (check t))
       (test "name of other test" (check nil)))

;; To run:
;;   $ emacs -Q -batch -l tester.el -f tester:run
