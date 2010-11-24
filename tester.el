(require 'cl)

;;;; Compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'replace-in-string)
    ;; XEmacs
    (defalias 'tester:replace-in-string 'replace-in-string)
  ;; GNU Emacs
  (defun tester:replace-in-string (target old new &optional literal)
    "Replace all matches in STR for REGEXP with NEWTEXT string, 
 and returns the new string."
    (replace-regexp-in-string old new target nil literal)))

(if (functionp 'region-exists-p)
    ;; XEmacs
    (defalias 'tester:region-exists-p 'region-exists-p)
  ;; GNU Emacs
  (defun tester:region-exists-p ()
    "Return non-nil iff the region is highlighted."
    (if transient-mark-mode
        mark-active
      (condition-case e
          (mark)
        (mark-inactive)))))

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
    (eval `(defun ,adder (self value &optional append)
             (let ((list (gethash ',attribute self)))
               (add-to-list 'list value append)
               (puthash ',attribute list self))))))

(defun tester:hash-table-keys (table)
  (let (keys)
    (maphash (lambda (key value)
               (setq keys (cons key keys)))
             table)
    keys))

;;;; Test structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tester:defstruct scene parent name wrappers tests strings buffers)
(tester:defstruct test scene name function result error-data)
(tester:defstruct buffer string point markers)

(defun tester:scene-full-wrapper-list (scene)
  (let ((wrappers ()))
    (while scene
      (setq wrappers (append (tester:scene-wrappers scene) wrappers))
      (when (> (hash-table-count (tester:scene-strings scene)) 0)
        (setq wrappers (cons (tester:scene-strings-wrapper scene) wrappers)))
      (setq scene (tester:scene-parent scene)))
    wrappers))

(defun tester:scene-strings-wrapper (scene)
  (let* (bindings)
    (maphash (lambda (name value)
               (add-to-list 'bindings (list name value)))
             (tester:scene-strings scene))
    (tester:make-wrapper `((let ,bindings (run))))))

(defvar tester:scenes nil
  "List of all test scenes.")

(defun tester:make-wrapper (forms)
  (eval `(lambda () ,@(tester:expand-run-calls forms))))

(defun tester:expand-run-calls (forms)
  (mapcar (lambda (form)
            (if (listp form)
                (if (equal form '(run))
                    '(funcall run)
                  (tester:expand-run-calls form))
              form))
          forms))

(defmacro scene (name &rest forms)
  `(tester:define-scene nil ',name ',forms))

(defun tester:define-scene (parent name forms)
  (let ((scene (tester:make-scene))
        subscenes)
    (tester:set-scene-parent scene parent)
    (tester:set-scene-name scene name)
    (tester:set-scene-strings scene (make-hash-table))
    (tester:set-scene-buffers scene (make-hash-table))
    (mapc (lambda (form)
            (case (car form)
              ('test
               (let ((test (tester:make-test)) function)
                 (tester:set-test-scene test scene)
                 (tester:set-test-name test (nth 1 form))
                 (setq function (eval `(lambda () ,@(cddr form))))
                 (tester:set-test-function test function)
                 (tester:add-to-scene-tests scene test t)))
              ('scene
               (setq subscenes (cons (cdr form) subscenes)))
              ('wrap
               (let ((wrapper (tester:make-wrapper (cdr form))))
                 (tester:add-to-scene-wrappers scene wrapper t)))
              ('strings
               (tester:set-scene-strings scene (tester:parse-strings (nth 1 form) (nth 2 form))))
              ('buffers
               (tester:set-scene-buffers scene (tester:parse-buffers (nth 1 form) (nth 2 form))))
              ('before
               (let ((wrapper (tester:make-wrapper (append (copy-list (cdr form)) (list '(run))))))
                 (tester:add-to-scene-wrappers scene wrapper t)))
              ('after
               (let ((wrapper (tester:make-wrapper (cons '(run) (copy-list (cdr form))))))
                 (tester:add-to-scene-wrappers scene wrapper t)))))
          forms)
    ;; Define nested scenes after this scene is constructed.
    (loop for spec in subscenes do
          (tester:define-scene scene (car spec) (cdr spec)))
    (add-to-list 'tester:scenes scene t)))

(defun tester:parse-strings (spec token)
  (or token
      (setq token "=="))
  (let ((table (make-hash-table)))
    (with-temp-buffer
      (insert spec)
      (goto-char (point-min))

      ;; find first token
      (tester:skip-whitespace)
      (or (string= token (buffer-substring (point) (+ (point) (length token))))
          (signal 'invalid-buffer-spec (concat "spec must start with `" token "'")))
      (forward-char (length token))

      ;; scan the rest
      (while (not (eobp))
        (let (name value)
          (tester:skip-whitespace)
          (setq name (buffer-substring (point) (point-at-eol)))
          (if (eq (forward-line) 0)
              (let (start end)
                (setq start (point))
                (if (search-forward-regexp (concat "^" (regexp-quote token)) nil 'or-goto-eob)
                    (setq end (match-beginning 0))
                  (or (eq ?\n (char-before (point)))
                      (insert "\n"))
                  (setq end (point)))
                (setq value (buffer-substring start end)))
            (setq value "\n"))
          (puthash (intern name) value table)))
      table)))

(defun tester:parse-buffers (spec token)
  (let ((table (tester:parse-strings spec token)))
    (maphash (lambda (name value)
               (puthash name (tester:parse-buffer value) table))
             table)
    table))

(defun tester:parse-buffer (spec)
  (let ((buffer (tester:make-buffer)))
    (tester:set-buffer-markers buffer (make-hash-table))
    (with-temp-buffer
      (insert spec)
      (goto-char (point-min))
      (while (search-forward-regexp "-\\(!\\|<\\(.*?\\)>\\)-" nil t)
        (cond
         ((string= (match-string 1) "!")
          (delete-region (match-beginning 0) (match-end 0))
          (tester:set-buffer-point buffer (point)))
         (t
          (let ((name (intern (match-string 2))))
            (delete-region (match-beginning 0) (match-end 0))
            (puthash name (point) (tester:buffer-markers buffer))))))
      (tester:set-buffer-string buffer (buffer-string)))
    buffer))

(defun tester:skip-whitespace ()
  (skip-chars-forward " \t\r\n\f\v"))

;;;; Test library

(defmacro check (form)
  `(or ,form
       (signal 'test-failed '(,form))))

(put 'test-failed 'error-conditions '(test-failed error))
(put 'invalid-buffer-spec 'error-conditions '(invalid-buffer-spec error))

(defun tester:select (&optional start end)
  "Highlight the region between START and END in the selected buffer.

If START is nil, it defaults to (point-min); if END is nil, it
defaults to (point-max).

If either START or END are less than or equal to zero, it means
that many places from the end of the buffer.  For example, 0
means (point-max), and -1 means (1- (point-max)).  (Recall that
the beginning of the buffer is position 1.)"
  (when (null start)
    (setq start (point-min)))
  (when (null end)
    (setq end (point-max)))
  (when (<= start 0)
    (incf start (point-max)))
  (when (<= end 0)
    (incf end (point-max)))
  (set-mark start)
  (goto-char end))

(defmacro in-buffer (name &rest forms)
  "Run the given forms in the named buffer.

See documentation for `scene' (in particular, the `buffers'
clause) for information on named buffers.

A fresh buffer is created each time this form is used."
  (let* ((buffer (or (tester:find-named-buffer tester:current-scene name)
                     (error (concat "Invalid buffer name: " (symbol-name name)))))
         (bindings (tester:mapcar-hash (lambda (name value)
                                         (list name `(set-marker (make-marker) ,value)))
                                       (tester:buffer-markers buffer))))
    `(with-temp-buffer
       (insert ,(tester:buffer-string buffer))
       (goto-char (or ,(tester:buffer-point buffer) 1))
       (let (,@bindings) ,@forms))))

(defun tester:find-named-buffer (scene name)
  (when scene
    (or (gethash name (tester:scene-buffers tester:current-scene))
        (tester:find-named-buffer (tester:scene-parent scene) name))))

(defun tester:mapcar-hash (function hash)
  (let (result)
    (maphash (lambda (name value)
               (setq result (cons (funcall function name value) result)))
             hash)
    result))

;;;; Running ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: since emacs lisp only does dynamic binding, all locals and
;; function parameters in this section need to be prefixed with
;; 'tester:' so they do not affect client code.

(defun tester:run ()
  "Run all tests.

This runs the tests and prints the results to `standard-output'.

To run the tests from a command line, do:

  emacs -batch -l tester.el -l <test-file> ... -f tester:run"
  (message "\nRunning tests:")
  (mapc (lambda (tester:current-scene)
          (message "  * %s" (tester:scene-name tester:current-scene))
          (mapc (lambda (tester:current-test)
                  (tester:run-current-test))
                (tester:scene-tests tester:current-scene)))
        tester:scenes)
  (tester:output-footer))

(defun tester:run-current-test ()
  (tester:run-current-test-with-wrappers
   (tester:test-function tester:current-test)
   (tester:scene-full-wrapper-list tester:current-scene))
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
    (condition-case error-data
        (funcall function)
      (test-failed
       (unless (tester:test-result tester:current-test)
         (tester:test-failed error-data)))
      (error
       (unless (tester:test-result tester:current-test)
         (tester:test-erred error-data)))))

(defun tester:test-passed ()
  (tester:set-test-result tester:current-test 'pass)
  (message "    - %s" (tester:test-name tester:current-test)))

(defun tester:test-failed (error-data)
  (tester:set-test-result tester:current-test 'fail)
  (tester:set-test-error-data tester:current-test error-data)
  (message "\e[0;31m    - %s -- FAIL!\e[0m" (tester:test-name tester:current-test)))

(defun tester:test-erred (error-data)
  (tester:set-test-result tester:current-test 'error)
  (tester:set-test-error-data tester:current-test error-data)
  (message "\e[0;31m    - %s -- ERROR!\e[0m" (tester:test-name tester:current-test)))

(defun tester:output-footer ()
  (if (> (tester:num-errors) 0)
      (let ((i 1))
        (mapc (lambda (test)
                (when (memq (tester:test-result test) '(fail error))
                  (message "\n%d) Error on \"%s\":" i (tester:test-name test))
                  (message (tester:replace-in-string (format "%s" (tester:test-error-data test))
                                                     "^" "  "))
                  (setq i (1+ i))))
              (tester:all-tests))))
  (message " ")
  (if (= (tester:num-passed) (tester:num-run))
      (message "\e[1;32mAll %s tests passed.\e[0m" (tester:num-passed))
    (message "\e[1;31m%s tests failed, %s errors (%s passed).\e[0m"
             (tester:num-failed) (tester:num-errors) (tester:num-passed)))
)

(defun tester:all-tests ()
  (apply 'append (mapcar 'tester:scene-tests tester:scenes)))

(defun tester:num-passed ()
  (count-if (lambda (test) (eq (tester:test-result test) 'pass)) (tester:all-tests)))

(defun tester:num-failed ()
  (count-if (lambda (test) (eq (tester:test-result test) 'fail)) (tester:all-tests)))

(defun tester:num-errors ()
  (count-if (lambda (test) (eq (tester:test-result test) 'error)) (tester:all-tests)))

(defun tester:num-run ()
  (count-if (lambda (test) (tester:test-result test)) (tester:all-tests)))

;;;; Provide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tester)
