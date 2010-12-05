(require 'cl)

;;;; Compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'replace-in-string)
    ;; XEmacs
    (defalias 'test:replace-in-string 'replace-in-string)
  ;; GNU Emacs
  (defun test:replace-in-string (target old new &optional literal)
    "Replace all matches in STR for REGEXP with NEWTEXT string, 
 and returns the new string."
    (replace-regexp-in-string old new target nil literal)))

(if (functionp 'region-exists-p)
    ;; XEmacs
    (defalias 'test:region-exists-p 'region-exists-p)
  ;; GNU Emacs
  (defun test:region-exists-p ()
    "Return non-nil iff the region is highlighted."
    (if transient-mark-mode
        mark-active
      (condition-case e
          (mark)
        (mark-inactive)))))

;;;; Support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro test:defstruct (name &rest attributes)
  `(test:define-struct ',name ',attributes))

(defun test:define-struct (name attributes)
  (test:define-constructor name attributes)
  (mapc (lambda (attribute)
          (test:define-getter name attribute)
          (test:define-setter name attribute)
          (test:define-adder  name attribute))
        attributes))

(defun test:define-constructor (name attributes)
  (let ((constructor-name (intern (concat "test:make-" (symbol-name name))))
        (assignments (mapcar (lambda (attribute)
                               `(puthash ',attribute ,attribute table))
                             attributes)))
    (eval `(defun ,constructor-name (&optional ,@attributes)
             (let ((table (make-hash-table)))
               ,@assignments
               table)))))

(defun test:define-getter (struct attribute)
  (let ((getter (intern (concat "test:" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,getter (self)
       (gethash ',attribute self)))))

(defun test:define-setter (struct attribute)
  (let ((setter (intern (concat "test:set-" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,setter (self value)
       (puthash ',attribute value self)))))

(defun test:define-adder (struct attribute)
  (let ((adder (intern (concat "test:add-to-" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,adder (self value &optional append)
             (let ((list (gethash ',attribute self)))
               (add-to-list 'list value append)
               (puthash ',attribute list self))))))

(defun test:hash-table-keys (table)
  (let (keys)
    (maphash (lambda (key value)
               (setq keys (cons key keys)))
             table)
    keys))

;;;; Test structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test:defstruct scene parent name wrappers tests strings buffers)
(test:defstruct test scene name function result error-data)
(test:defstruct buffer string point markers)

(defun test:scene-full-wrapper-list (scene)
  (let ((wrappers ()))
    (while scene
      (setq wrappers (append (test:scene-wrappers scene) wrappers))
      (when (> (hash-table-count (test:scene-strings scene)) 0)
        (setq wrappers (cons (test:scene-strings-wrapper scene) wrappers)))
      (setq scene (test:scene-parent scene)))
    wrappers))

(defun test:scene-strings-wrapper (scene)
  (let* (bindings)
    (maphash (lambda (name value)
               (add-to-list 'bindings (list name value)))
             (test:scene-strings scene))
    (test:make-wrapper `((let ,bindings (run))))))

(defvar test:scenes nil
  "List of all test scenes.")

(defun test:make-wrapper (forms)
  (eval `(lambda () ,@(test:expand-run-calls forms))))

(defun test:expand-run-calls (forms)
  (mapcar (lambda (form)
            (if (listp form)
                (if (equal form '(run))
                    '(funcall run)
                  (test:expand-run-calls form))
              form))
          forms))

(defmacro scene (name &rest forms)
  `(test:define-scene nil ',name ',forms))

(defun test:define-scene (parent name forms)
  (let ((scene (test:make-scene))
        subscenes)
    (test:set-scene-parent scene parent)
    (test:set-scene-name scene name)
    (test:set-scene-strings scene (make-hash-table))
    (test:set-scene-buffers scene (make-hash-table))
    (mapc (lambda (form)
            (case (car form)
              ('test
               (let ((test (test:make-test)) function)
                 (test:set-test-scene test scene)
                 (test:set-test-name test (nth 1 form))
                 (setq function (eval `(lambda () ,@(cddr form))))
                 (test:set-test-function test function)
                 (test:add-to-scene-tests scene test t)))
              ('scene
               (setq subscenes (cons (cdr form) subscenes)))
              ('wrap
               (let ((wrapper (test:make-wrapper (cdr form))))
                 (test:add-to-scene-wrappers scene wrapper t)))
              ('strings
               (test:set-scene-strings scene (test:parse-strings (nth 1 form) (nth 2 form))))
              ('buffers
               (test:set-scene-buffers scene (test:parse-buffers (nth 1 form) (nth 2 form))))
              ('before
               (let ((wrapper (test:make-wrapper (append (copy-list (cdr form)) (list '(run))))))
                 (test:add-to-scene-wrappers scene wrapper t)))
              ('after
               (let ((wrapper (test:make-wrapper (cons '(run) (copy-list (cdr form))))))
                 (test:add-to-scene-wrappers scene wrapper t)))))
          forms)
    ;; Define nested scenes after this scene is constructed.
    (loop for spec in subscenes do
          (test:define-scene scene (car spec) (cdr spec)))
    (add-to-list 'test:scenes scene t)))

(defun test:parse-strings (spec token)
  (or token
      (setq token "=="))
  (let ((table (make-hash-table))
        (header-regexp (concat "^\\([ \t]*\\)" (regexp-quote token) " *\\(.*\\)$"))
        indent-regexp content name value start end)
    (with-temp-buffer
      (insert spec)
      (when (/= (char-before (point)) ?\n)
        (insert "\n"))
      (goto-char (point-min))
      (while (search-forward-regexp header-regexp nil 'or-goto-eob)
        (if (null name)
            ;; First header - check nothing significant before it.
            (and (string-match "[^ \t\r\n\f\v]" (buffer-substring 1 (match-beginning 0)))
                 (signal 'invalid-buffer-spec (concat "spec must start with `" token "'")))
          ;; Not first header - add string.
          (setq end (match-beginning 0)
                content (buffer-substring start end)
                value (replace-regexp-in-string indent-regexp "" content))
          (puthash (intern name) value table))
        (setq indent-regexp (concat "^" (regexp-quote (match-string 1)))
              name (match-string 2)
              start (1+ (point))))
      ;; Add last string.
      (when name
        (setq end (point)
              content (buffer-substring start end)
              value (replace-regexp-in-string indent-regexp "" content))
        (puthash (intern name) (if (string= value "") "\n" value) table)))
    table))

(defun test:parse-buffers (spec token)
  (let ((table (test:parse-strings spec token)))
    (maphash (lambda (name value)
               (puthash name (test:parse-buffer value) table))
             table)
    table))

(defun test:parse-buffer (spec)
  (let ((buffer (test:make-buffer)))
    (test:set-buffer-markers buffer (make-hash-table))
    (with-temp-buffer
      (insert spec)
      (goto-char (point-min))
      (while (search-forward-regexp "-\\(!\\|<\\(.*?\\)>\\)-" nil t)
        (cond
         ((string= (match-string 1) "!")
          (delete-region (match-beginning 0) (match-end 0))
          (test:set-buffer-point buffer (point)))
         (t
          (let ((name (intern (match-string 2))))
            (delete-region (match-beginning 0) (match-end 0))
            (puthash name (point) (test:buffer-markers buffer))))))
      (test:set-buffer-string buffer (buffer-string)))
    buffer))

;;;; Test library

(defmacro check (form)
  `(or ,form
       (signal 'test-failed '(,form))))

(put 'test-failed 'error-conditions '(test-failed error))
(put 'invalid-buffer-spec 'error-conditions '(invalid-buffer-spec error))

(defun select-region (&optional start end)
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
  (let* ((buffer (or (test:find-named-buffer test:current-scene name)
                     (error (concat "Invalid buffer name: " (symbol-name name)))))
         (bindings (test:mapcar-hash (lambda (name value)
                                         (list name `(set-marker (make-marker) ,value)))
                                       (test:buffer-markers buffer))))
    `(with-temp-buffer
       (insert ,(test:buffer-string buffer))
       (goto-char (or ,(test:buffer-point buffer) 1))
       (let (,@bindings) ,@forms))))

(defun test:find-named-buffer (scene name)
  (when scene
    (or (gethash name (test:scene-buffers test:current-scene))
        (test:find-named-buffer (test:scene-parent scene) name))))

(defun test:mapcar-hash (function hash)
  (let (result)
    (maphash (lambda (name value)
               (setq result (cons (funcall function name value) result)))
             hash)
    result))

;;;; Running ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: since emacs lisp only does dynamic binding, all locals and
;; function parameters in this section need to be prefixed with
;; 'test:' so they do not affect client code.

(defun test:run ()
  "Run all tests.

This runs the tests and prints the results to `standard-output'.

To run the tests from a command line, do:

  emacs -batch -l test.el -l <test-file> ... -f test:run"
  (message "\nRunning tests:")
  (mapc (lambda (test:current-scene)
          (message "  * %s" (test:scene-name test:current-scene))
          (mapc (lambda (test:current-test)
                  (test:run-current-test))
                (test:scene-tests test:current-scene)))
        test:scenes)
  (test:output-footer))

(defun test:run-current-test ()
  (test:run-current-test-with-wrappers
   (test:test-function test:current-test)
   (test:scene-full-wrapper-list test:current-scene))
  (unless (test:test-result test:current-test)
    (test:test-passed)))

(defun test:run-current-test-with-wrappers (run test:wrappers)
  (if (null test:wrappers)
      (test:call-safely run)
    (let* ((test:outer-run run)
           (run (lambda () (test:run-current-test-with-wrappers
                            test:outer-run (cdr test:wrappers)))))
      (test:call-safely (car test:wrappers)))))

(defun test:call-safely (function)
    (condition-case error-data
        (funcall function)
      (test-failed
       (unless (test:test-result test:current-test)
         (test:test-failed error-data)))
      (error
       (unless (test:test-result test:current-test)
         (test:test-erred error-data)))))

(defun test:test-passed ()
  (test:set-test-result test:current-test 'pass)
  (message "    - %s" (test:test-name test:current-test)))

(defun test:test-failed (error-data)
  (test:set-test-result test:current-test 'fail)
  (test:set-test-error-data test:current-test error-data)
  (message "\e[0;31m    - %s -- FAIL!\e[0m" (test:test-name test:current-test)))

(defun test:test-erred (error-data)
  (test:set-test-result test:current-test 'error)
  (test:set-test-error-data test:current-test error-data)
  (message "\e[0;31m    - %s -- ERROR!\e[0m" (test:test-name test:current-test)))

(defun test:output-footer ()
  (if (> (test:num-errors) 0)
      (let ((i 1))
        (mapc (lambda (test)
                (when (memq (test:test-result test) '(fail error))
                  (message "\n%d) Error on \"%s\":" i (test:test-name test))
                  (message (test:replace-in-string (format "%s" (test:test-error-data test))
                                                     "^" "  "))
                  (setq i (1+ i))))
              (test:all-tests))))
  (message " ")
  (if (= (test:num-passed) (test:num-run))
      (message "\e[1;32mAll %s tests passed.\e[0m" (test:num-passed))
    (message "\e[1;31m%s tests failed, %s errors (%s passed).\e[0m"
             (test:num-failed) (test:num-errors) (test:num-passed)))
)

(defun test:all-tests ()
  (apply 'append (mapcar 'test:scene-tests test:scenes)))

(defun test:num-passed ()
  (count-if (lambda (test) (eq (test:test-result test) 'pass)) (test:all-tests)))

(defun test:num-failed ()
  (count-if (lambda (test) (eq (test:test-result test) 'fail)) (test:all-tests)))

(defun test:num-errors ()
  (count-if (lambda (test) (eq (test:test-result test) 'error)) (test:all-tests)))

(defun test:num-run ()
  (count-if (lambda (test) (test:test-result test)) (test:all-tests)))

;;;; Provide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'test)
