(require 'cl)

;;;; Compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'replace-in-string)
    ;; XEmacs
    (defalias 'utest:replace-in-string 'replace-in-string)
  ;; GNU Emacs
  (defun utest:replace-in-string (target old new &optional literal)
    "Replace all matches in STR for REGEXP with NEWTEXT string, 
 and returns the new string."
    (replace-regexp-in-string old new target nil literal)))

;;;; Support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro utest:defstruct (name &rest attributes)
  `(utest:define-struct ',name ',attributes))

(defun utest:define-struct (name attributes)
  (utest:define-constructor name attributes)
  (mapc (lambda (attribute)
          (utest:define-getter name attribute)
          (utest:define-setter name attribute)
          (utest:define-adder  name attribute))
        attributes))

(defun utest:define-constructor (name attributes)
  (let ((constructor-name (intern (concat "utest:make-" (symbol-name name))))
        (assignments (mapcar (lambda (attribute)
                               `(puthash ',attribute ,attribute table))
                             attributes)))
    (eval `(defun ,constructor-name (&optional ,@attributes)
             (let ((table (make-hash-table)))
               ,@assignments
               table)))))

(defun utest:define-getter (struct attribute)
  (let ((getter (intern (concat "utest:" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,getter (self)
       (gethash ',attribute self)))))

(defun utest:define-setter (struct attribute)
  (let ((setter (intern (concat "utest:set-" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,setter (self value)
       (puthash ',attribute value self)))))

(defun utest:define-adder (struct attribute)
  (let ((adder (intern (concat "utest:add-to-" (symbol-name struct) "-" (symbol-name attribute)))))
    (eval `(defun ,adder (self value &optional append)
             (let ((list (gethash ',attribute self)))
               (add-to-list 'list value append)
               (puthash ',attribute list self))))))

(defun utest:hash-table-keys (table)
  (let (keys)
    (maphash (lambda (key value)
               (setq keys (cons key keys)))
             table)
    keys))

;;;; Test structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(utest:defstruct scene parent name wrappers tests strings buffers)
(utest:defstruct test scene name function result error-data)
(utest:defstruct buffer string point markers)

(defun utest:scene-full-wrapper-list (scene)
  (let ((wrappers ()))
    (while scene
      (setq wrappers (append (utest:scene-wrappers scene) wrappers))
      (when (> (hash-table-count (utest:scene-strings scene)) 0)
        (setq wrappers (cons (utest:scene-strings-wrapper scene) wrappers)))
      (setq scene (utest:scene-parent scene)))
    wrappers))

(defun utest:scene-strings-wrapper (scene)
  (let* (bindings)
    (maphash (lambda (name value)
               (add-to-list 'bindings (list name value)))
             (utest:scene-strings scene))
    (utest:make-wrapper `((let ,bindings (run))))))

(defvar utest:scenes nil
  "List of all test scenes.")

(defun utest:make-wrapper (forms)
  (eval `(lambda () ,@(utest:expand-run-calls forms))))

(defun utest:expand-run-calls (forms)
  (mapcar (lambda (form)
            (if (listp form)
                (if (equal form '(run))
                    '(funcall run)
                  (utest:expand-run-calls form))
              form))
          forms))

(defmacro scene (name &rest forms)
  `(utest:define-scene nil ',name ',forms))

(defun utest:define-scene (parent name forms)
  (let ((scene (utest:make-scene))
        subscenes)
    (utest:set-scene-parent scene parent)
    (utest:set-scene-name scene name)
    (utest:set-scene-strings scene (make-hash-table))
    (utest:set-scene-buffers scene (make-hash-table))
    (mapc (lambda (form)
            (case (car form)
              ('test
               (let ((test (utest:make-test)) function)
                 (utest:set-test-scene test scene)
                 (utest:set-test-name test (nth 1 form))
                 (setq function (eval `(lambda () ,@(cddr form))))
                 (utest:set-test-function test function)
                 (utest:add-to-scene-tests scene test t)))
              ('scene
               (setq subscenes (cons (cdr form) subscenes)))
              ('wrap
               (let ((wrapper (utest:make-wrapper (cdr form))))
                 (utest:add-to-scene-wrappers scene wrapper t)))
              ('strings
               (utest:set-scene-strings scene (utest:parse-strings (nth 1 form) (nth 2 form))))
              ('buffers
               (utest:set-scene-buffers scene (utest:parse-buffers (nth 1 form) (nth 2 form))))
              ('before
               (let ((wrapper (utest:make-wrapper (append (copy-list (cdr form)) (list '(run))))))
                 (utest:add-to-scene-wrappers scene wrapper t)))
              ('after
               (let ((wrapper (utest:make-wrapper (cons '(run) (copy-list (cdr form))))))
                 (utest:add-to-scene-wrappers scene wrapper t)))))
          forms)
    ;; Define nested scenes after this scene is constructed.
    (loop for spec in subscenes do
          (utest:define-scene scene (car spec) (cdr spec)))
    (add-to-list 'utest:scenes scene t)))

(defun utest:parse-strings (spec token)
  (or token
      (setq token "=="))
  (let ((table (make-hash-table))
        (header-regexp (concat "^\\([ \t]*\\)" (regexp-quote token) " *\\(.*\\)$"))
        start
        (end (make-marker))
        indent name value)
    (with-temp-buffer
      (insert spec)
      (when (/= (char-before (point)) ?\n)
        (insert "\n"))
      (goto-char (point-min))
      (while (search-forward-regexp header-regexp nil 'or-goto-end)
        (if (null name)
            ;; First header - check nothing significant before it.
            (and (string-match "[^ \t\r\n\f\v]" (buffer-substring 1 (match-beginning 0)))
                 (signal 'invalid-buffer-spec (concat "missing `" token "' header")))
          ;; Not first header - add string.
          (set-marker end (match-beginning 0))
          (utest:remove-indent start end indent)
          (setq value (buffer-substring start end))
          (puthash (intern name) value table))
        (setq name (match-string 2)
              indent (match-string 1)
              start (1+ (point))))
      ;; Add last string.
      (when name
        (utest:remove-indent start (point) indent)
        (setq value (buffer-substring start (point)))
        (puthash (intern name) (if (string= value "") "\n" value) table)))
    (set-marker end nil)
    table))

(defun utest:remove-indent (start end indent)
  (unless (string= indent "")
    (let ((regexp (concat "^" (regexp-quote indent)))
          (end-marker (make-marker)))
      (set-marker end-marker end)
      (save-excursion
        (save-match-data
          (goto-char start)
          (while (search-forward-regexp regexp end-marker t)
            (delete-region (match-beginning 0) (match-end 0)))))
      (set-marker end-marker nil))))

(defun utest:parse-buffers (spec token)
  (let ((table (utest:parse-strings spec token)))
    (maphash (lambda (name value)
               (puthash name (utest:parse-buffer value) table))
             table)
    table))

(defun utest:parse-buffer (spec)
  (let ((buffer (utest:make-buffer)))
    (utest:set-buffer-markers buffer (make-hash-table))
    (with-temp-buffer
      (insert spec)
      (goto-char (point-min))
      (while (search-forward-regexp "-\\(!\\|<\\(.*?\\)>\\)-" nil t)
        (cond
         ((string= (match-string 1) "!")
          (delete-region (match-beginning 0) (match-end 0))
          (utest:set-buffer-point buffer (point)))
         (t
          (let ((name (intern (match-string 2))))
            (delete-region (match-beginning 0) (match-end 0))
            (puthash name (point) (utest:buffer-markers buffer))))))
      (utest:set-buffer-string buffer (buffer-string)))
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
  (let* ((buffer (or (utest:find-named-buffer utest:current-scene name)
                     (error (concat "Invalid buffer name: " (symbol-name name)))))
         (bindings (utest:mapcar-hash (lambda (name value)
                                         (list name `(set-marker (make-marker) ,value)))
                                       (utest:buffer-markers buffer))))
    `(with-temp-buffer
       (insert ,(utest:buffer-string buffer))
       (goto-char (or ,(utest:buffer-point buffer) 1))
       (let (,@bindings) ,@forms))))

(defun utest:find-named-buffer (scene name)
  (when scene
    (or (gethash name (utest:scene-buffers utest:current-scene))
        (utest:find-named-buffer (utest:scene-parent scene) name))))

(defun utest:mapcar-hash (function hash)
  (let (result)
    (maphash (lambda (name value)
               (setq result (cons (funcall function name value) result)))
             hash)
    result))

;;;; Running ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: since emacs lisp only does dynamic binding, all locals and
;; function parameters in this section need to be prefixed with
;; 'utest:' so they do not affect client code.

(defun utest:run ()
  "Run all tests.

This runs the tests and prints the results to `standard-output'."
  (message "\nRunning tests:")
  (mapc (lambda (utest:current-scene)
          (message "  * %s" (utest:scene-name utest:current-scene))
          (mapc (lambda (utest:current-test)
                  (utest:run-current-test))
                (utest:scene-tests utest:current-scene)))
        utest:scenes)
  (utest:output-footer))

(defun utest:run-current-test ()
  (utest:run-current-test-with-wrappers
   (utest:test-function utest:current-test)
   (utest:scene-full-wrapper-list utest:current-scene))
  (unless (utest:test-result utest:current-test)
    (utest:test-passed)))

(defun utest:run-current-test-with-wrappers (run utest:wrappers)
  (if (null utest:wrappers)
      (utest:call-safely run)
    (let* ((utest:outer-run run)
           (run (lambda () (utest:run-current-test-with-wrappers
                            utest:outer-run (cdr utest:wrappers)))))
      (utest:call-safely (car utest:wrappers)))))

(defun utest:call-safely (function)
    (condition-case error-data
        (funcall function)
      (test-failed
       (unless (utest:test-result utest:current-test)
         (utest:test-failed error-data)))
      (error
       (unless (utest:test-result utest:current-test)
         (utest:test-erred error-data)))))

(defun utest:test-passed ()
  (utest:set-test-result utest:current-test 'pass)
  (message "    - %s" (utest:test-name utest:current-test)))

(defun utest:test-failed (error-data)
  (utest:set-test-result utest:current-test 'fail)
  (utest:set-test-error-data utest:current-test error-data)
  (message "\e[0;31m    - %s -- FAIL!\e[0m" (utest:test-name utest:current-test)))

(defun utest:test-erred (error-data)
  (utest:set-test-result utest:current-test 'error)
  (utest:set-test-error-data utest:current-test error-data)
  (message "\e[0;31m    - %s -- ERROR!\e[0m" (utest:test-name utest:current-test)))

(defun utest:output-footer ()
  (if (> (utest:num-errors) 0)
      (let ((i 1))
        (mapc (lambda (test)
                (when (memq (utest:test-result test) '(fail error))
                  (message "\n%d) Error on \"%s\":" i (utest:test-name test))
                  (message (utest:replace-in-string (format "%s" (utest:test-error-data test))
                                                     "^" "  "))
                  (setq i (1+ i))))
              (utest:all-tests))))
  (message " ")
  (if (= (utest:num-passed) (utest:num-run))
      (message "\e[1;32mAll %s tests passed.\e[0m" (utest:num-passed))
    (message "\e[1;31m%s tests failed, %s errors (%s passed).\e[0m"
             (utest:num-failed) (utest:num-errors) (utest:num-passed)))
)

(defun utest:all-tests ()
  (apply 'append (mapcar 'utest:scene-tests utest:scenes)))

(defun utest:num-passed ()
  (count-if (lambda (test) (eq (utest:test-result test) 'pass)) (utest:all-tests)))

(defun utest:num-failed ()
  (count-if (lambda (test) (eq (utest:test-result test) 'fail)) (utest:all-tests)))

(defun utest:num-errors ()
  (count-if (lambda (test) (eq (utest:test-result test) 'error)) (utest:all-tests)))

(defun utest:num-run ()
  (count-if (lambda (test) (utest:test-result test)) (utest:all-tests)))

;;;; Provide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'utest)
