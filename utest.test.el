(require 'utest)

(scene "strings"
       (test "strings are delimited by headers"
             (let* ((strings (utest:parse-strings "== one\n1\n== two\n2\n" nil)))
               (check (hash-table-p strings))
               (check (= (hash-table-count strings) 2))
               (check (equal (gethash 'one strings) "1\n"))
               (check (equal (gethash 'two strings) "2\n"))
               ))
       (test "strings may be indented"
             (let* ((strings (utest:parse-strings " == one\n 1\n \t== two\n \t2\n \t 2\n" nil)))
               (check (hash-table-p strings))
               (check (= (hash-table-count strings) 2))
               (check (equal (gethash 'one strings) "1\n"))
               (check (equal (gethash 'two strings) "2\n 2\n")))))

(scene "buffers"
       (test "buffers are delimited by headers"
             (let* ((buffers (utest:parse-buffers "== one\n1\n== two\n2\n2\n" nil)))
               (check (hash-table-p buffers))
               (check (= (hash-table-count buffers) 2))
               (check (equal (utest:buffer-string (gethash 'one buffers)) "1\n"))
               (check (equal (utest:buffer-string (gethash 'two buffers)) "2\n2\n"))))
       (test "buffers may be indented"
             (let* ((buffers (utest:parse-buffers " == one\n 1\n  == two\n  2\n   2\n" nil)))
               (check (hash-table-p buffers))
               (check (= (hash-table-count buffers) 2))
               (check (equal (utest:buffer-string (gethash 'one buffers)) "1\n"))
               (check (equal (utest:buffer-string (gethash 'two buffers)) "2\n 2\n"))))
       (test "point may be specified with '-!-'"
             (let* ((buffers (utest:parse-buffers "== name\na-!-b\n" nil))
                    (buffer (gethash 'name buffers)))
               (check (= (utest:buffer-point buffer) 2))
               (check (equal (utest:buffer-string buffer) "ab\n"))))
       (test "markers may be specified with '-<NAME>-'"
             (let* ((buffers (utest:parse-buffers "== name\na-<first>-b-<second>-c\n" nil))
                    (buffer (gethash 'name buffers))
                    (markers (utest:buffer-markers buffer))
                    (first (gethash 'first markers))
                    (second (gethash 'second markers)))
               (check (= first 2))
               (check (= second 3))
               (check (equal (utest:buffer-string buffer) "abc\n"))))
       (test "omitting the body creates a buffer with a newline"
             (let* ((buffers (utest:parse-buffers "== name" nil))
                    (buffer (gethash 'name buffers)))
               (check (equal (utest:buffer-string buffer) "\n"))))
       (test "a trailing newline is added if none is given"
             (let* ((buffers (utest:parse-buffers "== name\nx" nil))
                    (buffer (gethash 'name buffers)))
               (check (equal  (utest:buffer-string buffer) "x\n"))))
       (test "the token may be specified with a second argument"
             (let* ((buffers (utest:parse-buffers "@@ name\nx\n" "@@"))
                    (buffer (gethash 'name buffers)))
               (check (equal (utest:buffer-string buffer) "x\n")))))

(scene "in-buffer"
       (buffers "
         == one
         a-<marker>-b")
       (test "switches to a temporary buffer"
             (let ((test-buffer (current-buffer)))
               (in-buffer one
                          (check (not (eq (current-buffer) test-buffer))))))
       (test "sets the buffer contents to that of the named buffer"
             (in-buffer one
                        (check (equal (buffer-string) "ab\n"))))
       (test "makes all markers available as local variables"
             (in-buffer one
                        (check (markerp marker))
                        (check (= (marker-position marker) 2))
                        (check (eq (marker-buffer marker) (current-buffer))))))

(scene "selecting a region"
       (test "select-region selects a region"
             (with-temp-buffer
               (transient-mark-mode 1)
               (insert (make-string 10 ?-))
               (check (not mark-active))
               (select-region)
               (check mark-active)))
)

(defvar values nil)
(scene "callbacks"
       (scene "outer"
              (before
                (check (equal values nil))
                (setq values '(1)))
              (after
                (check (equal values '(4 3 2 1)))
                (setq values (cons 5 values)))
              (scene "inner"
                     (before
                       (check (equal values '(1)))
                       (setq values (cons 2 values)))
                     (after
                       (check (equal values '(3 2 1)))
                       (setq values (cons 4 values)))
                     (test "callbacks are run in the correct order when scenes are nested"
                           (check (equal values '(2 1)))
                           (setq values (cons 3 values))))))

(scene "callbacks"
       (test "all callbacks are run"
             (check (equal values '(5 4 3 2 1)))))

(scene "nested named strings"
       (scene "outer"
              (strings "
                == one
                1
                == two
                2")
              (before (check (equal one "1\n")))
              (after (check (equal one "1\n")))
              (test "outer strings are available even if overridden in an inner scene"
                    (check (equal one "1\n")))
              (scene "inner"
                     (strings "
                       == one
                       11")
                     (before (check (equal one "11\n")))
                     (before (check (equal two "2\n")))
                     (test "inner strings override outer ones"
                           (check (equal one "11\n")))
                     (test "outer strings are available if not overridden"
                           (check (equal two "2\n"))))))
