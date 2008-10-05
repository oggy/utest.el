;; To run:
;;   $ emacs -Q -batch -l tester.el -l example-test.el -f tester:run

(require 'tester)

(scene "name of scene"
       (wrap (let ((x 1)) (run)))
       (wrap (let ((y 2)) (run)))
       (test "passing test" (check (= (+ x y) 3)))
       (test "failing test" (check nil))
       (test "erring test" (wibble))

       (scene "nested scene"
              (wrap (let ((z 3)) (run)))
              (test "using x and z" (check (= (+ x z) 4))))

       (test "using buffer one"
             (tester:in-buffer one
                               (check (string= (buffer-string) "hi\n"))
                               (check (= (point) 1))))
       (test "using buffer two"
             (tester:in-buffer two
                               (check (string= (buffer-string) "bye\n"))
                               (check (= (point) 4))))
       (buffers "
== one
hi
== two
bye-!-
")
       )