;; To run:
;;   $ emacs -Q -batch -l tester.el -l example-test.el -f tester:run

(require 'tester)

(scene "name of scene"
       (wrap (run))
       (wrap (run))
       (test "passing test" (check t))
       (test "failing test" (check nil))
       (test "erring test" (wibble))
       )
