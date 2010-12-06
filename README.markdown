## utest.el

Emacs lisp unit testing DSL. Cross-emacs compatible. Runnable from
command line.

## Example

In mylib.el:

    (defun add (a b) (+ a b))
    (provide 'mylib)

In mylib.test.el:

    (require 'mylib)
    (require 'utest)

    (scene "add"
      (test "returns the sum of two positive integers"
        (check (= (add 1 2) 3)))
      (test "returns the sum of two negative integers"
        (check (= (add -1 -2) -3))))

On the command line:

    $ utest -I /path/to/mylib mylib.test.el

`utest` exits with non-zero status if and only if any tests fail.  It
is compatible with GNU Emacs, Aquamacs, and XEmacs; you may specify an
emacs binary with `-e`. Great for automation and continuous
integration.

The `utest.el` library is searched for in the emacs load path, and in
the directory that `utest` lives in. You may expand the load path with
`-I`. Run with `--help` for more options.

## Digging Deeper

utest.el provides a rich DSL for common testing scenarios. **Tests**
are grouped into nestable **scenes**. Here's what you can do with
them.

### Before and After Hooks

You may specify forms to run before and after each test.

    (scene "my scene"
      (before
        ;; Code here runs before each test.
        ...)
      (after
        ;; Code here runs after each test.
        ...)
      (wrap
        ;; Code here wraps around the test. It should include a call
        ;; to `run' to run the test.
        ... (run) ...)
    )

Nested scenes behave as you would expect:

 * `before` hooks in outer scenes are run first.
 * `after` hooks in outer scenes are run last.
 * Outer `wrap` hooks wrap inner ones.

### Buffers

Most tests need to be run in the context of a buffer. You can define
named buffers via a `buffers` clause, and run code in a buffer's
context using `in-buffer`:

    (scene "using buffers"
      (buffers "
        == buffer-one
        This is buffer one.
        == buffer-two
        This is buffer two.")

      (test "buffer one"
        (in-buffer buffer-one
          (check (equal (buffer-string) "This is buffer one.\n"))))

      (test "buffer two"
        (in-buffer buffer-two
          (check (equal (buffer-string) "This is buffer two.\n")))))

Each buffer may specify the postion of point, as well as any number of
named markers, which are available as local variables in your
tests. Point is denoted by `-!-` in the buffer string, named markers
by `-<NAME>-`.

    (scene "test"
      (buffers "
        == buffer-one
        Foo -!-bar-<end>-")

      (test "buffer point and markers"
        (in-buffer buffer-one
          (check (equal (buffer-substring (point) end) "bar")))))

### Strings

To check long strings, like the contents of a multiline buffer, you
can keep your tests tidy by using named strings. This lets you use a
syntax similar to `buffers` to define long strings, which are
available as local variables in your tests.

    (scene "test"
      (strings "
        == string-one
        aaa bbb")

      (test "strings"
        (equal string-one "aaa bbb\n")))

### Selecting a Region

To test behavior when a region is selected, use `select-region`.

    (select-region start end)

Typically, `start` and `end` are markers in a named buffer (see
"Buffers" above).

## Note on Patches/Pull Requests

 * Bug reports: http://github.com/oggy/utest.el/issues
 * Source: http://github.com/oggy/utest.el
 * Patches: Fork on Github, send pull request.
   * Ensure patch includes tests.

## Copyright

Copyright (c) 2010 George Ogata. See LICENSE for details.
