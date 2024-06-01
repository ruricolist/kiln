# Installing Kiln

0. Clone Kiln where ASDF can find it. (Usually `~/common-lisp` or
   `~/quicklisp/local-projects`.)
1. Bootstrap the executable:
   - From a shell, run `make` in the Kiln directory.
   - OR, from a Lisp REPL, execute:
     ``` lisp
     > (asdf:make "kiln")
     ```
   This will generate an executable named `kiln` in the Kiln directory.
2. Symlink (don’t copy!) the `kiln` executable into your `PATH`.

   ```
   # Assuming XDG-compliant Linux.
   $ ln -s $(pwd -P)/kiln ${HOME}/.local/bin/kiln
   ```

   On an XDG-compliant Linux, with `~/.local/bin` on the `PATH`, you
   can install with Make:

   ```
   $ make test && make install
   ```

3. Test Kiln:
   ```
   $ kiln version
   ```

You can now use Kiln for writing shebang scripts. The remaining steps are for writing package scripts.

1. Create a `local-scripts` system where ASDF can find it. Make it a package-inferred system:
   ```lisp
   ;; In file local-scripts.asd.
   (defsystem "local-scripts" :class :package-inferred-system)
   ```
2. Write your first script as Lisp file in that directory. For example, write the following into `local-scripts/hello`:
    ```lisp
    (defpackage :local-scripts/hello
      (:use :cl)
      (:documentation "Say hello"))
    (in-package :local-scripts/hello)

    ;; A Kiln script defines a single function named main
    ;; that takes a list of arguments.
    (defun main (args)
      (destructuring-bind (name) args
        (format t "Hello, ~a~%" name)))
    ```
3. Run your script with Kiln:
    ```sh

    $ kiln hello $(whoami)
    Hello
    ```
