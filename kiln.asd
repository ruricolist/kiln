(defsystem "kiln"
  :version "0.0.1"
  :description "Practical scripting in Common Lisp"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (:kiln/all)
  :class :package-inferred-system
  :in-order-to ((build-op (load-op "kiln/scripts/rebuild")))
  :perform
  (build-op (o c) (symbol-call :kiln/scripts/rebuild '#:main nil)))

(asdf:defsystem "kiln/build"
  :description "Build the Kiln image"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (:kiln/image)
  :class :package-inferred-system
  :build-operation "asdf:program-op"
  :build-pathname "kiln"
  :entry-point "kiln/dispatch:invoke-entry-point")

(asdf:defsystem "kiln/scripts"
  :description "Built-in Kiln scripts"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :class :package-inferred-system
  :pathname "scripts/")

(register-system-packages "trivia" '(:trivia.fail))
