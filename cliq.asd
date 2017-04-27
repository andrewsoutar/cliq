#+named-readtables
(named-readtables:in-readtable :standard)

#+nil (load #p "/home/user/.emacs.d/elpa/sly-20170205.1642/slynk/slynk.asd")

(asdf:defsystem :cliq
  :version "0.0.1"
  :description "An antisocial network"
  :author ("Andrew Soutar <andrew@andrewsoutar.com>")
  :maintainer "Andrew Soutar <andrew@andrewsoutar.com>"
  :defsystem-depends-on (:com.andrewsoutar.bootstrap :cffi-toolchain)
  :class :bootstrap-system
  :build-operation :static-program-op
  :build-pathname "bin/server"
  :depends-on (:uiop :cliq/main))
