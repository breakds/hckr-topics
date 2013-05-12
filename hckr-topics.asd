;;;; hckr-topics.asd

(asdf:defsystem #:hckr-topics
    :serial t
    :depends-on (#:cl-html5-parser)
    :components ((:file "lisp/packages")
                 (:file "lisp/scrawler")))