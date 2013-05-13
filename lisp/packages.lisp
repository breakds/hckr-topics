;;;; packages.lisp

(defpackage #:hckr-topics.scrawler
  (:nicknames #:scrawler)
  (:use #:cl 
        #:ppcre
        #:html5-parser
        #:alexandria
        #:drakma)
  (:export #:version
           #:html-named-child
           #:html-chain
           #:html-collect
           #:fetch-hckr-news-list
           ;; debugging
           #:unbreak
           #:count-sentences
           #:get-word-list
           #:acquire-news
           #:make-news
           #:news))


  