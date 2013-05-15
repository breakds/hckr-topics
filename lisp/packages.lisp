;;;; packages.lisp

(defpackage #:hckr-topics.scrawler
  (:nicknames #:scrawler)
  (:use #:cl 
        #:ppcre
        #:html5-parser
        #:alexandria
        #:drakma
        #:chtml)
  (:export #:version
           #:html-named-child
           #:html-chain
           #:html-collect
           #:lhtml-named-child
           #:lhtml-chain
           #:lhtml-dfs-chain
           #:fetch-hckr-news-list
           #:run-scrawler
           ;; debugging
           #:blacklisted
           #:extract-article
           #:analyze-web-page
           #:analyze-node
           #:add-list
           #:get-word
           #:unbreak
           #:count-sentences
           #:get-word-list
           #:acquire-news
           #:make-news
           #:news))


  