;;;; scrawler.lisp

(in-package #:hckr-topics.scrawler)


(defstruct news 
  (id 0 :type fixnum)
  (title "" :type string)
  (points 0 :type fixnum)
  (url "" :type string)
  (doc "" :type string))
  
  

(defun version ()
  "display the version of the scrawler on REPL"
  (format t "Hacker News Topics Scrawler v0.01"))


;;; utilities
(defun group (lst n)
  "group items in lst by size n, return nil if not divisible by n"
  (labels ((group-iter (lst k accu)
             (if (null lst)
                 accu
                 (if (zerop k)
                     (group-iter (rest lst)
                                 (1- n)
                                 (cons (list (car lst)) accu))
                     (group-iter (rest lst)
                                 (1- k)
                                 (cons (cons (car lst) (car accu))
                                       (cdr accu)))))))
    (when (zerop (mod (length lst) n))
      (group-iter (reverse lst) n nil))))

(defun split-at-n (lst n)
  "split the list lst at the position n, namely the first part will
have the first n elements of lst"
  (labels ((split-iter (lst k accu)
             (if (zerop k)
                 (values (nreverse accu) lst)
                 (split-iter (rest lst) (1- k) (cons (car lst) accu)))))
    (if (< n 0)
        (split-iter lst (+ (length lst) n) nil)
        (split-iter lst n nil))))
                      
  
  
                                 
                                 
                                 


;;; html related
(defun html-named-child (node specifier)
  "return the nth child of the node with a specific name"
  (labels ((nth-named-sibling (child name n)
             (cond ((null child) nil)
                   ((equal name (node-name child))
                    (if (zerop n)
                        child
                        (nth-named-sibling (node-next-sibling child) 
                                           name
                                           (1- n))))
                   (t (nth-named-sibling (node-next-sibling child) 
                                         name 
                                         n)))))
    (when node
      (if (stringp specifier)
          (nth-named-sibling (node-first-child node) specifier 0) 
          (nth-named-sibling (node-first-child node) 
                             (car specifier) (cadr specifier))))))

(defun html-chain (node spec-seq)
  "starting from the given node, follow the path specified by the
  sequence spec-seq, where each element in spec-seq is a specifier."
  (if (null spec-seq)
      node
      (html-chain (html-named-child node (car spec-seq))
                  (rest spec-seq))))

(defun html-collect (node match-seq &optional (fixed-seq nil))
  "find the node with path fxied-seq, and collect all the (grand)
children that matches match-seq"
  (labels ((collect-named-sibling (child name accu)
             (cond ((null child) (nreverse accu))
                   ((equal name (node-name child))
                    (collect-named-sibling (node-next-sibling child)
                                           name
                                           (cons child accu)))
                   (t (collect-named-sibling (node-next-sibling child)
                                             name
                                             accu)))))
    (let ((anchor (html-chain node fixed-seq)))
      (when anchor
        (let ((cands (collect-named-sibling (node-first-child anchor)
                                            (car match-seq)
                                            nil)))
          (mapcar (lambda (x) (html-chain x (cdr match-seq)))
                  cands))))))


;;; hacker news fetcher
(defun acquire-news (triplet)
  "construct a news object from a <tr> triplet"
  (make-news :id (parse-integer 
                  (subseq (element-attribute 
                           (html-chain (car triplet) 
                                       '(("td" 1) "center" "a"))
                           "id") 3))
             :title (node-value (node-first-child 
                                 (html-chain (car triplet) 
                                             '(("td" 2) "a"))))
             :points (let ((str (node-value 
                                 (node-first-child
                                  (html-chain (cadr triplet)
                                              '(("td" 1) "span"))))))
                       (parse-integer (subseq str 0 
                                              (position #\Space str))))
             :url (element-attribute (html-chain (car triplet) 
                                                 '(("td" 2) "a"))
                                     "href")))
             

(defun fetch-hckr-news-list (uri)
  "returns a list of documents, and a url to the next page"
  (let ((page (parse-html5 (http-request uri))))
    (let ((prelim (html-collect page '("tr") 
                                '("html" "body" "center" "table"
                                  "tbody" ("tr" 2) "td" "table" "tbody"))))
      (when (= (mod (length prelim) 3) 2)
        (multiple-value-bind (item-trs more-trs) (split-at-n prelim -2)
          (let ((grouped-trs (group item-trs 3)))
            (values (mapcar #'acquire-news grouped-trs)
                    (element-attribute (html-chain (cadr more-trs)
                                                   '(("td" 1) "a"))
                                       "href"))))))))
                      
            
;;; article normalizer
(defun unbreak (str)
  "remove break entities like &nbsr; and &ldquot etc."
  (regex-replace-all "&.{0,9};" str))

(defun count-sentences (str)
  "count the number of sentences in this string"
  (ash (length (all-matches "(?<!mr|ms|dr|no)\\.|\\?|\\!" str)) -1))

(defun get-word-list (str)
  (split "\\s+" (string-downcase str)))



        
      
                                
  

    
    
  
      
    
  