;;;; scrawler.lisp

(in-package #:hckr-topics.scrawler)


(defstruct news 
  (id 0 :type fixnum)
  (title "" :type string)
  (points 0 :type fixnum)
  (url "" :type string))



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

;;; lhtml related
(defun lhtml-children (node)
  (cddr node))

(defun lhtml-named-child (node specifier)
  "return the nth child of the node with a specific name"
  (labels ((nth-named-sibling (node-lst name n)
             (cond ((null node-lst) nil)
                   ((atom (car node-lst)) (nth-named-sibling (rest node-lst)
                                                             name 
                                                             n))
                   ((equal name (caar node-lst))
                    (if (zerop n)
                        (car node-lst)
                        (nth-named-sibling (rest node-lst) 
                                           name
                                           (1- n))))
                   (t (nth-named-sibling (rest node-lst)
                                         name 
                                         n)))))
    (when node
      (if (atom specifier)
          (nth-named-sibling (lhtml-children node) specifier 0) 
          (nth-named-sibling (lhtml-children node) 
                             (car specifier) (cadr specifier))))))


(defun lhtml-chain (node spec-seq)
  "starting from the given node, follow the path specified by the
  sequence spec-seq, where each element in spec-seq is a specifier."
  (if (null spec-seq)
      node
      (lhtml-chain (lhtml-named-child node (car spec-seq))
                   (rest spec-seq))))

(defun lhtml-dfs-chain (node path)
  "starting from the given node, fetch all the text/string that
  satisfies the path"
  (if (null path)
      (when (stringp node)
        (list node))
      (when (and (consp node)
               (eq (car node) (car path)))
        (mapcan (lambda (x)
                  (lhtml-dfs-chain x (rest path)))
                (lhtml-children node)))))




;;; hacker news fetcher
(defun acquire-id (triplet)
  (ignore-errors
    (parse-integer 
     (subseq (element-attribute 
              (html-chain (car triplet) 
                          '(("td" 1) "center" "a"))
              "id") 3))))

(defun acquire-title (triplet)
  (ignore-errors
    (node-value (node-first-child 
                 (html-chain (car triplet) 
                             '(("td" 2) "a"))))))

(defun acquire-points (triplet)
  (ignore-errors
    (let ((str (node-value 
                (node-first-child
                 (html-chain (cadr triplet)
                             '(("td" 1) "span"))))))
      (parse-integer (subseq str 0 
                             (position #\Space str))))))

(defun acquire-url (triplet)
  (ignore-errors
    (element-attribute (html-chain (car triplet) 
                                   '(("td" 2) "a"))
                       "href")))


(defun acquire-news (triplet)
  "construct a news object from a <tr> triplet"
  (let ((id (acquire-id triplet))
        (title (acquire-title triplet))
        (points (acquire-points triplet))
        (url (acquire-url triplet)))
    (when (and id title points url)
      (make-news :id id
                 :title title
                 :points points
                 :url url))))

(defun fetch-hckr-news-list (uri)
  "returns a list of documents, and a url to the next page"
  (let ((page (parse-html5 (http-request uri))))
    (let ((prelim (html-collect page '("tr") 
                                '("html" "body" "center" "table"
                                  "tbody" ("tr" 2) "td" "table" "tbody"))))
      (when (= (mod (length prelim) 3) 2)
        (multiple-value-bind (item-trs more-trs) (split-at-n prelim -2)
          (let ((grouped-trs (group item-trs 3)))
            (values (remove-if #'null (mapcar #'acquire-news grouped-trs))
                    (element-attribute (html-chain (cadr more-trs)
                                                   '(("td" 1) "a"))
                                       "href"))))))))


;;; article normalizer
(defparameter *word-extractor* (create-scanner "^([a-z]+)[\\.\\?:,;]?$"))

(defun unbreak (str)
  "remove break entities like &nbsr; and &ldquot etc."
  (regex-replace-all "&.{0,9};" str " "))

(defun count-sentences (str)
  "count the number of sentences in this string"
  (ash (length (all-matches "(?<!mr|ms|dr|no)(\\.|\\?|\\!)(\\s+|$)" str)) -1))

(defun get-word (str)
  (ignore-errors
    (aref (nth-value 1 (scan-to-strings *word-extractor* str)) 0)))


(defun get-word-list (str)
  (remove-if #'null (mapcar #'get-word (split "\\s+" (string-downcase str)))))

(defun add-list (x y)
  (if (and x y)
      (cons (+ (car x) (car y)) 
            (add-list (rest x) (rest y)))))


(defun better-score-p (a b)
  (or (> (car a) (* (car b) 3))
      (and (< (car b) (* (car a) 3))
           (< (cadr b) (cadr a)))))




(defun pick-best-score (score-list)
  (reduce (lambda (acc cur)
            (if (better-score-p (cdr cur) (cdr acc))
                cur acc))
          (loop for key being the hash-keys of score-list
             using (hash-value value)
             collect (cons key value))))

(defun analyze-node (node)
  "return (path sentence-count word-count) list"
  (cond ((stringp node)
         (let ((filtered (unbreak node)))
           (list nil ;; no path for string node
                 (count-sentences filtered) ;; count sentences
                 (length (get-word-list filtered))))) ;; count words

        ((lhtml-children node)
         (let ((score-list (make-hash-table :test #'equal)))
           (mapcar (lambda (x) 
                     (let* ((score (analyze-node x))
                            (path (car score)))
                       (let ((existed (gethash path score-list)))
                         (if existed
                             (setf (gethash path score-list) 
                                   (add-list (cdr score) existed))
                             (setf (gethash path score-list) (cdr score))))))
                   (lhtml-children node))
           (let ((best (pick-best-score score-list)))
             (cons (cons (car node) (car best))
                   (cdr best)))))
  (t (list 0 0))))

(defun analyze-web-page (uri)
  (analyze-node (parse (http-request uri) (make-lhtml-builder))))

(defun extract-article (uri)
  (let ((dom (parse (http-request uri) (make-lhtml-builder))))
    (let ((result (analyze-node dom)))
      (lhtml-dfs-chain dom (car result)))))



(defun save-article (news-item folder)
  (with-open-file (*standard-output* 
                   (merge-pathnames (format nil "~a.txt" 
                                            (news-id news-item))
                                    folder)
                   :direction :output
                   :if-exists :supersede)
    (print (news-points news-item))
    (print (mapcan (lambda (x)
                     (get-word-list (unbreak x)))
                   (extract-article (news-url news-item))))))

;;; main scrawler

                 

(defparameter *hckrnews-url* "https://news.ycombinator.com/")


(defparameter *blacklist* '("github.com"))

(defun blacklisted (url)
  (reduce (lambda (acc cur)
            (if acc
                acc
                (when (all-matches cur url) t)))
          *blacklist* :initial-value nil))


(defun run-scrawler (&key (required-num 200) (folder "./"))
  (ensure-directories-exist folder)
  (let ((count 0))
    (labels ((scrawl-iter (list-key)
               (multiple-value-bind (lst more) 
                   (fetch-hckr-news-list (format nil "~a~a"
                                                 *hckrnews-url*
                                                 list-key))
                 (loop for item in lst
                    do (when (not (blacklisted (news-url item)))
                         (handler-case
                             (progn
                               (format t "->~t~a" (news-title item))
                               (save-article item folder)
                               (format t " [done] ~a/~a~%" (incf count) required-num))
                         (error () (format t " [fail]~%")))))
                 (when (< count required-num)
                   (loop for i below 10
                      do (sleep 2)
                        (format t "~a.." i))
                   (fresh-line)
                   (scrawl-iter (if (eq (char more 0) #\/)
                                    (subseq more 1)
                                    more))))))

      (scrawl-iter ""))))



                              




























