;;;; archive.lisp

(in-package #:hckr-topics.archive)

(defun version ()
  (format t "hacker topics archive v0.01~%"))

(defparameter *stop-word-file* 
  (merge-pathnames "conf/stopwords" (asdf:system-source-directory 'hckr-topics)))


(defparameter *stop-word-dict* 
  (let ((dict (make-hash-table :test #'equal)))
    (with-open-file (in *stop-word-file*
                        :direction :input)
      (loop for line = (read-line in nil 'eof)
         until (eq line 'eof)
         do (setf (gethash line dict) t)))
    dict))



(defun learn-dict (source)
  (let ((dict (make-hash-table :test #'equal)))
    (loop for news-file in (list-directory source)
       do (let ((word-list (with-open-file (in news-file
                                               :direction :input)
                             (read in)
                             (read in))))
            (loop for word in word-list
               do (when (not (gethash word *stop-word-dict*))
                    (if (gethash word dict)
                        (incf (gethash word dict))
                        (setf (gethash word dict) 1))))))
    (let ((final-dict (make-hash-table :test #'equal))
          (inv-dict (make-hash-table))
          (dict-size 0))
      (loop for word being the hash-keys of dict 
           using (hash-value cnt)
         when (> cnt 10)
         do 
           (setf (gethash word final-dict) dict-size)
           (setf (gethash dict-size inv-dict) word)
           (incf dict-size))
      (values final-dict inv-dict))))


  
  

                   
(defun archive-folder (source &key (target "./"))
  (ensure-directories-exist target)
  (multiple-value-bind (dict inv-dict) (learn-dict source)
    (loop for news-file in (list-directory source)
       do (multiple-value-bind (points word-list)
              (with-open-file (in news-file
                                  :direction :input)
                (values (read in) (read in)))
            (let ((doc (make-hash-table)))
              (loop for word in word-list
                 for w-id = (gethash word dict)
                 when w-id
                 do (if (gethash w-id doc)
                        (incf (gethash w-id doc))
                        (setf (gethash w-id doc) 1)))
            ;; write output file
              (with-open-file (*standard-output* (merge-pathnames (pathname-name news-file)
                                                                  target)
                                                 :direction :output
                                                 :if-exists :supersede)
                (format t "~a~%" points)
                (loop for w-id being the hash-keys of doc
                   using (hash-value cnt)
                   do (format t "~a ~a~%" w-id cnt)))
              (format t "[Done] ~a~%" (pathname-name news-file)))))
    ;; write news list
    (with-open-file (*standard-output* (merge-pathnames "list.txt"
                                                        target)
                                       :direction :output
                                       :if-exists :supersede)
      (loop for news-file in (list-directory source)
         do (format t "~a~%" (pathname-name news-file))))
    ;; write dictionary
    (with-open-file (*standard-output* (merge-pathnames "dict.txt"
                                                        target)
                                       :direction :output
                                       :if-exists :supersede)
      (loop for i below (hash-table-count inv-dict)
         do (format t "~a~%" (gethash i inv-dict))))))
      

                   
                      
            
    
    
