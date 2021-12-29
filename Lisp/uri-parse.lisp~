(defstruct uri-structure
  scheme
  userinfo
  host
  (port 80 :type integer)
  path
  query
  fragment)

(defun uri-parse (s)
  (let* ((strList (mapcar 'string (coerce s 'list)))
         (scheme (parse-scheme strList))
         (userinfo (parse-userinfo (second scheme)))
         (host (parse-host (second userinfo)))
         (port (parse-port (second host)))
         (path (parse-path (second port)))
         (query (parse-query (second path)))
         (fragment (parse-fragment (second query))))
    (make-uri-structure
                   :scheme (car scheme)
                   :userinfo (car userinfo)
                   :host (car host)
                   :port (car port)
                   :path (car path)
                   :query (car query)
                   :fragment (car fragment))))

(defun parse-scheme (l)
  (let ((res (identificatore l)))
    (list (car res) (second res))))

(defparameter gen-delims
  (list #\: #\/ #\? #\# #\[ #\] #\@))
(defparameter sub-delims
  (list #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))
(defparameter unres-symb
  (list #\- #\. #\_ #\~))

(defun parse-userinfo (l)
  (list (car l) (second l)))

(defun parse-host (l)
  (list (car l) (second l)))

(defun parse-port (l)
  (list (parse-integer (car l)) (second l)))

(defun parse-path (l)
  (list (car l) (second l)))

(defun parse-query (l)
  (list (car l) (second l)))

(defun parse-fragment (l)
  (list (car l) (second l)))

(defun deleteFirst (lista)
  (append nil (second lista)))

(defun identificatore (x)
  (let ((res (caratteri x '(#\/ #\? #\# #\@ #\:))))
    (list (car res) (second res))))

(defun caratteri (chrs filtri)
  (let ((i (car chrs))
        (rest (cdr chrs)))
    (cond ((or (null i)
               (member i filtri))
           (list nil chrs))
          ((or
            (alphanumericp i) ;unreserved
            (member i unres-symb)
            (member i gen-delims) ;reserved
            (member i sub-delims)) 
           (let ((res (caratteri (cdr chrs) filtri)))
             (list (append (list i) (car res)) (second res)))))))