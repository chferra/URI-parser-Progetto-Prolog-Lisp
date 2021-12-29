(defstruct uri-structure
  scheme
  userinfo
  host
  (port 80 :type integer)
  path
  query
  fragment)

(defstruct partial-res
  fieldVal
  rest
  (failure nil))

(defparameter gen-delims
  (list #\: #\/ #\? #\# #\[ #\] #\@))
(defparameter sub-delims
  (list #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))
(defparameter unres-symb
  (list #\- #\. #\_ #\~))

(defun uri-parse (s)
  (let* ((charList (coerce s 'list))
         (scheme (parse-scheme charList))
         (userinfo (parse-userinfo scheme))
         (host (parse-host userinfo))
         (port (parse-port host))
         (path (parse-path port))
         (query (parse-query path))
         (fragment (parse-fragment query)))
    (make-uri-structure
                   :scheme (partial-res-fieldVal scheme)
                   :userinfo (partial-res-fieldVal userinfo)
                   :host (partial-res-fieldVal host)
                   :port (partial-res-fieldVal port)
                   :path (partial-res-fieldVal path)
                   :query (partial-res-fieldVal query)
                   :fragment (partial-res-fieldVal fragment))))

(defun parse-scheme (l)
  (let ((res (identificatore l)))
    (make-partial-res 
     :fieldVal (car res)
     :rest (delFirstN (second res) 1)
     :failure (char/= (car (second res)) #\:))))

(defun parse-userinfo (s)
  (if (partial-res-failure s) s)
  (let ((res (identificatore l)))
    (list (car res) (second res))))

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

(defun delFirstN (l n)
  (cond ((< n 0) nil)
        ((eq n 0) l)
        ((eq n 1) (cdr l))
        (T (delFirstN (cdr l) (- n 1)))))

