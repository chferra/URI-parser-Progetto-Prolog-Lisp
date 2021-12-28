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
  (print (list (car l) (cdr l)))
  (list (car l) (cdr l)))

(defun parse-userinfo (l)
  (print (list (car l) (cdr l)))
  (list (car l) (cdr l)))

(defun parse-host (l)
  (print (list (car l) (cdr l)))
  (list (car l) (cdr l)))

(defun parse-port (l)
  (print (list (car l) (cdr l)))
  (list (parse-integer (car l)) (cdr l)))

(defun parse-path (l)
  (print (list (car l) (cdr l)))
  (list (car l) (cdr l)))

(defun parse-query (l)
  (print (list (car l) (cdr l)))
  (list (car l) (cdr l)))

(defun parse-fragment (l)
  (print (list (car l) (cdr l)))
  (list (car l) (cdr l)))

(defun deleteFirst (lista)
  (append nil (cdr lista)))