(defstruct uri-structure
  scheme
  userinfo
  host
  (port 80 :type integer)
  path
  query
  fragment)

(defparameter gen-delims
  (list #\: #\/ #\? #\# #\[ #\] #\@))
(defparameter sub-delims
  (list #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))
(defparameter unres-symb
  (list #\- #\. #\_ #\~))

(defun uri-parse (s)
  (let* ((charList (coerce s 'list))
         (scheme (parse-scheme charList))
         (auth (parse-auth (cdr scheme)))
         (pqf (parse-pqf auth)))
    (make-uri-structure
                   :scheme (first scheme)
                   :userinfo (first auth)
                   :host (second auth)
                   :port (third auth)
                   :path (first pqf)
                   :query (second pqf)
                   :fragment (third pqf))))

(defun parse-scheme (l)
  (let ((res (identificatore l)))
    (if (and (> (length res) 0)
         (char= (first (second res)) #\:))    
        (list (first res) (delFirstN (second res) 1)))))

(defun parse-auth (s) 
  (if (and (not (null s))
       (doubleSlashT s))
      (let* ((x (delFirstN s 2))
              (ui (parse-userinfo1 x))
              (host (parse-host (second ui)))
              (port (parse-port (second host))))
         (list (first ui) (first host) (first port) (second port)))
    (s)))

(defun parse-userinfo1 (s)
  (let ((res (identificatore s)))
    (if (and (> (length res) 0)
             (eql (first (second res)) #\@))
        (list (first res) (delFirstN (second res) 1))
      (list nil s))))  

(defun parse-host (s)
  (if (not (null s))
      (let ((fide (identificatore-host s)))
        (if (not (null (car fide)))
            (let ((pide (parse-host-pide (second fide))))
              (if (not (null pide))
                  (list (append (car fide) (car pide)) (second pide))))))))

(defun parse-host-pide (s)
  (if (eql (first s) #\.)
      (let ((ide (parse-host (delFirstN s 1))))
        (if (not (null ide))
            (list (cons #\. (car ide)) (second ide))))
    (list nil s)))

(defun parse-port (s)
  (if (and (not (null s))
           (eql (car s) #\:))
      (let ((ds (digits (delFirstN s 1))))
        (cond ((null (car ds))
                (list nil s))
               (T ds)))))

(defun parse-pqf (s) s)

(defun identificatore (x)
  (caratteri x '(#\/ #\? #\# #\@ #\:)))

(defun identificatore-host (x)
  (caratteri x '(#\. #\/ #\? #\# #\@ #\:)))

(defun doubleSlashT (l)
  (and (char= (first l) (second l))
       (char= (first l) #\/)))
       
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

(defun digits (d)
  (cond ((and (not (null d))
         (numberp (digit-char-p (car d)))) 
         (let ((ds (digits (delFirstN d 1))))
           (list (append (list (car d)) (car ds)) (second ds))))
         (T (list nil d))))

(defun delFirstN (l n)
  (cond ((< n 0) nil)
        ((eq n 0) l)
        ((eq n 1) (cdr l))
        (T (delFirstN (cdr l) (- n 1)))))