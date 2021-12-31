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
        (list (first res) (cdr (second res))))))

(defun parse-auth (s) 
  (if (and (not (null s))
       (doubleSlashT s))
      (let* ((x (cdr (cdr s))) ;(delFirstN s 2)
             (ui (parse-userinfo1 x))
             (host (parse-host (second ui)))
             (port (parse-port (second host))))
        (list (first ui)
              (first host)
              (first port)
              (second port)))
    (s)))

(defun parse-pqf (s) 
  (cond (((null s) nil)
         (eql (car s) #\/)
         (let* ((path (parse-path (cdr s)))
                (query (parse-query (second path)))
                (frgmt (parse-fragment (second query))))
           (list (first path)
                 (first query) 
                 (first frgmt) 
                 (second frgmt))))                      
         (T (list nil s))))

(defun parse-userinfo1 (s)
  (let ((res (identificatore s)))
    (if (and (> (length res) 0)
             (eql (first (second res)) #\@))
        (list (first res) (cdr (second res)))
      (list nil s))))  

(defun parse-host (s)
  (let (ip (parse-hostIP s))
    (if (not (null (car ip)))
        (ip)
      (parse-hostID s))))

(defun parse-hostIP (s n) 
  (if (not (null s))
      (cond ((= n 1)
             (oct s))
            (T 
             (let ((o (oct s)))
               (if (not (null (car o)))
                   (let ((octs (parse-hostIP-poct (second o) 
                                                  (- n 1))))
                     (if (not (null (car octs)))
                         (list (append (car o) (car octs)) 
                               (second octs))))))))))

(defun parse-hostIP-poct (s n)
  (if (eql (first s) #\.)
      (let ((oct (parse-hostIP (cdr s) n)))
        (if (not (null oct))
            (list (cons #\. (car oct)) (second oct))))
    (list nil s)))


(defun oct (s)
  (let* ((ds (digits s))
         (octn (chrl-to-int (car ds))))
    (if (and (not (null octn))
             (= (length (car ds)) 3)
             (<= 0 octn 255))
        ds)))

(defun chrl-to-int (l)
  (cond ((or (null l)
             (not (numberp (digit-char-p (car l)))))
         nil)
        ((null (cdr l))
         (digit-char-p (car l)))
        (T 
         (let ((o (chrl-to-int (cdr l))))
           (if (not (null o))
               (+ (* (digit-char-p (car l)) 
                     (expt 10 (- (length l) 1))) 
                  o))))))
            
(defun parse-hostID (s)
  (if (not (null s))
      (let ((fide (identificatore-host s)))
        (if (not (null (car fide)))
            (let ((pide (parse-hostID-pide (second fide))))
              (if (not (null pide))
                  (list (append (car fide) (car pide)) (second pide))))))))

(defun parse-hostID-pide (s)
  (if (eql (first s) #\.)
      (let ((ide (parse-hostID (cdr s))))
        (if (not (null ide))
            (list (cons #\. (car ide)) (second ide))))
    (list nil s)))

(defun parse-port (s)
  (cond ((and (not (null s))
           (eql (car s) #\:))
         (let ((ds (digits (cdr s ))))
           (cond ((null (car ds))
                  (list 80 s))
                 (T ds))))
         (T (list 80 s))))

(defun parse-path (s)
  )
  

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
         (let ((ds (digits (cdr d))))
           (list (append (list (car d)) (car ds)) (second ds))))
         (T (list nil d))))

(defun delFirstN (l n)
  (cond ((< n 0) nil)
        ((eq n 0) l)
        ((eq n 1) (cdr l))
        (T (delFirstN (cdr l) (- n 1)))))