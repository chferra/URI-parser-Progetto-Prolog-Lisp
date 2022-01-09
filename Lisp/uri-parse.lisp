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
         (scheme (parse-scheme charList)))
    (cond ((null scheme) nil)
          ((member-string (car scheme) (list "mailto"
                                             "news"
                                             "tel"
                                             "fax"
                                             "zos"))
           (uri-parseU2 scheme))
          (T (uri-parseU1 scheme)))))

(defun uri-parseU1 (scheme)
  (let* ((auth (parse-auth (second scheme)))
         (pqf (parse-pqf (fourth auth))))
    (cond ((null (fourth pqf))
           (make-uri-structure
            :scheme (first scheme)
            :userinfo (first auth)
            :host (second auth)
            :port (third auth)
            :path (first pqf)
            :query (second pqf)
            :fragment (third pqf))))))

(defun uri-parseU2 (scheme)
  (cond ((equalp (car scheme) "mailto")
         (parse-mailto scheme))
        ((equalp (car scheme) "news")
         (parse-news scheme))
        ((or (equalp (car scheme) "tel")
             (equalp (car scheme) "fax"))
         (parse-telfax scheme))
        ((equalp (car scheme) "zos")
         (parse-zos scheme))))

(defun uri-scheme (uriS-p)
  (cond ((uri-structure-p uriS-p)
         (uri-structure-scheme uriS-p))))

(defun uri-userinfo (uriS-p)
  (cond ((uri-structure-p uriS-p)
         (uri-structure-userinfo uriS-p))))

(defun uri-host (uriS-p)
  (cond ((uri-structure-p uriS-p)
         (uri-structure-host uriS-p))))

(defun uri-port (uriS-p)
  (cond ((uri-structure-p uriS-p)
         (uri-structure-port uriS-p))))

(defun uri-path (uriS-p)
  (cond ((uri-structure-p uriS-p)
         (uri-structure-path uriS-p))))

(defun uri-query (uriS-p)
  (cond ((uri-structure-p uriS-p)
         (uri-structure-query uriS-p))))

(defun uri-fragment (uriS-p)
  (cond ((uri-structure-p uriS-p)
         (uri-structure-fragment uriS-p))))

(defun uri-display (uriS-p &optional (out *standard-output*))
  (cond ((uri-structure-p uriS-p)
         (format out (concatenate 'string "URI display: ~%" 
                                  (string #\tab) "Schema: ~S~%"
                                  (string #\tab) "Userinfo: ~S~%"
                                  (string #\tab) "Host: ~S~%"
                                  (string #\tab) "Port: ~S~%"
                                  (string #\tab) "Path: ~S~%"
                                  (string #\tab) "Query: ~S~%"
                                  (string #\tab) "Fragment: ~S~%") 
                (uri-structure-scheme uriS-p)
                (uri-structure-userinfo uriS-p)
                (uri-structure-host uriS-p)
                (uri-structure-port uriS-p)
                (uri-structure-path uriS-p)
                (uri-structure-query uriS-p)
                (uri-structure-fragment uriS-p)) T)))

(defun parse-mailto (scheme)
  (let* ((ui (parse-userinfo2 scheme))
         (host (parse-host2 ui)))
    (cond ((null (second host))
           (make-uri-structure
            :scheme (first scheme)
            :userinfo (first ui)
            :host (first host))))))

(defun parse-news (scheme)
  (let ((host (parse-host (second scheme))))
    (cond ((null (second host))
           (make-uri-structure
            :scheme (first scheme)
            :host (first host))))))

(defun parse-telfax (scheme)
  (let ((ui (parse-userinfo2 scheme)))
    (cond ((null (second ui))
           (make-uri-structure
            :scheme (first scheme)
            :userinfo (first ui))))))

(defun parse-zos (scheme)
  (let* ((auth (parse-auth (second scheme)))
         (zpqf (parse-zpqf (fourth auth))))
    (cond ((and (not (null zpqf))
                (null (fourth zpqf)))
           (make-uri-structure
            :scheme (first scheme)
            :userinfo (first auth)
            :host (second auth)
            :port (third auth)
            :path (first zpqf)
            :query (second zpqf)
            :fragment (third zpqf))))))

(defun parse-zpqf (s)
  (cond ((eql (car s) #\/)
         (let ((zpath (parse-zos-path (cdr s))))
           (cond ((not (null zpath))
                  (let* ((query (parse-query (second zpath)))
                         (frgmt (parse-fragment (second query))))
                    (list
                     (chrl-to-string (first zpath))
                     (chrl-to-string (first query))
                     (chrl-to-string (first frgmt))
                     (second frgmt)))))))))

(defun parse-zos-path (s)
  (let* ((id44 (parse-id44 s)))
    (cond ((null (car id44)) nil)
          ((eql (car (second id44)) #\()
           (let ((id8 (parse-id8 (cdr (second id44)))))
             (cond ((and (eql (car (second id8)) #\))
                         (not (null (car id8)))
                         (<= (length (car id8)) 8))
                    (list
                     (chrl-to-string (append (car id44)
                                             '(#\()
                                             (car id8)
                                             '(#\))))
                     (cdr (second id8))))
                   (T (list (chrl-to-string (car id44))
                            (second id44))))))
          (T (list
              (chrl-to-string (car id44))
              (second id44))))))

(defun parse-id8 (s)
  (cond ((alpha-char-p (car s))
         (caratteriAN s))))

(defun parse-id44 (s)
  (cond ((and (not (null s))
              (alpha-char-p (car s)))
         (let ((res (parse-id44-Cs s)))
           (cond ((<= (length (car res)) 44)
                  res))))))

(defun parse-id44-Cs (s)
  (let ((fcs (caratteriAN s)))
    (cond ((not (null (car fcs)))
           (let ((pcs (parse-id44-pCs (second fcs))))
             (cond ((not (null pcs))
                    (list (append (car fcs) (car pcs)) (second pcs)))))))))

(defun parse-id44-pCs (s)
  (cond ((eql (first s) #\.)
         (let ((cs (parse-id44-Cs (cdr s))))
           (cond ((not (null cs))
                  (list (cons #\. (car cs)) (second cs))))))
        (T (list nil s))))

(defun parse-scheme (l)
  (let ((res (identificatore l)))
    (cond ((and (> (length (car res)) 0)
                (char= (first (second res)) #\:))
           (list (chrl-to-string (first res)) (cdr (second res)))))))

(defun parse-auth (s)
  (cond ((and (not (null s))
              (doubleSlashT s))
         (let* ((x (cdr (cdr s)))
                (ui (parse-userinfo1 x))
                (host (parse-host (second ui)))
                (port (parse-port host)))
           (cond ((not (null (car host)))
                  (list (first ui)
                        (first host)
                        (first port)
                        (second port)))
                 (T (list nil nil 80 s)))))
        (T (list nil nil 80 s))))

(defun parse-pqf (s)
  (cond ((eql (car s) #\/)
         (let* ((path (parse-path (cdr s)))
                (query (parse-query (second path)))
                (frgmt (parse-fragment (second query))))
           (list (chrl-to-string (first path))
                 (first query)
                 (first frgmt)
                 (second frgmt))))
        (T (list nil nil nil s))))

(defun parse-userinfo1 (s)
  (let ((res (identificatore s)))
    (cond ((and (> (length (car res)) 0)
                (eql (first (second res)) #\@))
           (list (chrl-to-string (car res)) (cdr (second res))))
          (T (list nil s)))))

(defun parse-userinfo2 (s)
  (let ((res (identificatore (second s))))
    (cond ((> (length res) 0)
           (list (chrl-to-string (first res)) (second res)))
          (T (list nil s)))))

(defun parse-host2 (s)
  (cond ((and (not (null (car s)))
              (eql (car (second s)) #\@))
         (let ((host (parse-host (cdr (second s)))))
           (cond ((not (null (car host)))
                  (list (chrl-to-string (car host)) (second host)))
                 (T (list nil (second s))))))
        (T (list nil (second s)))))

(defun parse-host (s)
  (let ((ip (parse-hostIP s 4)))
    (cond ((not (null (car ip)))
           (list (chrl-to-string (car ip)) (second ip)))
          (T (let ((id (parse-hostID s)))
               (cond ((not (null (car id)))
                      (list (chrl-to-string (car id)) (second id)))
                     (T (list nil s))))))))

(defun parse-hostIP (s n)
  (cond ((= n 1)
         (oct s))
        (T
         (let ((o (oct s)))
           (cond ((not (null (car o)))
                  (let ((octs (parse-hostIP-poct (second o)
                                                 (- n 1))))
                    (cond ((not (null (car octs)))
                           (list (append (car o) (car octs))
                                 (second octs)))))))))))

(defun parse-hostIP-poct (s n)
  (cond ((eql (first s) #\.)
         (let ((oct (parse-hostIP (cdr s) n)))
           (cond ((not (null oct))
                  (list (cons #\. (car oct)) (second oct))))))
        (T (list nil s))))

(defun oct (s)
  (let* ((ds (digits s))
         (octn (chrl-to-int (car ds))))
    (cond ((and (not (null octn))
                (= (length (car ds)) 3)
                (<= 0 octn 255))
           ds))))

(defun chrl-to-int (l)
  (cond ((or (null l)
             (not (numberp (digit-char-p (car l)))))
         nil)
        ((null (cdr l))
         (digit-char-p (car l)))
        (T
         (let ((o (chrl-to-int (cdr l))))
           (cond ((not (null o))
                  (+ (* (digit-char-p (car l))
                        (expt 10 (- (length l) 1)))
                     o)))))))

(defun chrl-to-string (l)
  (cond ((not (null l))
         (coerce l 'string))))

(defun member-string (s l)
  (not (null (member T (mapcar #'(lambda (x) (equalp s x)) l)))))

(defun parse-hostID (s)
  (let ((fide (identificatore-host s)))
    (cond ((not (null (car fide)))
           (let ((pide (parse-hostID-pide (second fide))))
             (cond ((not (null pide))
                    (list (append (car fide) (car pide)) (second pide)))))))))

(defun parse-hostID-pide (s)
  (cond ((eql (first s) #\.)
         (let ((ide (parse-hostID (cdr s))))
           (cond ((not (null ide))
                  (list (cons #\. (car ide)) (second ide))))))
        (T (list nil s))))

(defun parse-port (s)
  (cond ((and (not (null (car s)))
              (eql (car (second s)) #\:))
         (let ((ds (digits (cdr (second s)))))
           (cond ((null (car ds))
                  (list 80 (second s)))
                 (T (list (chrl-to-int (car ds)) (second ds))))))
        (T (list 80 (second s)))))

(defun parse-path (s)
  (let ((fide (identificatore s)))
    (cond ((not (null (car fide)))
           (let ((side (parse-path-sp (second fide))))
             (cond ((not (null (car side)))
                    (list (append (car fide) (car side))
                          (second side)))
                   (T fide))))
          (T (list nil s)))))

(defun parse-path-sp (s)
  (cond ((eql (first s) #\/)
         (let ((ide (parse-path (cdr s))))
           (cond ((not (null (car ide)))
                  (list (cons #\/ (car ide)) (second ide))))))
        (T (list nil s))))

(defun parse-query (s)
  (cond ((eql (first s) #\?)
         (let ((q (caratteri (cdr s) '(#\#))))
           (cond ((not (null (car q)))
                  (list (chrl-to-string (car q)) (second q)))
                 (T (list nil s)))))
        (T (list nil s))))

(defun parse-fragment (s)
  (cond ((eql (first s) #\#)
         (let ((f (caratteri (cdr s) '())))
           (cond ((not (null (car f)))
                  (list (chrl-to-string (car f)) (second f)))
                 (T (list nil s)))))
        (T (list nil s))))

(defun identificatore (x)
  (caratteri x '(#\/ #\? #\# #\@ #\:)))

(defun identificatore-host (x)
  (caratteri x '(#\. #\/ #\? #\# #\@ #\:)))

(defun doubleSlashT (l)
  (and (equal (first l) (second l))
       (equal (first l) #\/)))

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
            (member i sub-delims)
            (eql i #\Space))
           (let ((res (caratteri (cdr chrs) filtri)))
             (cond ((eql i #\Space)
                    (list (append (list #\% #\2 #\0) (car res)) (second res)))
                   (T
                    (list (append (list i) (car res)) (second res))))))
          (T (list nil chrs)))))

(defun caratteriAN (chrs)
  (let ((i (car chrs))
        (rest (cdr chrs)))
    (cond ((null i)
           (list nil chrs))
          ((or
            (alphanumericp i)
            (eql i #\Space))
           (let ((res (caratteriAN (cdr chrs))))
             (cond ((eql i #\Space)
                    (list (append (list #\% #\2 #\0) (car res)) (second res)))
                   (T
                    (list (append (list i) (car res)) (second res))))))
          (T (list nil chrs)))))

(defun digits (d)
  (cond ((and (not (null d))
              (numberp (digit-char-p (car d))))
         (let ((ds (digits (cdr d))))
           (list (append (list (car d)) (car ds)) (second ds))))
        (T (list nil d))))
