(defstruct uri-structure
  scheme
  userinfo
  host
  (port 80 :type integer)
  path
  query
  fragment)

(defun uri-parse (S)
  (coerce S 'list)) 