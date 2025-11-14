;;; sbcl --load deploy.lisp

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable T :compression T))

(asdf:operate 'asdf:build-op :obj3)
