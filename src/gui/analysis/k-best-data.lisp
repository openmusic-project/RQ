(in-package :rq)

;;; Data used by the k-best algorithm. Is an attribute to each segment.

(defclass! k-best-data ()
  ((tempo :accessor tempo :initarg :tempo :initform nil) ; can be either a 2-elements list (tempo range), a number (goal value), or nil (default tempo range)
   (tempomerge :accessor tempomerge :initarg :tempomerge :initform t) ; when nil, the solutions are ranked for each tempo independently. when t, the ranking is global, on all possible tempi.
   (schema :accessor schema :initarg :schema :initform '(( ((2 3) (2 3) 2)   (5 (2 3) 2)   ((7 11 13)) )) ) ; subdivision schema 
   (n-solution :accessor n-solution :initarg :n-solution :initform 10) ; number of solutions computed for each hashtable
   (precision :accessor precision :initarg :precision :initform 0.5) 
   (gracepen :accessor gracepen :initarg :gracepen :initform 2) ;penalty given to the gracenotes
   (segments=beats :accessor segments=beats :initarg :segments=beats :initform nil) ;true when each segment corresponds to a beat
   (n-beats-per-seg :accessor n-beats-per-seg :initarg :n-beats-per-seg :initform 1) ;number of beats per segments (only used when segments=beats is true).
   (updateflag :accessor updateflag :initarg :updateflag :initform nil) ;true iff the k-best structure is up-to-date
   (slur :accessor slur  :initarg :slur :initform nil ) ;slur is nil if no note in the previous segment overlaps with the current segment. otherwise, it is by how much the previous segment overlaps (in ms)
   (grid :accessor grid  :initarg :grid-poly :initform nil))) ;quantification grid, to be displayed

(defmethod k-best-data-equal ((data1 k-best-data) (data2 k-best-data))
  "True iff DATA1 and DATA2 have the same parameters"
  (and (equalp (tempo data1) (tempo data2))
       (equalp (tempomerge data1) (tempomerge data2))
       (equalp (schema data1) (schema data2))
       (equalp (n-solution data1) (n-solution data2))
       (equalp (precision data1) (precision data2))
       (equalp (gracepen data1) (gracepen data2))
       (equalp (segments=beats data1) (segments=beats data2))
       (if (segments=beats data1)
           (equalp (n-beats-per-seg data1) (n-beats-per-seg data2))
         t))
)

(defmethod k-best-data-copy ((data k-best-data))
  (make-instance 'k-best-data
                 :tempo (tempo data)
                 :tempomerge (tempomerge data)
                 :schema (schema data)
                 :n-solution (n-solution data)
                 :precision (precision data)
                 :updateflag (updateflag data)
                 :slur (slur data)
                 :grid (grid data)
                 :segments=beats (segments=beats data)
                 :n-beats-per-seg (n-beats-per-seg data))
)

(defmethod om::omng-save ((self k-best-data) &optional values?)
  `(make-instance 'k-best-data
                  :tempo ',(tempo self)
                  :tempomerge ',(tempomerge self)
                  :schema ',(schema self)
                  :n-solution ,(n-solution self)
                  :precision ,(precision self)
                  :gracepen ,(gracepen self)
                  :updateflag ,(updateflag self)
                  :slur ,(slur self)
                  :grid ',(grid self)
                  :segments=beats ,(segments=beats self)
                  :n-beats-per-seg ,(n-beats-per-seg self)))
