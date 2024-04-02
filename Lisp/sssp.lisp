; ************************
; ************************

; GOLUB OLEKSANDRA 856706

; ************************
; ************************

(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

; ************************
; ************************

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))


(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))
	  
	  
(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
	(list 'vertex graph-id vertex-id)))

	
(defun graph-vertices (graph-id)
  (let ((vertices ()))
    (maphash (lambda (k v)
               (when (and (equal 'vertex (first k))
                          (equal graph-id (second k)))
                 (setf vertices (cons (third k) vertices))))
             *vertices*)
    (nreverse vertices))) 
	
	
(defun new-edge (graph-id vertex1-id vertex2-id &optional (weight 1))
  (setf (gethash (list 'edge graph-id vertex1-id vertex2-id weight) 
															*edges*)
	(list 'edge graph-id vertex1-id vertex2-id weight)))


(defun graph-edges (graph-id)
  (let ((edges ()))
    (maphash (lambda (k v)
               (when (and (equal 'edge (first k))
                          (equal graph-id (second k)))
                 (setf edges (cons (list (second k) (third k) 
								(fourth k) (fifth k)) edges))))
             *edges*)
    (nreverse edges))) 


(defun graph-edges-sssp (graph-id v1 v2)
  (let ((edges ()))
    (maphash (lambda (k v)
               (when (and (equal 'edge (first k))
                          (equal graph-id (second k))
                          (equal v1 (third k))
                          (equal v2 (fourth k)))
                 (setf edges (cons (list (second k) (third k) 
								(fourth k) (fifth k)) edges))))
             *edges*)
    (nreverse edges))) ;


(defun graph-print (graph-id)
  (let (
	(l1 (graph-vertices graph-id))
	(l2 (graph-edges graph-id)))
   (print(append l1 l2))))

	
(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((neighbors ()))
    (maphash (lambda (k v)
               (when (and (equal 'edge (first k))
                          (equal graph-id (second k))
                          (equal vertex-id (third k)))
                 (setf neighbors (cons (list (third k) (fourth k) 
											(fifth k)) neighbors))))
             *edges*)
    (nreverse neighbors)))


(defun delete-vertices (graph-id)
  (let ((to-delete nil))
    (maphash (lambda (k v)
               (when (and (equal 'vertex (first k))
                          (equal graph-id (second k)))
                 (setf to-delete (cons k to-delete))))
             *vertices*)
    (labels ((remove-vertices (keys)
               (when keys
                 (remhash (car keys) *vertices*)
                 (remove-vertices (cdr keys)))))
      (remove-vertices to-delete))))


(defun delete-edges (graph-id)
  (let ((to-delete nil)) 
    (maphash (lambda (k v)
               (when (equal graph-id (second k))
                 (setf to-delete (cons k to-delete))))
             *edges*)
    (labels ((remove-edges (keys)
               (when keys
                 (remhash (car keys) *edges*)
                 (remove-edges (cdr keys)))))
      (remove-edges to-delete))))


(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (delete-vertices graph-id)
  (delete-edges graph-id))


; ************************
; ************************


(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
	    (list 'heap heap-id 0 (make-array capacity)))))


(defun heap-id (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (second l)))


(defun heap-size (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (third l)))


(defun heap-capacity (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (length(fourth l))))


(defun heap-actual-heap (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (fourth l)))


(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))


(defun heap-empty (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (cond ((= (third l) 0) t))))


(defun heap-not-empty (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (cond ((/= (third l) 0) t))))


(defun increase-heap-size (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (setf (third l) (+ (third l) 1))))


(defun decrease-heap-size (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (setf (third l) (- (third l) 1))))


(defun heap-insert-last (heap-id K V)
  (let (( l (gethash heap-id *heaps*))
	( p (heap-size heap-id)))
    (setf (aref (fourth l) p) (list K V))
    (increase-heap-size heap-id)
    t))


(defun heap-swap2 (heap-id p1 p0)
  (let (( l (heap-actual-heap heap-id)))
    (let (( k0 (first (aref l p0)))
	  ( k1 (first (aref l p1))))
      (cond
       ((< k1 k0)
	(let (( c (aref l p0)))
	  (setf (aref l p0) (aref l p1))
	  (setf (aref l p1) c)))))))


(defun heap-heapify3 (heap-id s)
  (cond
   ((= s 1) t)
   (t
    (heap-swap2 heap-id (- s 1) (- s 2))
    (heap-heapify3 heap-id (- s 1)))))


(defun heap-heapify (heap-id)
  (let (( s (heap-size heap-id)))
    (cond
     ((= s 1) t)
     ((= s 2) (heap-swap2 heap-id 1 0))
     ((> s 2) (heap-heapify3 heap-id s)))))


(defun heap-insert (heap-id K V)
  (let (( c (heap-capacity heap-id))
	( s (heap-size heap-id)))
    (cond
     ((< s c)
      (heap-insert-last heap-id K V)
      (heap-heapify heap-id)
       t)
     (t
      (print 'array-pieno)
      nil))))


(defun heap-swap-nill (heap-id p1 p0)
  (let (( l (heap-actual-heap heap-id)))
    (setf (aref l p0) (aref l p1))
    (setf (aref l p1) nil)))


(defun heap-swap-all (heap-id i s)
  (cond
   ((= i s) t)
   (t
    (heap-swap-nill heap-id (+ i 1) i)
    (heap-swap-all heap-id (+ i 1) s))))


(defun heap-heapify-extract (heap-id)
  (let (( s (heap-size heap-id)))
    (cond
     ((= s 0) t)
     ((= s 1) (heap-swap-nill heap-id 1 0))
     ((> s 1) (heap-swap-all heap-id 0 s)))))


(defun heap-extract (heap-id)
  (let (( l (heap-actual-heap heap-id)))
    (let ((  v (aref l 0)))
      (setf (aref l 0) nil)
      (decrease-heap-size heap-id)
      (heap-heapify-extract heap-id)
      v)))


(defun find-key (array s key)
  (cond
   ((< s 0) nil)
   (t
    (let (( k (first (aref array s))))
      (cond
       ((= k key) s)
       (t (find-key array (- s 1) key)))))))


(defun heap-shift-d (array s p)
  (cond
   ((= p s) t)
   ((> (first(aref array p)) (first(aref array (+ p 1))))
    (let (( c (aref array p)))
      (setf (aref array p) (aref array (+ p 1 )))
      (setf (aref array (+ p 1)) c)
      (heap-shift-d array s (+ p 1))))))


(defun heap-shift-s (array s p)
  (cond
   ((= p 0) t)
   ((< (first(aref array p)) (first(aref array (- p 1))))
    (let (( c (aref array p)))
      (setf (aref array p) (aref array (- p 1)))
      (setf (aref array (- p 1)) c)
      (heap-shift-s array s (- p 1))))))


(defun heap-shift (array s p)
  (cond
   ((= p 0) (heap-shift-d array s p))
   ((= p s) (heap-shift-s array s p))
   (t
    (heap-shift-d array s p)
    (heap-shift-s array s p))))


(defun heap-modify-key (heap-id new-key old-key)
  (let (( l (heap-actual-heap heap-id))
	( e (find-key
	     (heap-actual-heap heap-id)
	     (- (heap-size heap-id) 1)
	     old-key)))
    (cond
     ((eql e nil) nil)
     (t
      (let ( (v (second(aref l e))))
	(setf (aref l e) (list new-key v))
	(heap-shift l (- (heap-size heap-id) 1) e)
	t)))))


(defun heap-head (heap-id)
  (aref (heap-actual-heap heap-id) 0))


(defun heap-print (heap-id)
  (print(gethash heap-id *heaps*)))


; ************************
; ************************


(defun set-distance (graph-id vertex-id distance)
  (setf (gethash (list graph-id vertex-id) *distances*) distance))


(defun get-distance (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *distances*))


(defun set-visited (graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *visited*) t))


(defun visited-p (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *visited*))

(defun set-previous (graph-id vertex-id previous-vertex-id)
  (setf (gethash (list graph-id vertex-id) *previous*) previous-vertex-id))


(defun get-previous (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *previous*))
  
  
(defun sssp-dist (graph-id vertex-id)
  (get-distance graph-id vertex-id))


(defun sssp-visited (graph-id vertex-id)
  (visited-p graph-id vertex-id))


(defun sssp-previous (graph-id vertex-id)
  (get-previous graph-id vertex-id))
  
  
(defun change-distance (graph-id vertex-id new-dist)
  (set-distance graph-id vertex-id new-dist))  
  
  
(defun change-previous (graph-id vertex-id new-previous)
  (set-previous graph-id vertex-id new-previous))
  
  
(defun sssp-change-dist (graph-id vertex-id new-dist)
  (change-distance graph-id vertex-id new-dist))


(defun sssp-change-previous (graph-id vertex-id new-previous)
  (change-previous graph-id vertex-id new-previous))
  
  
(defun process-neighbors 
  (neighbors current-distance graph-id current-vertex queue)
  (when neighbors
    (let* ((edge (first neighbors))
           (neighbor (second edge))
           (weight (third edge))
           (new-distance (+ current-distance weight)))
      (when (< new-distance (get-distance graph-id neighbor))
        (set-distance graph-id neighbor new-distance)
        (set-previous graph-id neighbor current-vertex)
        (heap-insert queue new-distance neighbor)))
    (process-neighbors (rest neighbors) current-distance 
							graph-id current-vertex queue)))

(defun process-vertices (queue graph-id)
  (when (heap-not-empty queue)
    (let* ((current-pair (heap-extract queue))
           (current-distance (first current-pair))
           (current-vertex (second current-pair)))
      (unless (visited-p graph-id current-vertex)
        (set-visited graph-id current-vertex)
        (process-neighbors 
			(graph-vertex-neighbors graph-id current-vertex) 
				current-distance graph-id current-vertex queue))
      (process-vertices queue graph-id))))


(defun sssp-dijkstra (graph-id source-vertex-id)
  (let ((vertices (graph-vertices graph-id)))
    (labels ((process-vertex (vertex-list)
               (when vertex-list  
                 (let ((vertex (first vertex-list)))
                   (set-distance graph-id vertex 
					(if (equal vertex source-vertex-id) 
                                            0 most-positive-fixnum))
                   (setf (gethash (list graph-id vertex) *visited*) nil)
                   (process-vertex (rest vertex-list))))))
      (process-vertex vertices))
    (new-heap "priority-queue")
    (heap-insert "priority-queue" 0 source-vertex-id)
    (process-vertices "priority-queue" graph-id))
  nil)
		
		
(defun build-path (graph-id current source-vertex-id path)
  (if (equal current source-vertex-id)
      (nreverse (push current path))
      (let ((prev (get-previous graph-id current)))
        (if prev
            (build-path graph-id prev source-vertex-id 
					(push (list prev current (get-distance 
										graph-id current)) path))
            (return-from build-path nil)))))


(defun sssp-shortest-path (graph-id source-vertex-id target-vertex-id)
  (build-path graph-id target-vertex-id source-vertex-id '()))	