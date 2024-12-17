(in-package :adhoc-scene-graph)

(defvar *object-id->object-table*
  (make-hash-table :test #'eq :weakness :value :synchronized t))

(defun (setf krma::object-from-id) (object id)
  (sb-ext:with-locked-hash-table (*object-id->object-table*)
    (setf (gethash id *object-id->object-table*) object)))

(defun krma::object-from-id (id)
  (sb-ext:with-locked-hash-table (*object-id->object-table*)
    (gethash id *object-id->object-table*)))

;; start the counter for lisp objects at the end of half of int so we can use foreign handled ints
;; as oids directly without needing to access this counter (such as parasolid tags)
(defvar *object-id-counter* (expt 2 31))

(defun new-object-id ()
  (incf *object-id-counter*))

(defvar *group-counter* 0)

(defun new-group ()
  (incf *group-counter*))

(defun safe-euclid (vec4)
  (if (<= (abs (vw vec4)) single-float-epsilon)
      (vec3 (vx vec4) (vy vec4) (vz vec4))
      (vec3 (/ (vx vec4) (vw vec4))
	    (/ (vy vec4) (vw vec4))
	    (/ (vz vec4) (vw vec4)))))

#+NIL
(defgeneric rm-erase-node (node)
  (:documentation "Erases retained mode portion of a node and it's descendants.  Default method does nothing.")
  (:method (node) (declare (ignore node)) (values)))

#+NIL
(defgeneric rm-draw-node (node)
  (:documentation "Redraws retained mode portion of a node and it's descendants.  Does not delete old geometry. Default method does nothing.")
  (:method (node) (declare (ignore node)) (values)))



(defmethod rm-redraw-node (node)
  (rm-erase-node node)
  (rm-draw-node node)
  (values))

(defobject group-mixin ()
  :inputs
  ((group :defaulting)))

(defobject selectable-mixin ()
  :slots
  ((object-id :initform (new-object-id)))

  :inputs
  ((hovered? :visual nil)))

(defmethod initialize-instance :after ((self selectable-mixin) &rest initargs)
  (declare (ignorable initargs))
  (setf (krma::object-from-id (the object-id)) self)
  (values))

(defmethod forget-object (self)
  (values))

(defmethod forget-object ((self selectable-mixin))
  (sb-ext:with-locked-hash-table (*object-id->object-table*)
    (remhash (the object-id) *object-id->object-table*))
  (values))

(defun node-set-model-matrix (self)
  (let ((scene (the scene))
	(matrix (the global-matrix)))
    (mapcar #'(lambda (handle)
		(primitive-set-transform scene handle matrix))
	    (the primitive-handles))))

(defmethod clim:handle-event :around ((self selectable-mixin) (event clim:pointer-enter-event))
  (unless (the hovered?)
    (setf (the hovered?) t))
  (when (next-method-p)
    (call-next-method)))

(defmethod clim:handle-event :around ((self selectable-mixin) (event clim:pointer-exit-event))
  (when (the hovered?)
    (setf (the hovered?) nil))
  (when (next-method-p)
    (call-next-method)))

(defobject node-mixin ()
  :slots
  ((redraw? :initform nil)
   (retransform? :initform nil)
   (primitive-handles :initform ()))
  
  :inputs
  ((visible? :visual t)
   (origin (vec3 0 0 0))
   (rotation (quat 0 0 0 1))
   (scene :defaulting))

  :default-initargs (scene (krma:default-scene))
  
  :attributes
  ((rotation-matrix
    (matrix-from-quaternion (the rotation)))

   (translation-matrix
    (let* ((origin (the origin))
	   (x (vx origin))
	   (y (vy origin))
	   (z (vz origin)))
      (mat4 1 0 0 x
	    0 1 0 y
	    0 0 1 z
	    0 0 0 1)))


   (matrix (m* (the translation-matrix) (the rotation-matrix)))
   
   (global-matrix (if (null (the superior))
		      (the matrix)
		      (m* (the superior global-matrix) (the matrix))))

   (inverse-matrix (minv (the matrix)))

   (inverse-global-matrix (minv (the global-matrix)))
		    
   (node-children (remove-if-not #'(lambda (child)
				     (typep child 'node-mixin))
				 (the children)))))

(defmethod rm-draw-node :after ((node node-mixin))
  (setf (slot-value node 'redraw?) nil)
  (setf (slot-value node 'retransform?) nil)
  (values))

#+NIL
(defmethod initialize-instance :after ((instance node-mixin) &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  ;; this causes (the-part ...) not to work in component definitions
  ;; since rm-redraw-node depends on instantiating part,
  ;; and the act of instantiating part re-enters rm-redraw-node
  (rm-redraw-node instance)
  (values))

(defmethod initialize-instance :around ((instance node-mixin) &rest initargs
					&key &allow-other-keys)
  (declare (ignore initargs))
  (prog1 (call-next-method)
    (unless (superior instance)
      (bt:make-thread #'(lambda ()
			  (rm-draw-node-recursively instance))
		      :name "initial draw thread")
      (when (send instance scene)
	(setf (krma::node-children (send instance scene))
	      (append (krma::node-children (send instance scene)) (list instance)))))))

(defun rm-draw-node-recursively (node)
  (rm-draw-node node)
  (let ((children (send node children)))
    (mapcar #'rm-draw-node-recursively children)
    (values)))

(defun rm-redraw-node-recursively (node)
  (rm-redraw-node node)
  (let ((children (send node children)))
    (mapcar #'rm-redraw-node-recursively children)
    (values)))

(defun rm-maybe-draw-node-recursively (node)
  (when (typep node 'node-mixin)
    (unless (slot-value node 'primitive-handles)
      (rm-draw-node node)))
  (let ((children (send node children)))
    (mapcar #'rm-maybe-draw-node-recursively children)
    (values)))

(defun rm-maybe-draw-root-in-thread (node)
  (bt:make-thread #'(lambda ()
		      (rm-maybe-draw-node-recursively (send node root)))
		  :name "redraw thread"))
  
  
(defmethod add-to-scene (node &optional (scene (krma:default-scene)))
  (push node (krma::node-children scene))
  (values))  

(defun remove-from-scene (node &optional (scene (krma:default-scene)))
  (setf (krma::node-children scene)
	(remove node (krma::node-children scene)))
  (values))


(defmethod forget-object-recursively ((self node-mixin))
  (forget-object self)
  (mapcar #'forget-object-recursively (the node-children))
  (values))

(defobject group-owner-mixin (group-mixin)
  :slots
  ((group :initform (new-group) :accessor object-group)))

(defmethod initialize-instance ((self group-owner-mixin) &rest initargs)
  (declare (ignore initargs))
  (prog1 (call-next-method)
    (krma::scene-ensure-group (the scene) (the group))))

(defmethod node-parent ((self node-mixin))
  (the superior))

(defmethod rm-retransform-node ((self group-owner-mixin))
  (when (the scene)
    (krma::scene-ensure-group (the scene) (the group))
    (group-set-transform (the scene) (the group) (the global-matrix))))

(defmethod rm-retransform-node ((self node-mixin))
  (node-set-model-matrix self))

(defmethod rm-retransform-node (self)
  (values))

(defmethod rm-erase-node ((self node-mixin))
  (when (the scene)
    (when (the primitive-handles)
      (delete-primitives (the scene) (the primitive-handles))
      (setf (the primitive-handles) nil)))
  (values))

#+NIL
(defmethod rm-erase-node ((self group-owner-mixin))
  (delete-group (the scene) (the group) nil)
  (values))

(defmethod rm-erase-node-recursively (node)
  (declare (ignore node))
  (values))

(defmethod rm-erase-node-recursively ((self node-mixin))
  (rm-erase-node self)
  (mapcar #'rm-erase-node-recursively (the children))
  (values))

(defmethod rm-draw-node :before ((self node-mixin))
  (rm-retransform-node self)
  (values))

(defmethod translate-node ((self node-mixin) (vec vec3))
  (setf (the origin) (v+ (the origin) vec))
  (values))

(defgeneric transform-coordinate (coordinate node)
  (:documentation "Given a vector and a node, return a new vector which is the transformation of the original vector into the parent coordinate system of the node."))

(defmethod transform-coordinate ((vec 3dm.d:vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the matrix))))
    (safe-euclid vec4)))

(defmethod transform-coordinate ((vec 3dm.f:vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the matrix))))
    (safe-euclid vec4)))

(defgeneric global-transform-coordinate (coordinate node)
  (:documentation "Given a vector and a node, return a new vector which is the transformation of the original vector into the global coordinate system."))

(defmethod global-transform-coordinate ((vec 3dm.d::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the global-matrix))))
    (safe-euclid vec4)))

(defmethod global-transform-coordinate ((vec 3dm.f::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the global-matrix))))
    (safe-euclid vec4)))

(defgeneric untransform-coordinate (coordinate node)
  (:documentation "Given a vector and a node, where the vector is represented in the parent coordinate system of the node, return a new vector which is represented in the coordinate system of the node itself."))

(defmethod untransform-coordinate ((vec 3dm.d::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defmethod untransform-coordinate ((vec 3dm.f::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defmethod transform-coordinate-to ((vec 3dm.d::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defmethod transform-coordinate-to ((vec 3dm.f::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defgeneric untransform-global-coordinate (global-coordinate node)
  (:documentation "Given a global coordinate, return a new vector which is the the coordinate as represented in the node's coordinate system."))

(defmethod untransform-global-coordinate ((vec vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-global-matrix))))
    (safe-euclid vec4)))

;; newer api below:

(defun transform-coordinate-from-to-1 (coordinate from to)
  (declare (type 3dm.d::vec4 coordinate))
  (if (and (not from) (not to))
      coordinate
      (if (not from)
	  (m* coordinate (cl:the 3dm.d::mat4 (send to global-matrix)))
	  (if (not to)
	      (m* coordinate (cl:the 3dm.d::mat4 (send from inverse-global-matrix)))
	      (if (not (eq (send from root) (send to root)))
		  (error "Objects have no relationship.")
		  (let* ((from-root-path (send from root-path))
			 (to-root-path (send to root-path))
			 (relative-path (adhoc::compute-relative-path-1 from-root-path to-root-path))
			 (matrices (loop with matrices = ()
					 for message in relative-path
					 do (if (eq message 'superior)
						(progn
						  (push (send from inverse-matrix) matrices)
						  (setq from (send from (eval message))))
						(progn
						  (setq from (send from (eval message)))
						  (push (send from matrix) matrices)))
					 finally (return matrices))))
		    (loop for matrix in matrices
			  do (setq coordinate (m* (cl:the 3dm.d::vec4 coordinate)
						  (cl:the 3dm.d::mat4 matrix))))
		    coordinate))))))

(defgeneric transform-coordinate-from-to (coordinate from to)
  (:documentation "function `transform-coordinate-from-to' coordinate from to --> (from or to can be nil, meaning global), returns coordinate argument which is considered in coordinate system `from' transformed into coordinate system of `to'. `from' and `to' can be any node in a tree of nodes, as long as they are in the same tree.  nil means global coordinate system, which is not necessarily the same as root."))

(defmethod transform-coordinate-from-to ((coordinate 3dm.d::vec3) from to)
  (safe-euclid (transform-coordinate-from-to-1
		(vec4 (vx coordinate) (vy coordinate) (vz coordinate) 1.0d0)
		from to)))

(defmethod transform-coordinate-from-to ((coordinate 3dm.d::vec2) from to)
  (safe-euclid (transform-coordinate-from-to-1
		(vec4 (vx coordinate) (vy coordinate) 0.0d0 1.0d0)
		from to)))



(defgeneric transform-from-global-coordinate (coordinate to)
  (:documentation "function `transform-from-global-coordinate' coordinate to is equivalent to --> transform-coordinate-from-to coordinate nil to, returns coordinate, which is considered in the global coordinate system into coordinate system of `to'"))

(defmethod transform-from-global-coordinate ((coordinate 3dm.d::vec3) to)
  (safe-euclid
   (m* (vec4 (vx coordinate) (vy coordinate) (vz coordinate) 1.0d0)
       (send to global-matrix))))

(defmethod transform-from-global-coordinate ((coordinate 3dm.d::vec2) to)
  (safe-euclid
   (m* (vec4 (vx coordinate) (vy coordinate) 0.0d0 1.0d0)
       (send to global-matrix))))


(defgeneric transform-to-global-coordinate (coordinate from)
  (:documentation "function `transform-to-global-coordinate' coordinate from is equivalent to --> transform-coordinate-from-to coord from nil, returns coordinate, which is consider in the coordinate system of `from' as a global coordinate"))

(defmethod transform-to-global-coordinate ((coordinate 3dm.d::vec3) from)
  (safe-euclid
   (m* (vec4 (vx coordinate) (vy coordinate) (vz coordinate) 1.0d0)
       (send from inverse-global-matrix))))

(defmethod transform-to-global-coordinate ((coordinate 3dm.d::vec2) from)
  (safe-euclid
   (m* (vec4 (vx coordinate) (vy coordinate) 0.0d0 1.0d0)
       (send from inverse-global-matrix))))

(defmacro as (coordinate node)
  "macro `as' coordinate node is equivalent to --> transform-coordinate-from-to coordinate self node, returns coordinate, considered in the coordinate system of `self', into coordinates of coordinate system of `node'.  example (as (vec3 1 1 1) part) --> in a provided input transforms (vec3 1 1 1), wrt self, into coordinate of part (or child)"
  `(transform-coordinate-from-to ,coordinate self ,node))

(defmacro locally (&rest reference-chain)
  "macro `locally' &rest reference-chain, where node is (the ,@(butlast reference-chain)) and coordinate is (send node ,@(last reference-chain)) is equivalent to --> transform-coordinate-from-to coordinate node self, returns coordinate, considered in the coordinate system of `node' into coordinates of `self'. example: (locally last-box top-left-corner)"
  (let ((node-sym (gensym))
	(coordinate-sym (gensym)))
  `(let* ((,node-sym (the ,@(butlast reference-chain)))
	  (,coordinate-sym (send ,node-sym ,@(last reference-chain))))
     (transform-coordinate-from-to ,coordinate-sym ,node-sym self))))

(defmacro globally (coordinate)
  "macro `globally' coordinate is equivalent to --> (as coordinate nil) or (transform-to-global-coordinate coordinate self), returns coordinate, which is considered in the coordinate system of `self' into global coordinates"
  `(transform-to-global-coordinate ,coordinate self))











