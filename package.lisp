(in-package :adhoc)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defpackage :adhoc-scene-graph
  (:use :krma :3d-math)
  (:shadow cl:locally)
  
  (:import-from :3dm.d
		#:vec2
		#:vec3
		#:vec4
		#:mat2
		#:mat3
		#:mat4
		#:matn
		#:quat
		#:meye
		#:with-vec2
		#:with-vec3
		#:with-vec4
		#:with-vec)

  (:export #:node-mixin
	   #:group-mixin
	   #:selectable-mixin
	   #:group-owner-mixin
	   #:rm-draw-node
	   #:rm-erase-node
	   #:rm-redraw-node
	   #:matrix
	   #:global-matrix
	   #:origin
	   #:safe-euclid
	   #:group
	   #:hovered?
	   #:new-object-id
	   #:forget-object
	   #:node-set-model-matrix
	   #:redraw?
	   #:retransform?
	   #:primitive-handles
	   #:visible?
	   #:rotation
	   #:rotation-matrix
	   #:translation-matrix
	   #:inverse-matrix
	   #:inverse-global-matrix
	   #:node-children
	   #:object-id
	   #:*object-id->object-table*
	   #:rm-erase-node-recursively
	   #:forget-object-recursively
	   #:locally
	   #:as
	   #:transform-coordinate-from-to
	   #:transform-from-global-coordinate
	   #:transform-to-global-coordinate
	   #:globally))
	   
)
