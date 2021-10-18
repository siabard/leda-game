;;;; leda.lisp

(cl:in-package :leda) 

(defparameter *canvas-width* 640)
(defparameter *canvas-height* 480)

(defparameter *origin* (gk:vec2 0 0))
(defparameter *black* (gk:vec4 0 0 0 1))
(defparameter *white* (gk:vec4 1 1 1 1))

(defparameter *last-time* 0)
(defparameter *delta-time* 0.0)

;;;; define assets directory
(gk:register-resource-package :keyword
                              (asdf:system-relative-pathname :leda "assets/"))


;;;; assets

;;; True Type fonts

(gk:define-font :undotum "fonts/undotum.ttf")

(defparameter *undotum-32* nil)


;;; Image
(gk:define-image :grass-tile "bitmaps/grass_tile.png")

;;;; game object

(gk:defgame leda-game () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Hello Gamekit!"))



;;; init
(defmethod gamekit:post-initialize ((app leda-game))
  (setf *undotum-32*  (gk:make-font ':undotum 32)))

;;; update logic
(defmethod gk:act ((app leda-game))
  (let ((current-time (get-internal-real-time)))
    (when (> current-time *last-time*)
      (progn
	(setf *delta-time* (/  (- current-time *last-time*) 1000000.0))
	(setf *last-time* current-time)))))


;;; render game
;; 세상에.. 그러니까 defmethod에서 불리기만하면
;; 아무렇지도 않게 generic이 먹는다는 거잖아..
(defun draw-tile ()
  (gk:draw-image (gk:vec2 (/ *canvas-width* 2) (/ *canvas-height* 2)) ':grass-tile
		 :width (gk:image-width ':grass-tile)
		 :height (gk:image-height ':grass-tile)))

(defun draw-full (image-id width height)
  (let ((rows (/ *canvas-height* height))
	(cols (/ *canvas-width* width)))
    (loop for y below rows
	  do (loop for x below cols
		   do (gk:draw-image (gk:vec2 (* x width) (* y height))
				     image-id
				     :width width
				     :height height)))))

(defun draw-full-2 (image-id width height)
  (let* ((rows (floor *canvas-height* height))
	 (cols (floor *canvas-width* width))
	 (cells (* rows cols)))
    (loop for i below cells
	  do (let ((x (mod i cols))
		   (y (floor i cols)))
	       (gk:draw-image (gk:vec2 (* x width) (* y height))
			      image-id
			      :width width
			      :height height)))))


(defun draw-all ()
  (progn
    (gk:draw-rect *origin* *canvas-width* *canvas-height* :fill-paint *black*)
    (draw-full-2 ':grass-tile (gk:image-width ':grass-tile) (gk:image-height ':grass-tile))
    (gk:draw-text (write-to-string  (/ 1 *delta-time*)) (gk:vec2 300 400) :fill-color *white*)
    (gk:draw-text (write-to-string  *last-time*) (gk:vec2 300 500) :fill-color *white*)
    (gk:draw-text "LEDA 게임" (gk:vec2 400 300) :fill-color *white* :font *undotum-32*)
))


(defmethod gk:draw ((app leda-game))
  (draw-all)
  )
