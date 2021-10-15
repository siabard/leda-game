;;;; leda.lisp

(cl:in-package :leda) 

(defparameter *canvas-width* 800)
(defparameter *canvas-height* 600)

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
(defmethod gk:draw ((app leda-game))
  (gk:draw-rect *origin* *canvas-width* *canvas-height* :fill-paint *black*)
  (gk:draw-text (write-to-string  *delta-time*) (gk:vec2 300 400) :fill-color *white*)
  (gk:draw-text (write-to-string  *last-time*) (gk:vec2 300 500) :fill-color *white*)
  (gk:draw-text "LEDA 게임" (gk:vec2 400 300) :fill-color *white* :font *undotum-32*)
  )
