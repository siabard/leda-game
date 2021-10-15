;;;; leda.asd

(asdf:defsystem :leda
  :description "Describe leda here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit cl-tiled)
  :components ((:file "package")
               (:file "leda")))
