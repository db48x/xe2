(require 'sb-posix)

(push (merge-pathnames "lib/" (values *default-pathname-defaults*))
      asdf:*central-registry*)

(asdf:oos 'asdf:load-op 'rlx)

(sb-ext:save-lisp-and-die "run-xong"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
					       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (rlx:play "xong")
				      0)
			  :executable t)

