(require 'sb-posix)

(push (merge-pathnames "lib/" (values *default-pathname-defaults*))
      asdf:*central-registry*)

(asdf:oos 'asdf:load-op 'rlx)

(sb-ext:save-lisp-and-die "run-blast"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
					       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (rlx:play "blast")
				      0)
			  :executable t)

