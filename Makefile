EXECUTABLE := bin/ms-demo
.PHONY: $(EXECUTABLE)

$(EXECUTABLE):
	sbcl \
	--no-userinit \
	--eval '(load #p"~/quicklisp/setup.lisp")' \
        --eval '(push :release *features*)' \
	--eval '(asdf:make :marching-squares)'
