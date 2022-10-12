OCBFLAGS := -r -Is util,x86
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top profile

all: main.native generated factorial_handwritten
debug: all main.cma

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

generated.s: main.native
	./main.native &> generated.s

runtime.s: runtime.c
	gcc -O0 -fno-asynchronous-unwind-tables -S runtime.c

generated: generated.s runtime.s
	gcc -o generated runtime.s generated.s 

factorial_handwritten: factorial_handwritten.s driver.c
	gcc -o factorial_handwritten factorial_handwritten.s driver.c

.FORCE:

clean:
	$(OCB) -clean
	rm -f generated.s generated runtime.s factorial_handwritten

top: x86.byte
	utop

