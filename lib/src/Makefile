include ../../mozip.inc

PWD=$(shell pwd)

#coms=flib lapack fftw scalapack
coms=flib

all:
	for proj in ${coms}; do cd ${PWD}/$$proj; make; make install; done

install:
	for proj in ${coms}; do cd ${PWD}/$$proj; make install; done

clean:
	for proj in ${coms}; do cd ${PWD}/$$proj; make clean; done

cleanall:
	for proj in ${coms}; do cd ${PWD}/$$proj; make cleanall; done