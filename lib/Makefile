include ../mozip.inc

PWD=$(shell pwd)

coms=src

all:
	for proj in ${coms}; do cd ${PWD}/$$proj; make; done

install:
	for proj in ${coms}; do cd ${PWD}/$$proj; make install; done

clean:
	for proj in ${coms}; do cd ${PWD}/$$proj; make clean; done

cleanall:
	for proj in ${coms}; do cd ${PWD}/$$proj; make cleanall; done
