##########################################################################
#                                                                        #
#  Combine - an OCaml library for combinatorics                          #
#                                                                        #
#  Copyright (C) 2012-2014                                               #
#    Remy El Sibaie                                                      #
#    Jean-Christophe Filliatre                                           #
#                                                                        #
#  This software is free software; you can redistribute it and/or        #
#  modify it under the terms of the GNU Library General Public           #
#  License version 2.1, with the special exception on linking            #
#  described in file LICENSE.                                            #
#                                                                        #
#  This software is distributed in the hope that it will be useful,      #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  #
#                                                                        #
##########################################################################


PACKAGES = combinelib combine


all: build


init:
	ocp-build -init || ocp-build init

build: init
	ocp-build build $(PACKAGES)

install: uninstall
	ocp-build install $(PACKAGES)
	@for p in $(PACKAGES) ; do \
		cp _obuild/$$p/*.cmi `ocamlfind query $$p` ; \
		echo "Copy cmi's for $$p." ; \
	done


examples: main
	ocp-build build sudoku queens backslide


test:
	ocp-build test 
	./_obuild/tests/tests.byte

uninstall: init
	@for p in $(PACKAGES) ; do \
		ocamlfind remove $$p ; \
	done

clean: init
	ocp-build clean
	rm -rf ocp-build.*


pkg:
	ocp2opam \
		-ocp src/combine.ocp \
		-url "https://github.com/backtracking"
