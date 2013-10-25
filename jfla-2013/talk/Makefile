
slides.pdf: slides.tex
	rubber -d slides.tex

color.opt: color.ml
	ocamlopt -I +ocamlgraph -I +combine unix.cmxa graphics.cmxa graph.cmxa combine.cmxa $^ -o $@
