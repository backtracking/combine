
main-pp.pdf: insbox.tex main-pp.tex caml_problem.mps
	rubber -d main-pp.tex

caml_problem.mps: figures.ml
	mlpost -pdf figures.ml

main-pp.tex: main.tex
	latexpp -g color no -m of ocaml-tt -e ocaml ocaml-tt $^ -o $@

export: main-pp.pdf
	scp main-pp.pdf filliatr@moloch:WWW/pub/jfla-2013.pdf

paper9.tar.gz: main-pp.tex
	mkdir -p paper9
	cp -f main-pp.tex paper9/paper9.tex
	cp -f insbox.tex actes.sty *.mps zdd_construct2.pdf paper9/
	tar zcf paper9.tar.gz paper9/
