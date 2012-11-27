
main-pp.pdf: insbox.tex main-pp.tex caml_problem.mps
	rubber -d main-pp.tex

caml_problem.mps: figures.ml
	mlpost -pdf figures.ml

main-pp.tex: main.tex
	latexpp -g color no -m of ocaml-tt -e ocaml ocaml-tt $^ -o $@

export: main-pp.pdf
	scp main-pp.pdf filliatr@moloch:WWW/pub/jfla-2013.pdf
