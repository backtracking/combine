
main.pdf: main.tex caml_problem.mps
	rubber -d main.tex

caml_problem.mps: figures.ml
	mlpost -pdf figures.ml
