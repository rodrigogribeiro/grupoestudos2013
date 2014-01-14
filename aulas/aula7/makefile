default: epf-aula7.pdf

epf-aula7.pdf: 
	pandoc -t beamer -V theme:Luebeck Main.lhs -o epf-aula7.pdf
exec: execclean
	ghc --make Main.lhs -o expr
execclean:
	rm *.o
	rm *.hi
	rm expr*

clean:
	rm *.pdf
