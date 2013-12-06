default: epf-aula4.pdf

epf-aula4.pdf: clean
	pandoc -t beamer Main.lhs -V theme:Luebeck -o epf-aula4.pdf
	pdflatex lista2.tex

view: clean epf-aula4.pdf
	evince epf-aula4.pdf & 

clean:
	rm *.pdf
	rm *.log
	rm *.aux
	pdflatex lista2.tex
