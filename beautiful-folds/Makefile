slides: slides.html slides.pdf

slides.html: slides.md
	pandoc -t slidy -s slides.md -o slides.html

slides.pdf: slides.md
	pandoc -t beamer -s slides.md -o slides.pdf -V classoption=t -V classoption=9pt
