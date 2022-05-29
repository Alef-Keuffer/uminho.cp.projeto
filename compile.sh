filename="cp2021t"

lhs2TeX "$filename.lhs" > "$filename.tex"
xelatex -interaction batchmode "$filename"
bibtex "$filename"
makeindex "$filename"
xelatex -interaction batchmode "$filename"
xelatex -interaction batchmode "$filename"

rm -f *.tex *.log *.synctex.gz *.out *.aux *.bbl *.idx *.ilg *.ind *.ptb *.blg