filename="cp2021t"

lhs2TeX "$filename.lhs" > "$filename.tex"
xelatex -interaction batchmode "$filename"
bibtex "$filename"
xelatex -interaction batchmode "$filename"
xelatex -interaction batchmode "$filename"