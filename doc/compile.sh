#!/bin/sh

Rscript -e "library('knitr')" -e "knit('doc.Rnw')"
pdflatex "doc.tex"
pdflatex "doc.tex"
