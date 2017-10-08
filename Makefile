render:
	Rscript -e 'library(knitr); rmarkdown::render("index.Rmd", output_file = "index.html")'

serve:
	Rscript -e 'xaringan::infinite_moon_reader("index.Rmd")'

