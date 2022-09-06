LFILE = README
VIGNETTE = autotest-control

all: init vignette

init:
	echo "pkgdown::init_site()" | R --no-save -q

vignette:
	echo "pkgdown::build_article('$(VIGNETTE)',quiet=FALSE)" | R --no-save -q


knith: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).html')" | R --no-save -q

knitr: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).md')" | R --no-save -q

open:
	xdg-open docs/articles/$(VIGNETTE).html &

check:
	Rscript -e 'library(pkgcheck); checks <- pkgcheck(); print(checks); summary (checks)'

clean:
	rm -rf docs/ *.html *.png README_cache 
