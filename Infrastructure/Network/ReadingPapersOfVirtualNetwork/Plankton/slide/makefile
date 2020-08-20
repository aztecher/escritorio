##################################################################
# Makefile Template
#
# please replace the name 'filename' to your program name,
# and add the FLAGS or libraries you need to compile the program
#
# Date:  2018.03.04
# author: Mikiya Michishita
###################################################################


DEFAULT_TARGET := help

# list up all targets
ALL_TARGET := filename

target: $(DEFAULT_TARGET) ## help

all: $(ALL_TARGET) ## Make all files

pdf:  ## make pdf pdfpc
	./convert.sh plankton.tex

clean: ## Clean archived files
	rm *.aux
	rm *.dvi
	rm *.log
	rm *.nav
	rm *.out
	rm *.snm
	rm *.toc

.PHONT: help
help:
	@grep -E '^[a-zA-Z_-]+:.*##.*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*##"}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

