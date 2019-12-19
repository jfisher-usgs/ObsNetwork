# Prepare package for release

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: install check clean
.PHONY: all

build:
	cd ..
	R CMD build $(PKGSRC)
.PHONY: build

install: build
	cd ..
	R CMD INSTALL --build $(PKGNAME)_$(PKGVERS).tar.gz
.PHONY: install

check:
	cd ..
	R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz
.PHONY: check

clean:
	cd ..
	$(RM) -r $(PKGNAME).Rcheck/
.PHONY: clean
