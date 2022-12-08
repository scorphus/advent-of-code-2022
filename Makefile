# This file is part of Advent of Code 2022
# https://github.com/scorphus/advent-of-code-2022

# Licensed under the BSD-3-Clause license:
# https://opensource.org/licenses/BSD-3-Clause
# Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

# list all available targets
list:
	@sh -c "$(MAKE) -p no_targets__ | awk -F':' '/^[a-zA-Z0-9][^\$$#\/\\t=]*:([^=]|$$)/ {split(\$$1,A,/ /);for(i in A)print A[i]}' | grep -v '__\$$' | grep -v 'make\[1\]' | grep -v 'Makefile' | sort"
.PHONY: list
# required for list
no_targets__:

# install dependencies
setup:
	@raco pkg install --auto --update-deps --skip-installed
.PHONY: setup

# create new challenge from sample
next-day:
	@$(MAKE) new-day-$$(echo "(" $$(date +%d) + 1 ") % 30" | bc)
.PHONY: next-day

new-day:
	@$(MAKE) new-day-$$(date +%d | bc)
.PHONY: new-day

new-day-%:
	@$(MAKE) __new-day-$$(printf "%d" $*)
.PHONY: new-day-

__new-day-%:
	@mkdir -p day$$(printf "%02d" $*)
	@cp sample.rkt solution.rkt
	@sed -i.tmp s/%%DAY%%/$*/g solution.rkt
	@rm solution.rkt.tmp
	@-raco aoc -s $$(cat ~/.adventofcode.session) -y 2022 -d $* > input.txt
.PHONY: __new-day-

# run linter
lint:
	@for rkt in **/*rkt; do echo $$rkt; raco review $$rkt; done
.PHONY: test

# run tests
test:
	@raco test -t -x -p advent-of-code-2022
.PHONY: test

# report coverage in html format
cover:
	@raco cover .
.PHONY: cover
