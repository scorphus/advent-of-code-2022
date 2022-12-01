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
	@-raco pkg install advent-of-code
	@-raco pkg install cover
	@-raco pkg install fmt
	@-raco pkg install review
	@-raco pkg install test
.PHONY: setup

# create new challenge from sample
next-day:
	@$(MAKE) new-day-$$(echo "(" $$(date +%d) + 1 ") % 30" | bc)
.PHONY: next-day

new-day:
	@$(MAKE) new-day-$$(date +%d)
.PHONY: new-day

new-day-%:
	@$(MAKE) __new-day-$$(printf "%02d" $*)
.PHONY: new-day-

__new-day-%:
	@mkdir -p day$*
	@cp sample.rkt day$*.rkt
	@sed -i.tmp s/%%DAY%%/$*/ day$*.rkt
	@rm day$*.rkt.tmp
	@-raco aoc -s $$(cat ~/.adventofcode.session) -y 2022 -d $* > input$*.txt
.PHONY: __new-day-

# run tests
test:
	@raco test **/*.rkt
.PHONY: test

# report coverage in html format
cover:
	@raco cover **/*.rkt
.PHONY: cover
