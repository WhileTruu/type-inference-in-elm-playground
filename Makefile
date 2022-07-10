rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

.PHONY: test
test:
	cd cli && elm-test $(foreach x, $(call rwildcard,compiler/,*.elm), ../$x)