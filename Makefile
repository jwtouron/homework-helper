OUTPUTS=SquareRoots.js

.PHONY: all
all: $(OUTPUTS)

%.js: src/%.elm
	elm make --output=$@ $?
