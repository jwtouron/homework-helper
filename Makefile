%.js: src/%.elm
	elm make --output=$@ $?
