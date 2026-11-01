# ulang.jar, which ulang.sh runs

all: ulang.jar

.PHONY: install

# mill decides what to rebuild, hence always called
.PHONY: ulang.jar
ulang.jar:
	./mill -i ulang.assembly < /dev/null
	cp out/ulang/assembly.dest/out.jar ulang.jar

# the VS Code extension, packaged by hand since vsce is not installed, see vscode/vsix
EXTENSION = vscode/package.json vscode/language-configuration.json vscode/extension.js vscode/syntaxes/ulang.tmLanguage.json

ulang.vsix: $(EXTENSION) vscode/vsix/*
	rm -rf out/vsix && mkdir -p out/vsix/extension/syntaxes
	cp vscode/package.json vscode/language-configuration.json vscode/extension.js out/vsix/extension/
	cp vscode/syntaxes/ulang.tmLanguage.json out/vsix/extension/syntaxes/
	cp vscode/vsix/* out/vsix/
	rm -f ulang.vsix && cd out/vsix && zip -qr ../../ulang.vsix .

install: ulang.vsix
	codium --install-extension ulang.vsix

# the compiler of self/ as a program, ./ulang.bin source.u target.c, built from bootstrap/ulang.c, the C of the last
# working compiler, which is versioned as its backups are, with the runtime in self/runtime, GMP, and the Boehm collector
CFLAGS = -O2 -std=c11 -Wall -Wno-unused-variable -Wno-unused-function -I self/runtime -DULANG_LIB='"$(CURDIR)/ulang/resources"'
LIBS = -lgmp -lgc
STAGE = bootstrap/stage

# the compiler needs a large stack, as the interpreter does
COMPILE = ulimit -s unlimited &&

ulang.bin: bootstrap/ulang.c self/runtime/ulang.c self/runtime/ulang.h
	cc $(CFLAGS) bootstrap/ulang.c self/runtime/ulang.c $(LIBS) -o $@

# keeps the compiler so far in bootstrap/ with a time stamp, and installs the compiler of the C file $(1) and the binary $(2)
define install
	stamp=$$(date +%Y%m%d-%H%M%S) && mkdir -p bootstrap/$$stamp && \
	if [ -f bootstrap/ulang.c ]; then cp bootstrap/ulang.c bootstrap/$$stamp/; fi && \
	if [ -f ulang.bin ]; then cp ulang.bin bootstrap/$$stamp/; fi
	cp $(1) bootstrap/ulang.c
	cp $(2) ulang.bin
endef

.PHONY: bootstrap selfcompile

# the interpreter compiles the compiler, which must compile itself to the same C, and replaces ulang.bin
bootstrap: ulang.jar
	mkdir -p $(STAGE)
	./ulang.sh self/ulang.u -- ulang.u ../$(STAGE)/ulang1.c
	cc $(CFLAGS) $(STAGE)/ulang1.c self/runtime/ulang.c $(LIBS) -o $(STAGE)/ulang1.bin
	$(COMPILE) $(STAGE)/ulang1.bin self/ulang.u $(STAGE)/ulang2.c
	cmp $(STAGE)/ulang1.c $(STAGE)/ulang2.c
	$(call install,$(STAGE)/ulang1.c,$(STAGE)/ulang1.bin)

# ulang.bin compiles the compiler, whose program compiles it again, which must compile itself to the same C,
# since the C of the old compiler may differ, and replaces ulang.bin
selfcompile: ulang.bin
	mkdir -p $(STAGE)
	$(COMPILE) ./ulang.bin self/ulang.u $(STAGE)/ulang1.c
	cc $(CFLAGS) $(STAGE)/ulang1.c self/runtime/ulang.c $(LIBS) -o $(STAGE)/ulang1.bin
	$(COMPILE) $(STAGE)/ulang1.bin self/ulang.u $(STAGE)/ulang2.c
	cc $(CFLAGS) $(STAGE)/ulang2.c self/runtime/ulang.c $(LIBS) -o $(STAGE)/ulang2.bin
	$(COMPILE) $(STAGE)/ulang2.bin self/ulang.u $(STAGE)/ulang3.c
	cmp $(STAGE)/ulang2.c $(STAGE)/ulang3.c
	$(call install,$(STAGE)/ulang2.c,$(STAGE)/ulang2.bin)

# the tests of the modules of self/, compiled by ulang.bin and run in self/, except those that need read of the interpreter
SELFTESTS = syntax ctx check run lex c compile

.PHONY: selftest
selftest: ulang.bin
	mkdir -p $(STAGE)
	for n in $(SELFTESTS); do \
	  ($(COMPILE) ./ulang.bin self/$$n.u $(STAGE)/$$n.c > /dev/null) && \
	  cc $(CFLAGS) $(STAGE)/$$n.c self/runtime/ulang.c $(LIBS) -o $(STAGE)/$$n && \
	  (cd self && $(COMPILE) ../$(STAGE)/$$n > /dev/null) && echo "self/$$n.u passed" || exit 1; \
	done
