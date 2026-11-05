# ulang.bin, the self-hosted driver of ulang, compiled from bootstrap/ulang.c

all: ulang.bin

.PHONY: install

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

# the compiler of self/ as a program, ./ulang.bin compile <file> <entry> <target.c>, built from bootstrap/ulang.c, the C of the last
# working compiler, which is versioned as its backups are, with the runtime in self/runtime, GMP, and the Boehm collector
CFLAGS = -O2 -std=c11 -Wall -Wno-unused-variable -Wno-unused-function -I self/runtime -DULANG_LIB='"$(CURDIR)/prelude"'
LIBS = -lgmp -lgc
STAGE = bootstrap/stage

# the compiler needs a large stack, as the interpreter does
COMPILE = ulimit -s unlimited &&

# also when the Makefile changes, e.g. CFLAGS with the directory of the library
ulang.bin: bootstrap/ulang.c self/runtime/ulang.c self/runtime/ulang.h Makefile
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

# the interpreter self/run.u in ulang.bin runs the compiler on itself, whose C, compiled, must compile itself to the same C,
# and replaces ulang.bin
bootstrap: ulang.bin
	mkdir -p $(STAGE)
	$(COMPILE) ./ulang.bin run self/ulang.u main -- compile self/ulang.u main $(STAGE)/ulang1.c
	cc $(CFLAGS) $(STAGE)/ulang1.c self/runtime/ulang.c $(LIBS) -o $(STAGE)/ulang1.bin
	$(COMPILE) $(STAGE)/ulang1.bin compile self/ulang.u main $(STAGE)/ulang2.c
	cmp $(STAGE)/ulang1.c $(STAGE)/ulang2.c
	$(call install,$(STAGE)/ulang1.c,$(STAGE)/ulang1.bin)

# ulang.bin compiles the compiler, whose program compiles it again, which must compile itself to the same C,
# since the C of the old compiler may differ, and replaces ulang.bin
selfcompile: ulang.bin
	mkdir -p $(STAGE)
	$(COMPILE) ./ulang.bin compile self/ulang.u main $(STAGE)/ulang1.c
	cc $(CFLAGS) $(STAGE)/ulang1.c self/runtime/ulang.c $(LIBS) -o $(STAGE)/ulang1.bin
	$(COMPILE) $(STAGE)/ulang1.bin compile self/ulang.u main $(STAGE)/ulang2.c
	cc $(CFLAGS) $(STAGE)/ulang2.c self/runtime/ulang.c $(LIBS) -o $(STAGE)/ulang2.bin
	$(COMPILE) $(STAGE)/ulang2.bin compile self/ulang.u main $(STAGE)/ulang3.c
	cmp $(STAGE)/ulang2.c $(STAGE)/ulang3.c
	$(call install,$(STAGE)/ulang2.c,$(STAGE)/ulang2.bin)

# every .u file of tests/, self/, simp/, and the library, run by ulang.bin with all its tests in its directory, must succeed,
# or fail with the error that its first line states, e.g. // type error: unknown variable x
.PHONY: test
test: ulang.bin
	$(COMPILE) ./ulang.bin test tests self simp prelude

# the same files, each compiled by ulang.bin and cc with the runtime object, and run in its directory, by testc.sh in parallel,
# except for the modules of the prelude, which cannot be compiled on their own
TESTC = out/testc

$(TESTC)/ulang.o: self/runtime/ulang.c self/runtime/ulang.h Makefile
	mkdir -p $(TESTC)
	cc $(CFLAGS) -c self/runtime/ulang.c -o $@

# every file that parses, formatted by ulang.bin, which fails if the source does not parse to the same commands
.PHONY: testformat
testformat: ulang.bin
	@n=0; for f in $$(find tests self simp prelude -name '*.u' | sort); do \
	  head -n 1 $$f | grep -q '^// parse error' && continue; \
	  ./ulang.bin format $$f > /dev/null || exit 1; n=$$((n + 1)); \
	done; echo "$$n files formatted"

.PHONY: testc
testc: ulang.bin $(TESTC)/ulang.o
	./testc.sh $(TESTC) $(TESTC)/ulang.o tests self simp prelude
