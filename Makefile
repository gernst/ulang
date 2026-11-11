# out/ulang.bin, the self-hosted driver of ulang, compiled from the sources by the seed in bootstrap/

all: out/ulang.bin out/ulang.sh

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

# out/ulang.bin, the tool, is the compiler of the current sources ulang/*.u and prelude/*.u, translated by the seed, and
# bootstrap/ulang.bin, the seed, is built from bootstrap/ulang.c, the C of the last compiler that compiled itself to the same C,
# which is versioned, as the C of its backups in bootstrap/<time stamp>/ is, while the binaries are not; each is linked with the
# runtime in ulang/runtime, GMP, and the Boehm collector
CC = clang
CFLAGS = -O2 -std=c11 -Wall -Wno-unused-variable -Wno-unused-function -I ulang/runtime -DULANG_LIB='"$(CURDIR)/prelude"'
LIBS = -lgmp -lgc
RUNTIME = ulang/runtime/ulang.c ulang/runtime/ulang.h
SOURCES = $(wildcard ulang/*.u prelude/*.u)

# each binary is run by a copy of bootstrap/ulang.sh next to it, of the same name, which gives it the large stack that the
# interpreter and the compiler need
SCRIPT = bootstrap/ulang.sh

# a C file that failed half way is not kept
.DELETE_ON_ERROR:

# also when the Makefile changes, e.g. CFLAGS with the directory of the library
bootstrap/ulang.bin: bootstrap/ulang.c $(RUNTIME) Makefile
	$(CC) $(CFLAGS) bootstrap/ulang.c ulang/runtime/ulang.c $(LIBS) -o $@

out/ulang.c: bootstrap/ulang.bin $(SOURCES)
	mkdir -p out
	bootstrap/ulang.sh compile ulang/ulang.u main $@

out/ulang.bin: out/ulang.c $(RUNTIME) Makefile
	$(CC) $(CFLAGS) out/ulang.c ulang/runtime/ulang.c $(LIBS) -o $@

out/ulang.sh: $(SCRIPT)
	mkdir -p out
	cp $(SCRIPT) $@

# keeps the seed in bootstrap/<time stamp>/ and installs the C file $(1) and the binary $(2) as the seed, as one command, unless
# the C is that of the seed
install_seed = if cmp -s $(1) bootstrap/ulang.c; then echo "the seed is unchanged"; else \
	stamp=$$(date +%Y%m%d-%H%M%S) && mkdir -p bootstrap/$$stamp && cp bootstrap/ulang.c bootstrap/$$stamp/ && \
	{ [ ! -f bootstrap/ulang.bin ] || cp bootstrap/ulang.bin bootstrap/$$stamp/; } && \
	cp $(1) bootstrap/ulang.c && cp $(2) bootstrap/ulang.bin; fi

.PHONY: bootstrap selfcompile

# Each run has a stage directory of its own in out/, which is removed if the run succeeds, and then make builds out/ulang.bin
# again from the new seed.

# out/ulang.bin, the sources compiled by the seed, compiles them again, whose program must compile them to the same C, since
# the C of the seed may differ, and that becomes the seed
selfcompile: out/ulang.bin out/ulang.sh
	stage=$$(mktemp -d out/stage-XXXXXX) && \
	out/ulang.sh compile ulang/ulang.u main $$stage/ulang2.c && \
	$(CC) $(CFLAGS) $$stage/ulang2.c ulang/runtime/ulang.c $(LIBS) -o $$stage/ulang2.bin && \
	cp $(SCRIPT) $$stage/ulang2.sh && \
	$$stage/ulang2.sh compile ulang/ulang.u main $$stage/ulang3.c && \
	cmp $$stage/ulang2.c $$stage/ulang3.c && \
	$(call install_seed,$$stage/ulang2.c,$$stage/ulang2.bin) && \
	rm -r -- "$${stage:?}"
	$(MAKE) out/ulang.bin

# the interpreter ulang/run.u in the seed runs the compiler on the sources, for sources that the seed cannot compile, whose C,
# compiled, must compile them to the same C, and becomes the seed
bootstrap: bootstrap/ulang.bin
	mkdir -p out && stage=$$(mktemp -d out/stage-XXXXXX) && \
	bootstrap/ulang.sh run ulang/ulang.u main -- compile ulang/ulang.u main $$stage/ulang1.c && \
	$(CC) $(CFLAGS) $$stage/ulang1.c ulang/runtime/ulang.c $(LIBS) -o $$stage/ulang1.bin && \
	cp $(SCRIPT) $$stage/ulang1.sh && \
	$$stage/ulang1.sh compile ulang/ulang.u main $$stage/ulang2.c && \
	cmp $$stage/ulang1.c $$stage/ulang2.c && \
	$(call install_seed,$$stage/ulang1.c,$$stage/ulang1.bin) && \
	rm -r -- "$${stage:?}"
	$(MAKE) out/ulang.bin

# every .u file of tests/, ulang/, prove/, and the library, run by out/ulang.bin with all its tests in its directory, must
# succeed, or fail with the error that its first line states, e.g. // type error: unknown variable x
.PHONY: test
test: out/ulang.bin out/ulang.sh
	out/ulang.sh test tests ulang prove prelude

# the same files, each compiled by out/ulang.bin and cc with the runtime object, and run in its directory, by testc.sh in
# parallel
TESTC = out/testc

$(TESTC)/ulang.o: $(RUNTIME) Makefile
	mkdir -p $(TESTC)
	$(CC) $(CFLAGS) -c ulang/runtime/ulang.c -o $@

.PHONY: testc
testc: out/ulang.bin out/ulang.sh $(TESTC)/ulang.o
	./testc.sh $(TESTC) $(TESTC)/ulang.o tests ulang prove prelude
