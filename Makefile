# out/ulang.bin, the self-hosted driver of ulang, compiled from the sources by the seed in bootstrap/

all: out/ulang.bin out/ulang.sh ulang.bin ulang.sh

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

# out/ulang.bin, the tool, is the compiler of the current sources ulang/*.u and ulang/*/*.u, with the library in
# ulang/prelude, translated by the seed, and bootstrap/ulang.bin, the seed, is built from bootstrap/ulang.c, the C of the last
# compiler that compiled itself to the same C, which is versioned, as the C of its backups in bootstrap/<time stamp>/ is, while
# the binaries are not; each is linked with the runtime in ulang/runtime, GMP, and the Boehm collector
CC = clang
CFLAGS = -O2 -std=c11 -Wall -Wno-unused-variable -Wno-unused-function -I ulang/runtime -DULANG_LIB='"$(CURDIR)/ulang/prelude"'
LIBS = -lgmp -lgc
RUNTIME = ulang/runtime/ulang.c ulang/runtime/ulang.h
SOURCES = $(wildcard ulang/*.u ulang/*/*.u)

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
	bootstrap/ulang.sh compile ulang/ulang.u -o $@

out/ulang.bin: out/ulang.c $(RUNTIME) Makefile
	$(CC) $(CFLAGS) out/ulang.c ulang/runtime/ulang.c $(LIBS) -o $@

out/ulang.sh: $(SCRIPT)
	mkdir -p out
	cp $(SCRIPT) $@

# copies of the tool and its script at the root, ./ulang.sh, which are not versioned
ulang.bin: out/ulang.bin
	cp out/ulang.bin $@

ulang.sh: $(SCRIPT)
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
	out/ulang.sh compile ulang/ulang.u -o $$stage/ulang2.c && \
	$(CC) $(CFLAGS) $$stage/ulang2.c ulang/runtime/ulang.c $(LIBS) -o $$stage/ulang2.bin && \
	cp $(SCRIPT) $$stage/ulang2.sh && \
	$$stage/ulang2.sh compile ulang/ulang.u -o $$stage/ulang3.c && \
	cmp $$stage/ulang2.c $$stage/ulang3.c && \
	$(call install_seed,$$stage/ulang2.c,$$stage/ulang2.bin) && \
	rm -r -- "$${stage:?}"
	$(MAKE) all

# the interpreter ulang/run/run.u in the seed runs the compiler on the sources, for sources that the seed cannot compile, whose C,
# compiled, must compile them to the same C, and becomes the seed
bootstrap: bootstrap/ulang.bin
	mkdir -p out && stage=$$(mktemp -d out/stage-XXXXXX) && \
	bootstrap/ulang.sh run ulang/ulang.u -- compile ulang/ulang.u -o $$stage/ulang1.c && \
	$(CC) $(CFLAGS) $$stage/ulang1.c ulang/runtime/ulang.c $(LIBS) -o $$stage/ulang1.bin && \
	cp $(SCRIPT) $$stage/ulang1.sh && \
	$$stage/ulang1.sh compile ulang/ulang.u -o $$stage/ulang2.c && \
	cmp $$stage/ulang1.c $$stage/ulang2.c && \
	$(call install_seed,$$stage/ulang1.c,$$stage/ulang1.bin) && \
	rm -r -- "$${stage:?}"
	$(MAKE) all

# every .u file of the project, also of the library and of the tests of the packages in their subfolders test/, but those of
# ulang/test/errors/, which project.u declares to run only compiled, run by out/ulang.bin with all its tests in the
# working directory, the root, must succeed
.PHONY: test
test: out/ulang.bin out/ulang.sh
	out/ulang.sh test
	$(MAKE) --no-print-directory testerrors
	$(MAKE) --no-print-directory testdocs

# the tests of the project compiled into one binary by compile --tests, whose main runs those that its arguments select, as
# test does, at the root, by a copy of the script; at -O1, which cc compiles in half the time of -O2; its C depends on every
# .u file of the project, PROJECT_SOURCES, while SOURCES are those of the tool, the compiler and the library
SUITE = out/suite
PROJECT_SOURCES = $(shell find . -name '*.u' -not -path '*/out/*' -not -path '*/.*/*')

$(SUITE)/suite.c: out/ulang.bin out/ulang.sh $(PROJECT_SOURCES)
	mkdir -p $(SUITE)
	out/ulang.sh compile --tests -o $@

$(SUITE)/suite.bin: $(SUITE)/suite.c $(RUNTIME) Makefile
	$(CC) $(subst -O2,-O1,$(CFLAGS)) $< ulang/runtime/ulang.c $(LIBS) -o $@

$(SUITE)/suite.sh: $(SCRIPT)
	mkdir -p $(SUITE)
	cp $(SCRIPT) $@

# the tests of the errors of the phases in ulang/test/errors/, whose cases each load the library, which the interpreted
# parser and checker do too slowly, so they run only compiled
.PHONY: testerrors
testerrors: $(SUITE)/suite.bin $(SUITE)/suite.sh
	$(SUITE)/suite.sh ulang/test/errors

# all tests of the project compiled, which have the same outcome as interpreted, but without lemmas
.PHONY: testc
testc: $(SUITE)/suite.bin $(SUITE)/suite.sh
	$(SUITE)/suite.sh

# the tests of the project as testc runs them, but compiled to assembly by ulang/compile/x86.u, which as and the linker
# turn into a binary with the runtime, compiled once to out/asm/ulang.o, without compiling C of programs
ASM = out/asm
SUITE_ASM = out/suite-asm

$(ASM)/ulang.o: $(RUNTIME) Makefile
	mkdir -p $(ASM)
	$(CC) $(CFLAGS) -c ulang/runtime/ulang.c -o $@

$(SUITE_ASM)/suite.s: out/ulang.bin out/ulang.sh $(PROJECT_SOURCES)
	mkdir -p $(SUITE_ASM)
	out/ulang.sh compile --tests -o $@

$(SUITE_ASM)/suite.bin: $(SUITE_ASM)/suite.s $(ASM)/ulang.o
	$(CC) $< $(ASM)/ulang.o $(LIBS) -o $@

$(SUITE_ASM)/suite.sh: $(SCRIPT)
	mkdir -p $(SUITE_ASM)
	cp $(SCRIPT) $@

.PHONY: testasm
testasm: $(SUITE_ASM)/suite.bin $(SUITE_ASM)/suite.sh
	$(SUITE_ASM)/suite.sh

# the checks of the repository in checks/, programs that run compiled at the root, since they parse the sources of the
# compiler, which the interpreter does too slowly: checks/docs.u, that the references of the documentation name
# declarations of their files
CHECKS = out/checks

$(CHECKS)/docs.c: out/ulang.bin out/ulang.sh checks/docs.u $(SOURCES)
	mkdir -p $(CHECKS)
	out/ulang.sh compile checks/docs.u -o $@

$(CHECKS)/docs.bin: $(CHECKS)/docs.c $(RUNTIME) Makefile
	$(CC) $(CFLAGS) $(CHECKS)/docs.c ulang/runtime/ulang.c $(LIBS) -o $@

$(CHECKS)/docs.sh: $(SCRIPT)
	mkdir -p $(CHECKS)
	cp $(SCRIPT) $@

.PHONY: testdocs
testdocs: $(CHECKS)/docs.bin $(CHECKS)/docs.sh
	$(CHECKS)/docs.sh

# checks/termination.u, a report of the recursive functions of ulang/ and the library whose termination the prover does
# not prove, not a test, since most of them recurse through comprehensions, which its measures do not reach yet
$(CHECKS)/termination.c: out/ulang.bin out/ulang.sh checks/termination.u $(SOURCES)
	mkdir -p $(CHECKS)
	out/ulang.sh compile checks/termination.u -o $@

$(CHECKS)/termination.bin: $(CHECKS)/termination.c $(RUNTIME) Makefile
	$(CC) $(CFLAGS) $(CHECKS)/termination.c ulang/runtime/ulang.c $(LIBS) -o $@

$(CHECKS)/termination.sh: $(SCRIPT)
	mkdir -p $(CHECKS)
	cp $(SCRIPT) $@

.PHONY: termination
termination: $(CHECKS)/termination.bin $(CHECKS)/termination.sh
	$(CHECKS)/termination.sh

# rewrites every .u file of the project as format does, after a change of the printer
.PHONY: reformat
reformat: out/ulang.bin out/ulang.sh
	out/ulang.sh format
