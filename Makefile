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

# every .u file of ulang/, also of the library and of the tests of the packages in their subfolders test/, run by out/ulang.bin with
# all its tests in its directory, must succeed, or fail with the error that its first line states, e.g.
# // type error: unknown variable x
.PHONY: test
test: out/ulang.bin out/ulang.sh
	out/ulang.sh test ulang
	$(MAKE) --no-print-directory testdocs

# the checks of the repository in checks/, which run only compiled, since they parse the sources of the compiler, which the
# interpreter does too slowly: checks/docs.u, that the references of the documentation name declarations of their files;
# each binary run in checks/ by a copy of the script
CHECKS = out/checks

$(CHECKS)/docs.c: out/ulang.bin out/ulang.sh checks/docs.u $(SOURCES)
	mkdir -p $(CHECKS)
	out/ulang.sh compile checks/docs.u --tests -o $@

$(CHECKS)/docs.bin: $(CHECKS)/docs.c $(RUNTIME) Makefile
	$(CC) $(CFLAGS) $(CHECKS)/docs.c ulang/runtime/ulang.c $(LIBS) -o $@

$(CHECKS)/docs.sh: $(SCRIPT)
	mkdir -p $(CHECKS)
	cp $(SCRIPT) $@

.PHONY: testdocs
testdocs: $(CHECKS)/docs.bin $(CHECKS)/docs.sh
	cd checks && ../$(CHECKS)/docs.sh

# rewrites every .u file as format does, after a change of the printer, which skips the files whose first line expects a
# parse error, since it refuses them
.PHONY: reformat
reformat: out/ulang.bin out/ulang.sh
	out/ulang.sh format project.u ulang examples

# the same files, each compiled by out/ulang.bin and cc with the runtime object, and run in its directory, by testc.sh in
# parallel
TESTC = out/testc

$(TESTC)/ulang.o: $(RUNTIME) Makefile
	mkdir -p $(TESTC)
	$(CC) $(CFLAGS) -c ulang/runtime/ulang.c -o $@

.PHONY: testc
testc: out/ulang.bin out/ulang.sh $(TESTC)/ulang.o
	./testc.sh $(TESTC) $(TESTC)/ulang.o ulang
