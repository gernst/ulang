# ulang.jar, which ulang.sh runs

all: ulang.jar

.PHONY: install

# mill decides what to rebuild, hence always called
.PHONY: ulang.jar
ulang.jar:
	./mill -i ulang.assembly < /dev/null
	cp out/ulang/assembly.dest/out.jar ulang.jar

# the VS Code extension, packaged by hand since vsce is not installed, see vscode/vsix
EXTENSION = vscode/package.json vscode/language-configuration.json vscode/syntaxes/ulang.tmLanguage.json

ulang.vsix: $(EXTENSION) vscode/vsix/*
	rm -rf out/vsix && mkdir -p out/vsix/extension/syntaxes
	cp vscode/package.json vscode/language-configuration.json out/vsix/extension/
	cp vscode/syntaxes/ulang.tmLanguage.json out/vsix/extension/syntaxes/
	cp vscode/vsix/* out/vsix/
	rm -f ulang.vsix && cd out/vsix && zip -qr ../../ulang.vsix .

install: ulang.vsix
	codium --install-extension ulang.vsix
