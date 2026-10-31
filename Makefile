# ulang.jar, which ulang.sh runs

all: ulang.jar

# mill decides what to rebuild, hence always called
.PHONY: ulang.jar
ulang.jar:
	./mill -i ulang.assembly < /dev/null
	cp out/ulang/assembly.dest/out.jar ulang.jar
