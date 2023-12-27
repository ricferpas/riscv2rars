
default: riscv2rars.jar

SOURCES=$(shell find src/ -type f)

target/scala-2.13/riscv2rars-assembly-0.1-SNAPSHOT.jar: ${SOURCES} build.sbt
	rm -f $@
	sbt assembly

riscv2rars.jar: target/scala-2.13/riscv2rars-assembly-0.1-SNAPSHOT.jar
	ln -sf $^ $@

clean:
	rm -f riscv2rars.jar
	sbt clean
	rm -rf target
	rm -rf project/target project/project

