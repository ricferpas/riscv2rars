
default: riscv2rars.jar

SOURCES=$(shell find -L src/ -type f)
TARGET_JAR=target/scala-3.7.4/riscv2rars-assembly-0.1-SNAPSHOT.jar

$(TARGET_JAR): ${SOURCES} build.sbt project/build.properties project/plugins.sbt
	rm -f $@
	sbt assembly

riscv2rars.jar: $(TARGET_JAR)
	ln -sf $^ $@

clean:
	rm -f riscv2rars.jar
	sbt clean
	rm -rf target
	rm -rf project/target project/project

