
# Path from current directory to top level ableC repository
ABLEC_BASE?=../../../ableC
# Path from current directory to top level extensions directory
EXTS_BASE?=../../../extensions

# The jar file to build
ABLEC_JAR=ableC.jar
# The artifact specification grammar to compile
ARTIFACT=artifact
# All directories containing grammars that may be included
GRAMMAR_DIRS=$(ABLEC_BASE)/grammars $(wildcard $(EXTS_BASE)/*/grammars)
# All silver files in included grammars, to be included as dependancies
GRAMMAR_SOURCES=$(shell find $(GRAMMAR_DIRS) -name *.sv -print0 | xargs -0)
# Flags passed to silver including the appropriate directories
override SVFLAGS+=$(addprefix -I ,$(GRAMMAR_DIRS))
override SVFLAGS+=$(addprefix -I ,/home/counc009/gc/include)

# All extended C files to compile
XC_FILES=$(wildcard *.xc)
# All C files that should be generated
C_FILES=$(XC_FILES:.xc=.c)
# All object files that should be generated
OBJECTS=$(XC_FILES:.xc=.o)
# All executables that should be generated
EXECUTABLES=$(XC_FILES:.xc=.out)
# All evaluation marker files that should be generated
TESTS=$(XC_FILES:.xc=.test)

# All directories contining extension header files that may be included
XC_INCLUDE_DIRS=$(wildcard $(EXTS_BASE)/*/include)
# All header files that may be included, to be included as dependencies
XC_INCLUDE_SOURCES=$(foreach dir,$(XC_INCLUDE_DIRS),$(wildcard $(dir)/*.*h))
# Flags passed to ableC including the appropriate directories
override XCFLAGS+=$(addprefix -I,$(XC_INCLUDE_DIRS))
override XCFLAGS+=$(addprefix -I,/home/counc009/gc/include)
# Flags passed to Java when invoking ableC
override JAVAFLAGS+=-Xss6M

# Flags passed to the C compiler, e.g. to enable various compiler extensions
override CFLAGS+=-fopenmp

# All directories contining extension libraries that may be linked
LIB_DIRS=$(wildcard $(EXTS_BASE)/*/lib)
# Flags passed to the linker specifying the appropriate directories
override LDFLAGS+=$(addprefix -L,$(LIB_DIRS))
# Flags passed to the linker specifying libraries to link
LDLIBS=/home/counc009/gc/lib/libgc.a -lgomp

# All directories contining extension library sources
SRC_DIRS=$(wildcard $(EXTS_BASE)/*/src)
# All extension library targets
LIBS=$(SRC_DIRS:src=libs)
# All C files used to build libraries, to be included as dependencies
SRC_SOURCES=$(foreach dir,$(SRC_DIRS),$(wildcard $(dir)/*.c))

all: $(ABLEC_JAR) $(C_FILES) $(OBJECTS) $(EXECUTABLES) $(TESTS)

libs: $(LIBS)

$(LIBS):
	$(MAKE) -C $(@:libs=src)

$(ABLEC_JAR): $(GRAMMAR_SOURCES)
	touch $(wildcard $(ARTIFACT)/*.sv)
	silver -o $@ $(SVFLAGS) $(ARTIFACT)

%.c: %.xc $(XC_INCLUDE_SOURCES) $(ABLEC_JAR)
	java $(JAVAFLAGS) -jar $(ABLEC_JAR) $< $(XCFLAGS)

%.out: %.o $(SRC_SOURCES) | libs
	$(CC) $(LDFLAGS) $< $(LOADLIBES) $(LDLIBS) -o $@

%.test: %.out
	@echo "Running $<..."
	@./$< || echo "(exit $$?)"
	touch $@

clean:
	rm -f *~ *.i *.c *.o *.out *.test *.jar *.copperdump.html build.xml

.PHONY: all libs $(LIBS) clean
