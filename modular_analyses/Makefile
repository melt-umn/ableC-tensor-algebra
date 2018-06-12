
# Path from current directory to top level ableC repository
ABLEC_BASE?=../../../ableC
# Path from current directory to top level extensions directory
EXTS_BASE?=../../../extensions

# The jar files to build
MDA_JAR=MDA.jar
MWDA_JAR=MWDA.jar
# The artifact specification grammars to compile
MDA_ARTIFACT=determinism
MWDA_ARTIFACT=well_definedness
# All directories containing grammars that may be included
GRAMMAR_DIRS=$(ABLEC_BASE)/grammars $(wildcard $(EXTS_BASE)/*/grammars)
# All silver files in included grammars, to be included as dependancies
GRAMMAR_SOURCES=$(shell find $(GRAMMAR_DIRS) -name *.sv -print0 | xargs -0)
# Flags passed to silver including the appropriate directories
override SVFLAGS+=$(addprefix -I ,$(GRAMMAR_DIRS))

all: mda mwda

mda: $(MDA_JAR)

mwda: $(MWDA_JAR)

$(MDA_JAR): $(GRAMMAR_SOURCES)
	touch $(wildcard $(MDA_ARTIFACT)/*.sv)
	silver -o $(MDA_JAR) $(SVFLAGS) $(MDA_ARTIFACT)

$(MWDA_JAR): $(GRAMMAR_SOURCES)
	touch $(wildcard $(MWDA_ARTIFACT)/*.sv)
	silver -o $(MWDA_JAR) $(SVFLAGS) --clean --warn-all --warn-error $(MWDA_ARTIFACT)

clean:
	rm -f *~ *.jar *.copperdump.html build.xml

.PHONY: all mda mwda clean
.NOTPARALLEL: # Avoid running multiple Silver builds in parallel
