
# Path from current directory to top level ableC repository
ABLEC_BASE?=../../../ableC
# Path from current directory to top level extensions directory
EXTS_BASE?=../../../extensions

# The artifact specification grammars to compile
MDA_ARTIFACT=determinism
MWDA_ARTIFACT=well_definedness
# All directories containing grammars that may be included
GRAMMAR_DIRS=$(ABLEC_BASE)/grammars $(wildcard $(EXTS_BASE)/*/grammars)
# All silver files in included grammars, to be included as dependancies
GRAMMAR_SOURCES=$(shell find $(GRAMMAR_DIRS) -name *.sv -print0 | xargs -0)
# Flags passed to silver including the appropriate directories
override SVFLAGS+=$(addprefix -I , $(GRAMMAR_DIRS))

all: mda mwda
mda: mda.test
mwda: mwda.test

mda.test: $(GRAMMAR_SOURCES)
	touch $(wildcard $(MDA_ARTIFACT)/*.sv)
	silver-ableC --dont-translate $(SVFLAGS) $(MDA_ARTIFACT)
	touch $@

mwda.test: $(GRAMMAR_SOURCES)
	touch $(wildcard $(MWDA_ARTIFACT)/*.sv)
	silver-ableC --dont-translate $(SVFLAGS) --clean --warn-all --warn-error $(MWDA_ARTIFACT)
	touch $@

clean:
	rm -f *~ *.jar *.copperdump.html build.xml *.test

.PHONY: all mda mwda clean
