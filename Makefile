# Get all root folders to run the make in except the _build and llvm dirrectory
SUBDIRS := $(wildcard */.)
SUBDIRS := $(filter-out _build/.,$(SUBDIRS))
SUBDIRS := $(filter-out llvm/.,$(SUBDIRS))

MAKE := make test

all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@;\

.PHONY: all $(SUBDIRS)
