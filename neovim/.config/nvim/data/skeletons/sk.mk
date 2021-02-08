### ==========================================================================
### Section 1. General Configuration
### ==========================================================================

# Executables name
EXE=

# Installation dir definitions
PREFIX=$(HOME)/dotfiles
BINDIR=$(PREFIX)/bin
DIR_NAME=

### ==========================================================================
### Section 2. High-level Configuration
### ==========================================================================

optimize=yes
debug=no

### ==========================================================================
### Section 3. Low-level Configuration
### ==========================================================================

# Compiler options
CFLAGS=-g -Wall -Wextra -Wshadow -fstack-protector-all
ifeq ($(COMP),)
	COMP=gcc
endif

ifeq ($(COMP),gcc)
	CFLAGS+= -Wno-long-long -pedantic -std=c11 -lm -O3
endif

ifeq ($(COMP),clang)
	CFLAGS+= -Wlong-long -pedantic -std=c11
endif

# Debugging options
ifeq ($(debug),no)
	CFLAGS+= -DNDEBUG
endif

# Optimization options
ifeq ($(optimize),yes)
	CFLAGS+= -O3
endif

### ==========================================================================
### Public targets
### ==========================================================================

help:
	@echo ""
	@echo -e "\e[1;33mTo compile, type:\e[0m"
	@echo ""
	@echo "	make target [COMP=comp]"
	@echo ""
	@echo -e "\e[1;33mSupported targets:\e[0m"
	@echo "	build		Build the specified executables"
	@echo "	clean			Clean up the directory"
	@echo "	install		Install the executables"
	@echo "	uninstall	Uninstall the executables"
	@echo ""
	@echo -e "\e[1;33mSupported compilers:\e[0m"
	@echo "	gcc"
	@echo "	clang"
	@echo ""
	@echo -e "\e[1;33mExamples:\e[0m"
	@echo "	make build COMP=clang"
	@echo "	make build"
	@echo "	make install"
	@echo "	..."

build:
	$(MAKE) COMP=$(COMP) all

clean:
	@rm *.o $(EXE)

install: build
	mkdir -p $(BINDIR)/$(DIR_NAME)
	cp $(EXE) $(BINDIR)/$(DIR_NAME)/

uninstall:
	@rm -rf $(BINDIR)/$(DIR_NAME)

### ==========================================================================
### Private targets
### ==========================================================================

all: $(EXE)

target: dependency1.o dependency2.o
	$(COMP) $(CFLAGS) $^ -o $@

%.o: %.h %.c
	$(COMP) $(CFLAGS) -c $(filter-out $<,$^)

