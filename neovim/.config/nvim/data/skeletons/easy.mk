# Executables name
EXE=exeName

# Compiler options
COMP=g++
CFLAGS=-std=c++17 -Wshadow -Wall -g -lm -fsanitize=address -fsanitize=undefined -D_GLIBCXX_DEBUG
#CFLAGS=-Wlong-long -pedantic -std=c++17 -g -Wall -Wextra -Wshadow -fstack-protector-all

help:
	@echo ""
	@echo -e "\e[1;33mTo compile, type:\e[0m"
	@echo ""
	@echo "	make target"
	@echo ""
	@echo -e "\e[1;33mSupported targets:\e[0m"
	@echo "	all				Build the specified executables"
	@echo "	clean			Clean up the directory"

all: $(EXE)

exeName: exeName.o dependency1.o dependency2.o
	$(COMP) $(CFLAGS) $^ -o $@

%.o: %.h %.cpp
	$(COMP) $(CFLAGS) -c $(filter-out $<,$^)

clean:
	@rm *.o $(EXE)
