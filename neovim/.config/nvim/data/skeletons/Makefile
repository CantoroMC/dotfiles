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

exeName: exeName.o depName1.o depName2.o
	$(COMP) $(CFLAGS) $^ -o $@

exeName.o: exeName.cpp
	$(COMP) $(CFLAGS) -c $^

depName1.o: ../dir/depName1.h ../dir/depName1.cpp
	$(COMP) $(CFLAGS) -c $(filter-out $<,$^)

depName2.o: ../dir/depName2.h ../dir/depName2.cpp
	$(COMP) $(CFLAGS) -c $(filter-out $<,$^)

clean:
	@rm *.o $(EXE)

