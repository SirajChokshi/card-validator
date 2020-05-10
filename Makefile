# the compile with -Wall flags(all errors and warnings)
COMPILER = ghc -Wall
# our main module
MAIN = main

# will execute other targets
all: target clean

# depents on $(MAIN).hs - will be executed if Main.hs is changed
target: $(MAIN).hs
		# ghc -Wall Main.hs
		$(COMPILER) $(MAIN).hs

# cleanup
clean:
		# remove temporary compile files
		rm $(MAIN).o $(MAIN).hi

# start
start:
		./$(MAIN)