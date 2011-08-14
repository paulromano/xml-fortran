program = tst_grid
template = grid
objects = $(program).o $(template).o ../../src/xmlparse.a

#===============================================================================
# Compiler Options
#===============================================================================

F90 = gfortran
F90FLAGS = -g -fbacktrace -I../../src
LDFLAGS = 

#===============================================================================
# Targets
#===============================================================================

all: $(program)
$(program): $(objects)
	$(F90) $(objects) -o $@ $(LDFLAGS)
clean:
	@rm -f *.o *.mod *.out $(template).f90 $(program)
neat:
	@rm -f *.o *.mod *.out $(template).f90

#===============================================================================
# Rules
#===============================================================================

.SUFFIXES: .f90 .o
.PHONY: all clean neat

#===============================================================================
# Dependencies
#===============================================================================

$(program).o: $(template).o
	$(F90) -c $(F90FLAGS) $(program).f90

$(template).o: $(template).xml
	../../src/xmlreader $(template)
	$(F90) -c $(F90FLAGS) $(template).f90
