program = writexml
objects = $(program).o ../../src/xmlparse.a

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
	@rm -f *.o *.mod $(program)
neat:
	@rm -f *.o *.mod

#===============================================================================
# Rules
#===============================================================================

.SUFFIXES: .f90 .o
.PHONY: all clean neat

%.o: %.f90
	$(F90) -c $(F90FLAGS) $<
