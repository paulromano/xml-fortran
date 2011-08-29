program = test_geometry
template = geometry_t
objects = $(program).o $(template).o

#===============================================================================
# User Options
#===============================================================================

COMPILER = intel
DEBUG    = no

#===============================================================================
# Compiler Options
#===============================================================================

# Set compiler defaults

F90 = gfortran
F90FLAGS = -I../../lib
LDFLAGS = -L../../lib -lxmlparse

# Intel Fortran compiler options

ifeq ($(COMPILER),intel)
  F90 = ifort
  F90FLAGS += -fpp -warn
endif

# GNU Fortran compiler options

ifeq ($(COMPILER),gfortran)
  F90 = gfortran
  F90FLAGS += -cpp -Wall
endif

# Set compiler flags for debugging

ifeq ($(DEBUG),yes)
  F90FLAGS += -g 
  LDFLAGS  += -g
  ifeq ($(COMPILER),intel)
    F90FLAGS += -traceback -ftrapuv -fp-stack-check -check all
  endif
  ifeq ($(COMPILER),gfortran)
    F90FLAGS += -pedantic -std=f2008 -fbacktrace -fbounds-check \
                -ffpe-trap=invalid,zero,overflow,underflow
  endif
endif

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
