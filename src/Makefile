# Naive makefile for the simple test program "tstparse"
#
# Coarse test for the XML-Fortran module
#

#===============================================================================
# User Options
#===============================================================================

COMPILER = intel
DEBUG    = no

#===============================================================================
# Compiler Options
#===============================================================================

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

all: xmlreader
test: tstparse
install:
	mkdir -p ../lib
	cp libxmlparse.a ../lib
	cp xmlparse.mod read_xml_primitives.mod write_xml_primitives.mod ../lib
clean:
	@rm -f *.o *.mod *.a xmlreader tstparse
neat:
	@rm -f *.o *.mod *.a

#===============================================================================
# Rules
#===============================================================================

.SUFFIXES: .f90 .o
.PHONY: all clean neat

%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

#===============================================================================
# Dependencies
#===============================================================================

libxmlparse.a: xmlparse.o read_xml_primitives.o write_xml_primitives.o
	ar r libxmlparse.a xmlparse.o read_xml_primitives.o write_xml_primitives.o

tstparse: tstparse.o libxmlparse.a
	$(F90) tstparse.o libxmlparse.a -o $@

xmlreader: xmlreader.o libxmlparse.a
	$(F90) xmlreader.o libxmlparse.a -o $@

tstparse.o: xmlparse.o
xmlreader.o: xmlparse.o
