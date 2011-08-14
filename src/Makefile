# Naive makefile for the simple test program "tstparse"
#
# Coarse test for the XML-Fortran module
#

#===============================================================================
# Compiler Options
#===============================================================================

F90 = gfortran
F90FLAGS = -g -fbacktrace

#===============================================================================
# Targets
#===============================================================================

all: xmlreader
test: tstparse
install:
	mkdir -p ../lib
	cp xmlparse.a ../lib
	cp xmlparse.mod ../lib
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

xmlparse.a: xmlparse.o read_xml_primitives.o write_xml_primitives.o
	ar r xmlparse.a xmlparse.o read_xml_primitives.o write_xml_primitives.o

tstparse: tstparse.o xmlparse.a
	$(F90) tstparse.o xmlparse.a -o $@

xmlreader: xmlreader.o xmlparse.a
	$(F90) xmlreader.o xmlparse.a -o $@

tstparse.o: xmlparse.o
xmlreader.o: xmlparse.o
