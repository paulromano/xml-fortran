# Naive makefile for the simple test program "tstparse"
#
# Coarse test for the XML-Fortran module
#

FC = gfortran
FCFLAGS = -c

all: xmlparse.a tstparse xmlreader

install:
	mkdir -p ../lib
	cp xmlparse.a ../lib
	cp xmlparse.mod ../lib

xmlparse.a: xmlparse.o read_xml_prims.o
	ar r xmlparse.a xmlparse.o read_xml_prims.o

xmlparse.o: xmlparse.f90
	$(FC) $(FCFLAGS) xmlparse.f90

tstparse: tstparse.f90 xmlparse.a
	$(FC) $(FCFLAGS) tstparse.f90
	$(FC) -o tstparse tstparse.o xmlparse.a

xmlreader: xmlreader.f90 xmlparse.a
	$(FC) $(FCFLAGS) xmlreader.f90
	$(FC) -o xmlreader xmlreader.o xmlparse.a

read_xml_prims.o: read_xml_prims.f90 xmlparse.f90
	$(FC) $(FCFLAGS) read_xml_prims.f90

clean:	
	rm -f *.o
	rm -f *.mod
	rm -f *.a
