# Naive makefile for the example programs
#
# Coarse test for the XML-Fortran module
#

FC = gfortran
FCFLAGS = -c -I ../src

all: tst_grid tst_readint

tst_grid: tst_grid.o grid.o ../src/xmlparse.a
	$(FC) -o tst_grid tst_grid.o grid.o ../src/xmlparse.a

tst_grid.o: tst_grid.f90 grid.o
	$(FC) $(FCFLAGS) tst_grid.f90

grid.o: grid.f90
	$(FC) $(FCFLAGS) grid.f90

grid.f90: grid.xml
	echo grid > xmlreader.inp
	../src/xmlreader
	$(FC) $(FCFLAGS) grid.f90

tst_readint: tst_readint.o readint.o ../src/xmlparse.a
	$(FC) -o tst_readint tst_readint.o readint.o ../src/xmlparse.a

tst_readint.o: tst_readint.f90 readint.o
	$(FC) $(FCFLAGS) tst_readint.f90

readint.o: readint.f90
	$(FC) $(FCFLAGS) readint.f90

readint.f90: readint.xml
	echo readint > xmlreader.inp
	../src/xmlreader
	$(FC) $(FCFLAGS) readint.f90

clean:
	rm -f *.o
	rm -f *.mod
	rm -f *.a
