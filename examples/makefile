# Naive makefile for the example programs
#
# $Id: makefile,v 1.2 2006/05/31 04:36:04 arjenmarkus Exp $
#
# Coarse test for the XML-Fortran module
#
# Note:
# You may need to change the name of the Fortran compiler
# and the compile options
#
#FC	=	f90
#FC	=	lf95
FC	=	ifort
FOPT	=	-c -module ../src
FC	=	gfortran
FOPT	=	-c -I ../src

all		:	tst_grid tst_readint

tst_grid	:	tst_grid.o grid.o ../src/xmlparse.a
	$(FC) -o tst_grid tst_grid.o grid.o ../src/xmlparse.a

tst_grid.o	:	tst_grid.f90 grid.o
	$(FC) $(FOPT) tst_grid.f90

grid.o	:	grid.f90
	$(FC) $(FOPT) grid.f90

grid.f90	:	grid.xml
	echo grid > xmlreader.inp
	../src/xmlreader
	$(FC) $(FOPT) grid.f90

tst_readint	:	tst_readint.o readint.o ../src/xmlparse.a
	$(FC) -o tst_readint tst_readint.o readint.o ../src/xmlparse.a

tst_readint.o	:	tst_readint.f90 readint.o
	$(FC) $(FOPT) readint.f90

readint.o	:	readint.f90
	$(FC) $(FOPT) readint.f90

readint.f90	:	readint.xml
	echo readint > xmlreader.inp
	../src/xmlreader
	$(FC) $(FOPT) readint.f90
