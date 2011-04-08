#!/bin/bash

LIBDIR=$mgfourbase$/pythia-pgs/libraries/PGS4/lib/
PYTHIALIB=$mgfourbase$/pythia-pgs/libraries/pylib/lib/
PDFLIB=$mgfourbase$/pythia-pgs/libraries/lhapdf/lib/

gfortran -c -m64 hepevt2stdhep.f
gfortran -m64 -o hepevt2stdhep.iw hepevt2stdhep.o $$LIBDIR/libpgslib.a $$LIBDIR/libexthep.a  $$LIBDIR/libstdhep.a $$LIBDIR/libFmcfio.a $$PYTHIALIB/libpythiaext.a   $$LIBDIR/libtauola.a



gfortran -c -m64  hep2lhe.f
gfortran -c -m64 ME2pythia.f
gfortran -c -m64 getjet.f
gfortran -c -m64 ktclusdble.f
gfortran -c -m64 pgs_ranmar.f
gfortran -c -m64 stdhep_print.f
gfortran -m64 -o hep2lhe.iw hep2lhe.o ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o $$LIBDIR/libtauola.a $$PYTHIALIB/libpythiaext.a $$LIBDIR/libstdhep.a $$LIBDIR/libFmcfio.a $$PDFLIB/libLHAPDF.a

gfortran -m64 -o stdhep_print.iw stdhep_print.o ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o $$LIBDIR/libtauola.a $$PYTHIALIB/libpythiaext.a $$LIBDIR/libstdhep.a $$LIBDIR/libFmcfio.a $$PDFLIB/libLHAPDF.a



#gfortran -o pythia.iw pythia.f ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o $$LIBDIR/libtauola.a $$PYTHIALIB/libpythiaext.a $$LIBDIR/libstdhep.a $$LIBDIR/libFmcfio.a $$PDFLIB/libLHAPDF.a


