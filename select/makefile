

#FC=gfortran
#FFLAGS= -g -O0  -cpp #-DHAVE_MAIN

#FC=ifort 
#FFLAGS= -g -traceback -check all -O0 -cpp  -check all

include ../make.inc 

#FFLAGS= -O3 -cpp 


.SUFFIXES:
.SUFFIXES: .f90 .o 

.f90.o:
	$(FC) -o $@ -c $(FFLAGS) $*.f90

OBJ= select5.o m_vector_i2.o m_combisummary.o  m_combination.o


prog: select_main

obj: $(OBJ)

#select: select_main.o select5.o m_vector_i2.o m_combisummary.o  m_combination.o 
#	$(FC) -o $@  $^


select_main: select_main.o $(OBJ) 
	$(FC) -o $@ $^ 

select_main.o: select5.o 

select5.o: m_vector_i2.o m_combisummary.o  m_combination.o 

m_combisummary.o : m_combination.o  m_vector_i2.o 

m_combination.o: m_vector_i2.o 


select2: select2.o
	$(FC) -o $@ select2.o 

#select: select.o
#	$(FC) -o $@ select.o 
#select_site2: select_site2.o
#	$(FC) -o $@ select_site2.o 

recursiveloop2: recursiveloop2.o m_vector_i2.o
	$(FC) -o $@ recursiveloop2.o m_vector_i2.o

clean:
	rm -f *.o *.mod	

