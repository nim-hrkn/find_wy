
.SUFFIXES:
.SUFFIXES: .f90 .f .o

include make.inc

#VPATH = .:select:tsp_modify


SELECT = select/select5.o  select/m_combisummary.o select/m_combination.o select/m_vector_i2.o 
#TSP= $(TSPPATH)/tsp98.o $(TSPPATH)/m_gen.o $(TSPPATH)/m_genwy.o $(TSPPATH)/gen.o  $(TSPPATH)/genwy.o 

.f.o:
	$(FC) -c -o $@ $(FFLAGS_fixed) $*.f  -Iselect
.f90.o:
	$(FC) -c -o $@ $(FFLAGS_free) $*.f90  -Iselect 

ALL:
	(cd select; make)
	make  all 

all: find_wy findsite write1wy

find_wy: find_wy.o m_tsp.o  m_wycoff.o  m_gn_ka.o  $(SELECT)   keyvaluev3.o  m_util.o  m_vector_c2.o  m_vector_r2.o  m_vector_t_xyz1.o m_fixedparam.o  m_xyz1.o  m_json_write.o 
	$(FC) -o $@  $^ $(TSP)
findsite: findsite_main.o m_tsp.o  m_wycoff.o  m_gn_ka.o 
	$(FC) -o $@  $^ $(TSP) 
write1wy: write1wy.o m_tsp.o  m_wycoff.o  m_gn_ka.o 
	$(FC) -o $@  $^ $(TSP) 

m_json_write.o: m_xyz1.o 

find_wy.o: m_tsp.o  select/select5.o  keyvaluev3.o m_util.o  m_vector_c2.o  m_vector_r2.o m_vector_t_xyz1.o m_fixedparam.o  m_xyz1.o  m_json_write.o 
findsite_main.o: m_tsp.o 

m_vector_t_xyz1.o:  m_xyz1.o 

m_tsp.o : $(TSP) m_wycoff.o m_gn_ka.o 

m_util.o: m_fixedparam.o  m_xyz1.o 

rebuild:
	(cd select; make clean;make )
	make clean; make all

clean:
	rm -f *.o *.mod 

cleanall:
	(cd select; make clean)
	make clean


cleanprog:
	rm -f find_wy findsite 
