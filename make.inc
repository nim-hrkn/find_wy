
TSPPATH=/home/kino/tmp/m_tspace
#INCPATH = -I $(TSPPATH)
TSP=$(TSPPATH)/m_tsp.a 


FC=gfortran  $(INCPATH)
FFLAGS=$(INCPATH)  -O2 -cpp -DNOSTOP -ffixed-line-length-255 -DUSE_GEN -std=legacy #-fcheck=all
FFLAGS_fixed= $(FFLAGS) -ffixed-line-length-132
FFLAGS_free= $(FFLAGS) -ffree-line-length-132


#FC=ifort $(INCPATH) -assume realloc_lhs 
#FFLAGS= $(INCPATH) -O2 -cpp -DUSE_GEN -DNOSTOP #  -g -traceback  -check  all
#FFLAGS_fixed= $(FFLAGS) -132
#FFLAGS_free= $(FFLAGS) 

