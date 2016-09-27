#!/usr/bin/env python


import os
import sys
import json
import subprocess
import hashlib
import numpy as np

# change this if necesssry
sys.path.append("py")
# or add the directory to PYTHONPATH 

import find_wy

def set_lat(lat):
	"""a sample subroutine to set lattice parameters """
	latlen_max=np.array([10,11,12])
	a=np.random.rand()*latlen_max[0]
	if lat["b"]=="a":
		b=a
	elif lat["b"]=="0":
		b=np.random.rand()*latlen_max[1]
	else:
		print "unknown character for b", lat["b"]
		sys.exit(500)

	if lat["c"]=="a":
		b=a
	elif lat["c"]=="b":
		c=b
	elif lat["c"]=="0":
		c=np.random.rand()*latlen_max[2]
	else:
		print "unknown character for c",lat["c"]
	
	if lat["alpha"]==0:
		alpha=np.random.rand()*120.0 # e.g.
	else:
		alpha=lat["alpha"]

	if lat["beta"] ==0 :
		beta= np.random.rand()*120.0 # e.g.
	else:
		beta=lat["beta"]

	if lat["gamma"] ==0 :
		gamma= np.random.rand()*120.0 # e.g.
	else:
		gamma=lat["gamma"]

	return a,b,c,alpha,beta,gamma 

if __name__ == "__main__":

	#fw=find_wy.find_wy(100) # error
	fw=find_wy.find_wy(143)
	lat=fw.lat_info()
	print lat

	#set seed if you want to make the same series of random numbers to make lat 
	seed=100
	np.random.seed(seed)

	a,b,c,alpha,beta,gamma = set_lat(lat) 

	species_name=["Si", "O"]
	species_num=[4, 8]
		
	print a,b,c,alpha,beta,gamma,species_name,species_num
	rskel,r=fw.get_positions( a,b,c,alpha,beta,gamma,species_name,species_num ,randomseed=seed)
	print  rskel
	print r


