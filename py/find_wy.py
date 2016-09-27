"""
Copyright 2016,  Hiori Kino

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""


import os
import sys
import json
import subprocess
import hashlib
import numpy as np

class find_wy:
	""" python interface for find_wy"""
	def __init__(self, spacegroup, origin=1 ):
		self.spacegroup=spacegroup
		self.iorigin=origin


	def lat_info(self):
		""" run find_wy and get lattice information """

		self.prog="find_wy"
		if  "FIND_WY" in  os.environ: 
			self.prog= os.environ["FIND_WY"]

		dic={ "spacegroup": str(self.spacegroup),
			"orginchoice" : str(self.iorigin),
			"latinfo_only" : "true" }
		slist=[]
		for s in dic:
			slist.append( " ".join( [s,dic[s] , "\n" ] ) )

		hashstr=hashlib.md5()
		for s in slist:
			hashstr.update(s)

		self.cwd=os.getcwd()
		wdir="exec"+hashstr.hexdigest()
		self.execdir=os.path.join(self.cwd,wdir)
		if not os.path.isdir(self.execdir):
			try:
				os.mkdir(self.execdir)
			except:
				print "failed to make a directory ", self.execdir
				sys.exit(500)
		os.chdir(self.execdir)

		r=self.execute_prog(self.prog,slist)

		jsoninput="LAT.json"
		f= open(jsoninput, "r")
		data = json.load(f)
		f.close()
			

		# recover current directory
		os.chdir(self.cwd)

		# clean workdir
		if True:
			cmd=r"\rm -rf "+self.execdir
			print cmd
			ret=subprocess.call( cmd , shell=True )

                return data

	def get_positions(self,a,b,c,alpha,beta,gamma, species_name, species_num):
		self.prog="find_wy"
                if  "FIND_WY" in  os.environ:
                        self.prog= os.environ["FIND_WY"]

                dic={ "spacegroup": str(self.spacegroup),
                        "orginchoice" : str(self.iorigin),
                        "randomseed" : "auto", 
			"a" : str(a), 
			"b" : str(b), 
			"c" : str(c), 
			"cosa" : str(np.cos(alpha/180.0*np.pi)), 
			"cosb" : str(np.cos(beta/180.0*np.pi)), 
			"cosc" : str(np.cos(gamma/180.0*np.pi)), 
			"species_name" : " ".join(species_name) , 
			"species_num" : " ".join(map(str,species_num)),
			"nspecies" : str(len(species_name)) }
                slist=[]
                for s in dic:
                        slist.append( " ".join( [s,dic[s] , "\n" ] ) )

		print slist

                hashstr=hashlib.md5()
                for s in slist:
                        hashstr.update(s)

                self.cwd=os.getcwd()
                wdir="exec"+hashstr.hexdigest()
                self.execdir=os.path.join(self.cwd,wdir)
                if not os.path.isdir(self.execdir):
			try:
                        	os.mkdir(self.execdir)
			except:
                                print "failed to make a directory ", self.execdir
                                sys.exit(500)
                os.chdir(self.execdir)

                r=self.execute_prog(self.prog,slist)

                jsoninput="POS_WY_SKEL.json"
                f= open(jsoninput, "r")
                data_skel = json.load(f)
                f.close()

                jsoninput="POS_WY.json"
                f= open(jsoninput, "r")
                data = json.load(f)
                f.close()



                # recover current directory
                os.chdir(self.cwd)

                # clean workdir
                if True:
                        cmd=r"\rm -rf "+self.execdir
                        print cmd
                        ret=subprocess.call( cmd , shell=True )

                return [data_skel,data]



	def execute_prog(self,prog,slist):
		"""execute the program"""	
		inputfilename="input_sample.txt"
		f=open(inputfilename,"w")
		f.writelines(slist)
		f.close()
		cmd=" ".join( [prog, inputfilename ," > o", "2> e"] )
		ret=subprocess.call(cmd,shell=True)
		if ret !=0 :
			print "failed to run '",cmd,"'"
			print "return code=", ret 
			subprocess.call ( "cat e", shell=True) 
			sys.exit(200)

if __name__ == "__main__":

	fw=find_wy(143)
	lat=fw.lat_info()
	print lat

	species_name=["Si", "O"]
	species_num=[4, 8]


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

	# set seed if you want to make the same series of random numbers
	#seed=100
	#np.random(seed)
		
	print a,b,c,alpha,beta,gamma,species_name,species_num
	rskel,r=fw.get_positions( a,b,c,alpha,beta,gamma,species_name,species_num )
	print  rskel
	print r


