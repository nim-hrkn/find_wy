#!/usr/bin/env python
from __future__ import print_function 
import sys
import json
import numpy as np


def print_usage():
        print("usage")
        print("thisscript file_skel_all.json")
        sys.exit(100)

def conversion(data,clat):
        atomnames=[]
        positions=[]
        for specie in data["atoms"]:
           for data2 in specie:
                rval = np.random.random_sample(3)
                for each in data2:    
                    val=[]
                    for i,ch in enumerate(each["xyzch"]):
			if ch=="-2x":
                                val.append(-2.0*rval[0])
                        elif ch=="-x+y":
                                val.append(-rval[0]+rval[1])
                        elif ch=="-z":
                                val.append(-rval[2])
                        elif ch=="-y":
                                val.append(-rval[1])
                        elif ch=="-x":
                                val.append(-rval[0])
                        elif ch=="0":
                                val.append(0.0)
                        elif ch=="x":
                                val.append(rval[0])
                        elif ch=="y":
                                val.append(rval[1])
                        elif ch=="z":
                                val.append(rval[2])
                        elif ch=="x-y":
                                val.append(rval[0]-rval[1])
                        elif ch=="2x":
                                val.append(2.0*rval[0])
                        else:
                                print("conversion, internal error")
                                print("unknown ch=",ch)
                                sys.exit(100)
                    val=np.array(val)
                    positions.append( val+each["add"] )
                    atomnames.append( each["name"] )

        cart=[]
        for p in positions:
                v=np.zeros(3)
                for i in range(3):
                        a=np.array(clat[i])
                        v += p[i]* a 
                cart.append(v)

        return atomnames, cart 

def count_atoms(atomnames):
        na=[]
        for a in atomnames:
               if not a in na:
                     na.append(a)
        dica={}
        for a in na:
                dica[a]=0
        for a in atomnames:
                 dica[a]  += 1
        ib=[]
        for a in na:
                ib.append(  dica[a]  )
        return na, ib

def xyz2poscar(plat,atomnames,positions,filename="POSCAR.new"):
        slist=[]
        slist.append("find_wy")
        slist.append("1.0")
        for p in plat:
                s=map(str,p)
                slist.append(" ".join(s))

        a,ia = count_atoms(atomnames)
        slist.append(" ".join(a) )
        slist.append(" ".join(map(str,ia)))
        slist.append("Cart")
        for a,ps in zip(atomnames,positions):
                slist.append(" ".join(map(str,ps)) )

        with open(filename,"wb") as f:
                nl="\n"
                for s in slist:
                        f.write((s+nl).encode())
        print ( filename,"is made." )


if __name__ == '__main__':

        try:
                filename=sys.argv[1]
        except:
                print_usage()
                sys.exit(100)

        with open(filename, 'r') as f:
                data = json.load(f)

        plat=data["primitivevector"]
        clat=data["conventionalvector"]
        atomnames,cart = conversion(data,clat)

        xyz2poscar(plat,atomnames,cart)

