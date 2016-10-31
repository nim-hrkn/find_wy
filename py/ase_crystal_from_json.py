#!/usr/bin/env python
import sys
from ase.spacegroup import crystal
from ase.io import write
import json


def print_usage():
	print "usage"
	print "thisscript file.json"
	sys.exit(100)

if __name__ == '__main__':

        try:
                filename=sys.argv[1]
        except:
                print_usage()
                sys.exit(100)

	#filename="../POS_WY.json"

        f = open(filename, 'r')
        data = json.load(f)
        f.close()

        #a= 4.946
        #c= 4.17
        #lat = crystal(('Fe', 'Fe'),
        #                       basis=[(0.5, 0.5, 0.111),(0.5, 0, 0)],
        #                       spacegroup=166,
        #                       cellpar=[a, a, c, 90, 90, 120])
        #lat = crystal(['Fe', 'Fe',"Nd"],
        #                       basis=[[0.5, 0, 0.5],[ 0.333333, 0.666667, 0],[0, 0, 0]], 
        #                       spacegroup=191,
        #                       cellpar=[a, a, c, 90, 90, 120])

	atoms=data["atoms"]
	elements=[ x["name"] for x in atoms ]
	positions= [ x["frac"] for x in atoms ]

	lat=crystal(elements,basis=positions,spacegroup=data["spacegroupid"],cellpar=data["lat"])

#        print lat

	print "summary"
        for (sym,pos) in zip(lat.get_chemical_symbols(),lat.get_positions()):
                print sym,pos


	filename="POSCAR.ase"
        filetype="vasp"
        write(filename,lat,format=filetype)
	print filename, " is made. for ",filetype

	filename="POS.new.json"
	filetype="json"
        write(filename,lat,format=filetype)
	print filename, " is made. for ",filetype


