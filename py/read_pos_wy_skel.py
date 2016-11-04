#!/usr/bin/env python
import sys
import json
import numpy as np
from ase.spacegroup import crystal
from ase.io import write


def print_usage():
        print("usage")
        print("thisscript file.json")
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


        print("original description")
        for each in data["atoms"]:
                print(each["name"],each["xyzch"],each["add"])

        print("conversion") 
        for each in data["atoms"]:
                rval = np.random.random_sample(3)
                val=[]
                for i,ch in enumerate(each["xyzch"]):
                        if ch=="0":
                                val.append(0.0)
                        elif ch=="x":
                                val.append(rval[0])
                        elif ch=="y":
                                val.append(rval[1])
                        elif ch=="z":
                                val.append(rval[2])
                        else:
                                print("internal error")
                                print("unknown ch=",ch)
                                sys.exit(100)
                val=np.array(val)

                print(each["name"],val+each["add"])


        sys.exit(0)
