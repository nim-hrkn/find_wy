# find\_wy

* 原子数が多い場合はrandom searchを行う。（defaultでは最大1000回）
* 原子数が少ない場合は全探索して一つ選択する。

内部で自動的に切り替える。

inputfile = input\_si2o4.txt 

# Check Wiki of this code.

py/ directory contains python script examples. 
They accepts both python2 and python3. 


# important!

modified tsp98 is necessary. First download
https://github.com/nim-hrkn/m_tspace
It uses tsp98. 

##  tsp98 

You must also download tsp98.f and prmtsp.f from 
[http://phoenix.mp.es.osaka-u.ac.jp/~tspace/tspace_main/tsp07/tsp98.f](http://phoenix.mp.es.osaka-u.ac.jp/~tspace/tspace_main/tsp07/tsp98.f), and 
[http://phoenix.mp.es.osaka-u.ac.jp/~tspace/tspace_main/tsp07/prmtsp.f](http://phoenix.mp.es.osaka-u.ac.jp/~tspace/tspace_main/tsp07/prmtsp.f).

I can't put it on this directory because the author of tspace doesn't make the license problem clear. 
