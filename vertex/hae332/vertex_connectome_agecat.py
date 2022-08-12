#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 11 11:59:01 2022

@author: ali
"""

pip install xarray

import numpy as np
import pyreadr


data_path='/Users/ali/Desktop/Jul/apoe2_paper/vertex/' 
x=np.load(data_path +"connectivity.npy")
x.shape
xx=x.transpose()
xx.shape
x=xx


####age

y=pyreadr.read_r(data_path + 'response.rda')
y=y["response"]
print(y)
y=y["age_cat"]



#V,c,d, plt = vertex(x,y,z=y4, return_plot=True, verbose=True)
V,c,d, plt = vertex(x,y, return_plot=True, verbose=True)


import pandas
atlas=pandas.read_csv('/Users/ali/Desktop/Jul/apoe/mouse_anatomy.csv')
#noreadcsf=[148,152,161,314,318,327]
# remove white matter too:
#noreadcsf=[ 148, 152, 161, 314, 318, 327, 120, 121, 122, 134, 102, 118, 119, 123, 124, 125, 126,
#127, 128, 129, 130, 131, 132, 133, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144,
#145, 146, 147, 150, 268, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295,
#296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312,
#313, 316]

#noreadcsf=pyreadr.read_r(data_path + 'noreadcsf.rda')
#noreadcsf=noreadcsf["noreadcsf"]
#noreadcsf=list(set(noreadcsf))
#noreadcsf=noreadcsf.stack().tolist()

#noreadcsf=[int(x - 1) for x in noreadcsf]



#atlas=atlas.drop(atlas.index[noreadcsf])

#report=atlas.iloc[V]
#report['score']=c
#report=report.sort_values('score', ascending=False)

#print(report)

report.to_csv(data_path+'report_age_cat.csv', index=False)

V_list=[x+1 for x in range(332)]
V_panda=[int(x +1) for x in V]



np.savetxt(data_path+'vertices_age_cat.csv', V_panda, delimiter=',', fmt='%s')
#V_panda=pandas.DataFrame (V_panda)
#V_panda.to_csv(data_path+'vertices.csv', index=False)
