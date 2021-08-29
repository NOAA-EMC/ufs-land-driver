#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 27 02:31:39 2021
@author: li.xu

contact:
    Li Xu
    CPC/NCEP/NOAA
    (301)683-1548
    li.xu@noaa.gov

"""
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr

from xu import ndates,run2,last_day

#/cpc/drought/pdf/noahmp/conus_0p5/restart/ufs_land_restart.2021-01-31_23-00-00.nc
def keep(f):
    '''last day of month to keep'''
    dt=f[-22:-3] # totla 19char
    date=dt[:10]
    yyyy=date[:4]
    mm=date[5:7]
    dd=date[8:10]

    if dd == last_day(yyyy,mm):
        return True
    else:
        return False


p='/cpc/drought/pdf/noahmp/conus_0p5/restart/ufs_land_restart.*.nc'

from glob import glob

files=glob(p)

for f in files:
    if not keep(f):
        run2(f'mv {f} /cpc/drought/pdf/noahmp/conus_0p5/trash')
    else:
        print('keep:',f)

