#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 21 23:49:03 2021
@author: li.xu
"""
import numpy as np
import matplotlib.pyplot as plt

from xu import ndates
import numpy as np
import matplotlib.pyplot as plt

from xu import ndates,run2,run

p_in = '/cpc/drought/pdf/ufs/tmpoutput'

p_out = '/cpc/drought/pdf/ufs/output/c96'
p_g05 = '/cpc/drought/pdf/ufs/output/g05'

from deco import *

import time

@synchronized
def do_day(yyyymmdd='20200101', n=35,m=10):
    for i in range(n):
        date = ndates(i * 24, yyyymmdd, fmt='%Y-%m-%d')
        print(date)
        fin = f'{p_in}/*{date}*.nc'
        fout = f'{p_out}/ufs_land_output.{date}.nc'
        ncea(fin, fout)
        print('deleting...')
        run2(f'rm -f {fin}')
        time.sleep(m)


@concurrent
def ncea(fin='/p/t*.nc', fout='/p/te.nc'):
    run2(f'/cpc/home/li.xu/anaconda3/bin/ncea {fin} -O {fout}')


def test():
    ncea('/cpc/home/li.xu/lxu/noahmp/NoahMP-us05/output/*19790203*',
         '/tmp/tmp.nc')



def do_day2(yyyymmdd='20200101', n=35,m=1):
    from glob import glob

    p_in = '/run/user/4986/'
    def ncea(fin='/p/t*.nc', fout='/p/te.nc'):
        run(f'/cpc/home/li.xu/anaconda3/bin/ncea {fin} -O {fout}')

    for _ in range(10):
        print('cycle:',_)
        for i in range(n):
            date = ndates(i * 24, yyyymmdd, fmt='%Y-%m-%d')
            fin = f'{p_in}/*{date}*.nc'
            files=glob(fin)
            if len(files) == 0: continue

            count=0
            while  len(files) < 23:
                print(date)
                print('wait: ',m,'sec, for times:',count)
                time.sleep(m)
                count +=1
                files=glob(fin)
                if count >60:
                    print('deleting...',files)
                    run(f'rm -f {fin}')
                    break

            else:
                time.sleep(1)
                print('all 24 files in:')
                fout = f'{p_out}/ufs_land_output.{date}.nc'
                print('fout:',fout)
                ncea(fin, fout)
                print('deleting...')
                run(f'rm -f {fin}')



def main(yyyymmdd,n):
    print(yyyymmdd)


if __name__ == '__main__':
    import sys
    import fire
    if len(sys.argv) <= 1:
        print('input: YYYYMMDD nday nsec(10) or -h')
    else:
        if len(sys.argv[1]) == 8:
            do_day2(sys.argv[1],int(sys.argv[2]),int(sys.argv[3]))
        else:
            fire.Fire()
