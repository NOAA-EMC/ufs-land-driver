#!/bin/bash -l

YYYYMMDD=`ndate.py -d 0`

cd /cpc/home/li.xu/lxu/ufsland/ufs-land-driver/run/c96/


ufsland.exe &>ufs.out &

mean2day.py 19800101 365000 1 &>mean.out &


email_info "run UFSland ${YYYYMMDD}" "./mean.out" "li.xu@noaa.gov"
