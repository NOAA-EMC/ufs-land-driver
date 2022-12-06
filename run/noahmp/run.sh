#!/bin/bash -l

mkdir -p /dev/shm/lixu/noahmp
rm -rf /dev/shm/lixu/noahmp/*

nohup ufsLand.exe &> ufs.log &
mean2day.py 19500101 400 1 &>mean2day.log &


wait

email_info "run uldas " "./ufs.log" "li.xu@noaa.gov"


