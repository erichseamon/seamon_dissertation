#!/bin/sh
#
# NAME: climate-subset.sh
# PURPOSE: Bash script that is called by R for subsetting climatic datasets
# AUTHOR: Erich Seamon
# INSTITUTION: University of Idaho
# DATE: 12/10/2019
#

source /tmp/agmesh-subset-R-scenario.txt

#echo "enter first year of data range: "
#read yearstart
#echo "enter last year of data range: "
#read yearend
#echo "enter the minimum latitude:"
#read lat1
#echo "enter the maximum latitude:"
#read lat2
#echo "enter the minimum longitude:"
#read lon1
#echo "enter the maximum longitude:"
#read lon2
yearspan=$(seq $yearstart $yearend)
scenario="gridmet_monthly"

for i in $yearspan; do 

#-jan

jandate = jan
varyearfinal = $jandate$varyear
ncra -d day,1,31 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyearfinal.nc

ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyearfinal.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyearfinal.nc

#-feb

ncra -d day,32,59 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-march
ncra -d day,59,90 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-april
ncra -d day,91,120 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-may
ncra -d day,121,151 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-june
ncra -d day,152,181 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-july
ncra -d day,182,212 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-august
ncra -d day,213,243 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-sept
ncra -d day,244,273 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-oct
ncra -d day,274,304 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#-nov
ncra -d day,305,334 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#dec
ncra -d day,335,365 /reacchspace/obj1/netcdf/MET/data/$varyear.nc / erich_out_vs_2016_2.nc
ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc








var=pdsi_
var2=pdsi
varyear=$var$i
echo "Calculating and creating" $i "..."

ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc

var=fm1000_
var2=fm1000
varyear=$var$i
echo "Calculating and creating" $i "..."

ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc

var=fm1_
var2=fm1
varyear=$var$i
echo "Calculating and creating" $i "..."

ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc

var=fm100_
var2=fm100
varyear=$var$i
echo "Calculating and creating" $i "..."

ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc


var=pet_
var2=pet
varyear=$var$i
echo "Calculating and creating" $i "..."

ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc


var=erc_
var2=erc
varyear=$var$i
echo "Calculating and creating" $i "..."

ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc


#var=pdur_
#var2=pdur
#varyear=$var$i
#echo "Calculating and creating" $i "..."

#ncks -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
#ncpdq -O -a lat,lon,day /agmesh-scenarios/$scenario/$var2/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc

var=pr_
var2=pr
varyear=$var$i


ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc

var=tmmx_
var2=tmmx
varyear=$var$i

ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc


var=tmmn_
var2=tmmn
varyear=$var$i



ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc


var=srad_
var2=srad
varyear=$var$i



ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc

var=rmin_
var2=rmin
varyear=$var$i



ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc

var=rmax_
var2=rmax
varyear=$var$i


ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc


var=vs_
var2=vs
varyear=$var$i


ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc


var=th_
var2=th
varyear=$var$i


ncea -O -d lat,$lat1,$lat2 -d lon,$lon1,$lon2 /reacchspace/obj1/netcdf/MET/data/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc
ncpdq -O -a day,lon,lat /agmesh-scenarios/$scenario/$varyear.nc /agmesh-scenarios/$scenario/$varyear.nc









done


