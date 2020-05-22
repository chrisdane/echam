
# job to calculate the vertically integrated water vapor transport

# it is assumed that the following variables exist in the input file (on Pa pressure levels):
# aps : surface pressure
# q   : atmospheric water vapour (on 1000, 850, 700, 500, 250 & 100hPa)
# u   : atmospheric wind field - u component (on 1000, 850, 700, 500, 250 & 100hPa)
# v   : atmospheric wind field - v component (on 1000, 850, 700, 500, 250 & 100hPa)

# the output of this job are 2 files:
# *.qtot_u.nc : vertically integrated water vapor transport - u component
# *.qtot_v.nc : vertically integrated water vapor transport - v component


FILE_IN=../$1.nc
FILE_OUT1=../$1.qtot_u.nc
FILE_OUT2=../$1.qtot_v.nc

mkdir tmp_qtransport
cd tmp_qtransport

cdo -selvar,aps $FILE_IN dummy.PS.nc
cdo -selvar,q $FILE_IN dummy.Q.nc
cdo -selvar,u $FILE_IN dummy.U.nc
cdo -selvar,v $FILE_IN dummy.V.nc

# calculate pressure difference between the different levels
cdo -ifthen -gtc,92500. dummy.PS.nc -subc,92500. dummy.PS.nc delta_pres.1.nc

cdo -ifthen -gtc,77500. dummy.PS.nc -subc,77500. dummy.PS.nc delta_pres.2.nc
cdo -ifnotthen -gtc,15000 delta_pres.2.nc delta_pres.2.nc dummy.nc
cdo -setmisstoc,0. dummy.nc delta_pres.2.nc
cdo -add -setmisstoc,0. -mulc,15000. -div delta_pres.1.nc delta_pres.1.nc delta_pres.2.nc dummy.nc
cdo -setctomiss,0. dummy.nc delta_pres.2.nc

cdo -ifthen -gtc,60000. dummy.PS.nc -subc,60000. dummy.PS.nc delta_pres.3.nc
cdo -ifnotthen -gtc,17500 delta_pres.3.nc delta_pres.3.nc dummy.nc
cdo -setmisstoc,0. dummy.nc delta_pres.3.nc
cdo -add -setmisstoc,0. -mulc,17500. -div delta_pres.2.nc delta_pres.2.nc delta_pres.3.nc dummy.nc
cdo -setctomiss,0. dummy.nc delta_pres.3.nc

cdo -ifthen -gtc,37500. dummy.PS.nc -subc,37500. dummy.PS.nc delta_pres.4.nc
cdo -ifnotthen -gtc,22500 delta_pres.4.nc delta_pres.4.nc dummy.nc
cdo -setmisstoc,0. dummy.nc delta_pres.4.nc
cdo -add -setmisstoc,0. -mulc,22500. -div delta_pres.3.nc delta_pres.3.nc delta_pres.4.nc dummy.nc
cdo -setctomiss,0. dummy.nc delta_pres.4.nc

cdo -ifthen -gtc,17500. dummy.PS.nc -subc,17500. dummy.PS.nc delta_pres.5.nc
cdo -ifnotthen -gtc,20000 delta_pres.5.nc delta_pres.5.nc dummy.nc
cdo -setmisstoc,0. dummy.nc delta_pres.5.nc
cdo -add -setmisstoc,0. -mulc,20000. -div delta_pres.4.nc delta_pres.4.nc delta_pres.5.nc dummy.nc
cdo -setctomiss,0. dummy.nc delta_pres.5.nc

cdo -ifthen -gtc,0. dummy.PS.nc -subc,0. dummy.PS.nc delta_pres.6.nc
cdo -ifnotthen -gtc,17500 delta_pres.6.nc delta_pres.6.nc dummy.nc
cdo -setmisstoc,0. dummy.nc delta_pres.6.nc
cdo -add -setmisstoc,0. -mulc,17500. -div delta_pres.5.nc delta_pres.5.nc delta_pres.6.nc dummy.nc
cdo -setctomiss,0. dummy.nc delta_pres.6.nc

rm dummy.nc

# multiply water vapour by wind component and pressure "thickness" of the different levels

cdo -setmisstoc,0. -mul -mul -sellevel,100000 dummy.Q.nc -sellevel,100000 dummy.U.nc delta_pres.1.nc dummy.Q1U.nc
cdo -setmisstoc,0. -mul -mul -sellevel,85000 dummy.Q.nc -sellevel,85000 dummy.U.nc delta_pres.2.nc dummy.Q2U.nc
cdo -setmisstoc,0. -mul -mul -sellevel,70000 dummy.Q.nc -sellevel,70000 dummy.U.nc delta_pres.3.nc dummy.Q3U.nc
cdo -setmisstoc,0. -mul -mul -sellevel,50000 dummy.Q.nc -sellevel,50000 dummy.U.nc delta_pres.4.nc dummy.Q4U.nc
cdo -setmisstoc,0. -mul -mul -sellevel,25000 dummy.Q.nc -sellevel,25000 dummy.U.nc delta_pres.5.nc dummy.Q5U.nc
cdo -setmisstoc,0. -mul -mul -sellevel,10000 dummy.Q.nc -sellevel,10000 dummy.U.nc delta_pres.6.nc dummy.Q6U.nc

cdo -setmisstoc,0. -mul -mul -sellevel,100000 dummy.Q.nc -sellevel,100000 dummy.V.nc delta_pres.1.nc dummy.Q1V.nc
cdo -setmisstoc,0. -mul -mul -sellevel,85000 dummy.Q.nc -sellevel,85000 dummy.V.nc delta_pres.2.nc dummy.Q2V.nc
cdo -setmisstoc,0. -mul -mul -sellevel,70000 dummy.Q.nc -sellevel,70000 dummy.V.nc delta_pres.3.nc dummy.Q3V.nc
cdo -setmisstoc,0. -mul -mul -sellevel,50000 dummy.Q.nc -sellevel,50000 dummy.V.nc delta_pres.4.nc dummy.Q4V.nc
cdo -setmisstoc,0. -mul -mul -sellevel,25000 dummy.Q.nc -sellevel,25000 dummy.V.nc delta_pres.5.nc dummy.Q5V.nc
cdo -setmisstoc,0. -mul -mul -sellevel,10000 dummy.Q.nc -sellevel,10000 dummy.V.nc delta_pres.6.nc dummy.Q6V.nc

# integrate over all levels, divide by g => final unit is [kg_H2O / m-1 / s-1]

cdo -divc,9.81 -add -add -add -add -add dummy.Q1U.nc dummy.Q2U.nc dummy.Q3U.nc dummy.Q4U.nc dummy.Q5U.nc dummy.Q6U.nc dummy.Qtot_U.nc
cdo -divc,9.81 -add -add -add -add -add dummy.Q1V.nc dummy.Q2V.nc dummy.Q3V.nc dummy.Q4V.nc dummy.Q5V.nc dummy.Q6V.nc dummy.Qtot_V.nc
#cdo -divc,9.81 -add -add -add -add dummy.Q2U.nc dummy.Q3U.nc dummy.Q4U.nc dummy.Q5U.nc dummy.Q6U.nc dummy.Qtot_U.nc
#cdo -divc,9.81 -add -add -add -add dummy.Q2V.nc dummy.Q3V.nc dummy.Q4V.nc dummy.Q5V.nc dummy.Q6V.nc dummy.Qtot_V.nc
#
mv dummy.Qtot_U.nc $FILE_OUT1
mv dummy.Qtot_V.nc $FILE_OUT2

# clean up
rm delta_pres.?.nc dummy.Q?[UV].nc dummy.PS.nc dummy.?.nc

cd ..
rmdir tmp_qtransport

# done
echo "Done."
exit
