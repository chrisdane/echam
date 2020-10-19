# wiso

isotope deltas (`lev1: d16O, lev2: d18O, lev3: `) in different variables

during model run:
```
/ace/user/paleo/utils.ace/cosmos-wiso/echam5/calc_wiso_monmean_d.cosmos-aso.sh
# convert 2m-temperature and surface temperature from K to C)
${cdo} -s -f nc -monmean -subc,273.15  -selcode,167 $IN temp2
# convert  precipitation, evaporation and runoff fields from kg/m**2s to mm/month
# (conversion factor: 3600*24*365/12*1000/1000 = 2.628e6)
${cdo} -s -f nc -monmean -mulc,2.628e6 -selcode,142 $IN aprl

# calculate total precipitation
${cdo} -s -f nc -chvar,aprl,aprt -chcode,142,260 -add aprl aprc aprt

# get related water isotope fields and reservoirs
${cdo} -s -f nc -monmean -mulc,2.628e6 -selcode,53 $IN_WISO wisoaprl

# calculate total precipitation
${cdo} -s -f nc -chvar,wisoaprl,wisoaprt -chcode,53,50 -add wisoaprl wisoaprc wisoaprt

# set ECHAM defaults fields to 0. if flux fields (reservoirs) are less than 0.05 mm/month (0.05 mm)
${cdo} -s -f nc -setmisstoc,0. -ifthen -gec,0.05 aprt aprt dummy; mv dummy aprt

# set water isotope fields to 0. if ECHAM default fields (reservoirs) are zero
${cdo} -s -f nc -setmisstoc,0. -ifthen -nec,0. aprt wisoaprt dummy; mv dummy wisoaprt

# calculate delta values of all water isotope fields (reference standard: SMOW) 
# - the SMOW values have to be stored in the file SMOW_FAC (with the correct grid size & order of isotope values!)
${cdo} -s -f nc -chvar,wisoaprt,wisoaprt_d     -chcode,50,10 -mulc,1000 -subc,1. -div -div wisoaprt   aprt   ${SMOW_FAC} wisoaprt_d
```

post:
```
https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_calc_wiso_echam5_yearmean.sh
cdo -s -f nc -chvar,wisoaprt,wisoaprt_d     -chcode,50,10 -mulc,1000. -subc,1. -div -div -yearsum wisoaprt   -yearsum aprt   $SMOW_FAC_file wisoaprt_d.yearmean
--> post-processed annual (if yearsum) wisoaprt_d
```
precipitation-weighted temperature:
```
cdo -s -f nc -chvar,temp2,ptemp -chcode,167,170  -div -mul -yearsum temp2 -yearsum aprt -yearsum aprt ptemp.yearmean
```

