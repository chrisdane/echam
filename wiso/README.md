# wiso

isotope deltas (`lev1: d16O, lev2: d18O, lev3: `) in different variables

during model run:
```
/ace/user/paleo/utils.ace/cosmos-wiso/echam5/calc_wiso_monmean_d.cosmos-aso.sh
${cdo} -s -f nc -monmean -mulc,2.628e6 -selcode,142 $IN aprl
${cdo} -s -f nc -monmean -mulc,2.628e6 -selcode,143 $IN aprc
${cdo} -s -f nc -monmean -mulc,2.628e6 -selcode,53 $IN_WISO wisoaprl
${cdo} -s -f nc -monmean -mulc,2.628e6 -selcode,54 $IN_WISO wisoaprc
${cdo} -s -f nc -chvar,aprl,aprt -chcode,142,260 -add aprl aprc aprt
${cdo} -s -f nc -chvar,wisoaprl,wisoaprt -chcode,53,50 -add wisoaprl wisoaprc wisoaprt
${cdo} -s -f nc -chvar,wisoaprt,wisoaprt_d     -chcode,50,10 -mulc,1000 -subc,1. -div -div wisoaprt   aprt   ${SMOW_FAC} wisoaprt_d
--> model-output wisoaprt_d with temporal output interval, e.g. monthly
```

post:
```
https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_calc_wiso_echam5_yearmean.sh
cdo -s -f nc -chvar,wisoaprt,wisoaprt_d     -chcode,50,10 -mulc,1000. -subc,1. -div -div -yearsum wisoaprt   -yearsum aprt   $SMOW_FAC_file wisoaprt_d.yearmean
--> post-processed annual (if yearsum) wisoaprt_d
```

