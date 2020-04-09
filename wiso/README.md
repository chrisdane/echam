# wiso

isotope deltas (`lev1: d16O, lev2: d18O, lev3: `) in different variables

during model run:
```
/ace/user/paleo/utils.ace/cosmos-wiso/echam5/calc_wiso_monmean_d.cosmos-aso.sh
```

post:
```
https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_calc_wiso_echam5_yearmean.sh
cdo -s -f nc -chvar,wisoaprt,wisoaprt_d     -chcode,50,10 -mulc,1000. -subc,1. -div -div -yearsum wisoaprt   -yearsum aprt   $SMOW_FAC_file wisoaprt_d.yearmean
cdo -s -f nc -chvar,wisoaprl,wisoaprl_d     -chcode,53,13 -mulc,1000. -subc,1. -div -div -yearsum wisoaprl   -yearsum aprl   $SMOW_FAC_file wisoaprl_d.yearmean
cdo -s -f nc -chvar,wisoaprc,wisoaprc_d     -chcode,54,14 -mulc,1000. -subc,1. -div -div -yearsum wisoaprc   -yearsum aprc   $SMOW_FAC_file wisoaprc_d.yearmean
cdo -s -f nc -chvar,wisoaprs,wisoaprs_d     -chcode,55,15 -mulc,1000. -subc,1. -div -div -yearsum wisoaprs   -yearsum aprs   $SMOW_FAC_file wisoaprs_d.yearmean
cdo -s -f nc -chvar,wisoevap,wisoevap_d     -chcode,59,19 -mulc,1000. -subc,1. -div -div -yearsum wisoevap   -yearsum evap   $SMOW_FAC_file wisoevap_d.yearmean
cdo -s -f nc -chvar,wisope,wisope_d         -chcode,60,20 -mulc,1000. -subc,1. -div -div -yearsum wisope     -yearsum pe     $SMOW_FAC_file wisope_d.yearmean
cdo -s -f nc -chvar,wisows,wisows_d         -chcode,51,11 -mulc,1000. -subc,1. -div -div -yearsum wisows     -yearsum ws     $SMOW_FAC_file wisows_d.yearmean
cdo -s -f nc -chvar,wisosn,wisosn_d         -chcode,52,12 -mulc,1000. -subc,1. -div -div -yearsum wisosn     -yearsum sn     $SMOW_FAC_file wisosn_d.yearmean
cdo -s -f nc -chvar,wisosnglac,wisosnglac_d -chcode,76,33 -mulc,1000. -subc,1. -div -div -yearsum wisosnglac -yearsum snglac $SMOW_FAC_file wisosnglac_d.yearmean
cdo -s -f nc -chvar,wisorunoff,wisorunoff_d -chcode,57,17 -mulc,1000. -subc,1. -div -div -yearsum wisorunoff -yearsum runoff $SMOW_FAC_file wisorunoff_d.yearmean
cdo -s -f nc -chvar,temp2,ptemp -chcode,167,170  -div -mul -yearsum temp2 -yearsum aprt -yearsum aprt ptemp.yearmean
```

