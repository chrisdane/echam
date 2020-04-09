# wiso

d18O in precipitation (aprt):
```
cdo -s -f nc -chvar,wisoaprt,wisoaprt_d -chcode,50,10 -mulc,1000. -subc,1. -div -div -yearsum wisoaprt -yearsum aprt $SMOW_FAC_file wisoaprt_d.yearmean
```

