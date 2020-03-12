# echam

changed `SAO_3D_COSMOS_standard.nc` to
```
ncatted -O -a table,any3Dvar,d,, mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncatted -O -a units,any3Dvar,d,, mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncatted -O -a long_name,any3Dvar,d,, mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncatted -O -a code,any3Dvar,d,, mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
cdo chname,SAO,any3Dvar mpiom_r360x180L40_geographic_grid_standard.nc tmp\n",
ncpdq -M hgh_byt mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncap2 -s SAO=int(SAO) mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncatted -a _FillValue,,m,i,0 mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncap2 -s where(SAO >= 0) SAO = 1 SAO_3D_COSMOS_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n"
```


