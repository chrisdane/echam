# echam

changed `SAO_3D_COSMOS_standard.nc` to
```bash
ncap2 -s where(SAO >= 0) SAO = 1 SAO_3D_COSMOS_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n"
ncatted -a _FillValue,,m,i,0 mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncap2 -s SAO=int(SAO) mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncpdq -M hgh_byt mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
cdo chname,SAO,any3Dvar mpiom_r360x180L40_geographic_grid_standard.nc tmp\n",
ncatted -O -a code,any3Dvar,d,, mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncatted -O -a long_name,any3Dvar,d,, mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncatted -O -a units,any3Dvar,d,, mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
ncatted -O -a table,any3Dvar,d,, mpiom_r360x180L40_geographic_grid_standard.nc mpiom_r360x180L40_geographic_grid_standard.nc\n",
```

corrected `zeitser-wiso.partab` for time series data (`*.ext`) of mpiom wiso:
```bash
tar -xvf fort_32900101_32901231.tar
#   fort.75: var100 (gmoc), var101 (amoc)
#   fort.90: var69 (todo: find out)
#   fort.100: codetable var1-var223
#   fort.101: codetable var1-var223
#   fort.102: codetable var1-var223
#   TIMESER.32900101_32901231.asc: ascii data
#   TIMESER.32900101_32901231.ext: grb: var1-var223
# mpiom_functions.r: correct_mpiom_partabn(partabn_file="zeitser-wiso.partab")
#   change e.g. "  NAME=c1_PSIGULF" to "  out_name=c1_PSIGULF"
#   change e.g "  CODE=1" to "  name=var12"
cdo -f nc copy TIMESER.32900101_32901231.ext TIMESER.32900101_32901231.ext.nc
cdo setpartabn,mpiom_wiso_zeitser_partabn_corrected.txt TIMESER.32900101_32901231.ext.nc tmp && mv tmp TIMESER.32900101_32901231.ext.nc
# but for *.grb mpiom output
cdo -t mpiom1 -f nc copy Hol-Tx10_mpiom_32900101_32901231.grb Hol-Tx10_mpiom_32900101_32901231.grb.nc
```


