# mpiom

renamed `bek.nc` to `mpiom_r360x180_bek.nc`

renamed `SAO_3D_COSMOS_standard.nc` to `mpiom_r360x180L40_geographic_grid.nc`

changed `SAO_3D_COSMOS_standard.nc` to byte precission to save space:
```bash
ncatted -a _FillValue,,o,f,-127 SAO_3D_COSMOS_standard.nc mpiom_r360x180L40_geographic_grid_byte.nc
ncpdq -O -M hgh_byt mpiom_r360x180L40_geographic_grid_byte.nc mpiom_r360x180L40_geographic_grid_byte.nc
```

convert mpiom data (`*.grb`) to nc and apply default parameter table:
```
cdo -t mpiom1 -f nc copy Hol-Tx10_mpiom_32900101_32901231.grb Hol-Tx10_mpiom_32900101_32901231.grb.nc
```

convert mpiom time series data (`*.ext`) to nc and apply parameter table `zeitser-wiso.partab`:
```bash
tar -xf fort_32900101_32901231.tar -C /target_dir
#   fort.75: var100 (gmoc), var101 (amoc)
#   fort.90: var69 (todo: find out)
#   fort.100: codetable var1-var223
#   fort.101: codetable var1-var223
#   fort.102: codetable var1-var223
#   TIMESER.32900101_32901231.asc: ascii data
#   TIMESER.32900101_32901231.ext: grb: var1-var223
cdo -f nc copy -setpartab,zeitser-wiso.partab TIMESER.32900101_32901231.ext TIMESER.32900101_32901231.ext.nc
```

