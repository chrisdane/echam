# mpiom

# convert from grb to nc with parameter table
convert mpiom grb data to nc and apply default parameter table:
```bash
cdo -t mpiom1 -f nc copy Hol-Tx10_mpiom_32900101_32901231.grb Hol-Tx10_mpiom_32900101_32901231.grb.nc
```

# regridding
regrid from bi-/tripolar mpiom grids to regular grids with
```bash
# GR15 (bipolar) grb scalar
cdo -remabil,r360x180 -setgrid,gridfile_s -sethalo,-1,-1 sin sout
# GR15 grb vector
cdo -remabil,r360x180 -mrotuvb -setgrid,gridfile_u -sethalo,-1,-1 uin -setgrid,gridfile_v -sethalo,-1,-1 vin uvout
# GR15 nc scalar
cdo -remabil,r360x180 -sethalo,-1,-1 sin sout
# GR15 nc vector
cdo -remabil,r360x180 -sethalo,-1,-1 -mrotuvb uin vin uvout
# TP04 (tripolar) grb scalar
cdo -remabil,r360x180 -selindexbox,1,800,3,404 -setgrid,gridfile_s -sethalo,-1,-1 sin sout
# TP04 grb vector
cdo -remabil,r360x180 -selindexbox,1,800,3,404 -mrotuvb -setgrid,gridfile_u -sethalo,-1,-1 uin -setgrid,gridfile_v -sethalo,-1,-1 vin uvout
# TP04 nc scalar
cdo -remabil,r360x180 -selindexbox,2,801,3,404 -sethalo,-1,-1 sin sout
# TP04 nc vector
cdo -remabil,r360x180 -selindexbox,2,801,3,404 -mrotuvb uin vin uvout
```
with
```
grid_s: /pool/data/MPIOM/GR15/GR15s.nc, /pool/data/MPIOM/TP04/TP04s.nc
grid_u: /pool/data/MPIOM/GR15/GR15u.nc, /pool/data/MPIOM/TP04/TP04u.nc
grid_v: /pool/data/MPIOM/GR15/GR15v.nc, /pool/data/MPIOM/TP04/TP04v.nc 
```

# convert time series data .ext
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

# misc
get sea ice values above a threshold:
```bash
ncap2 -O -s "where(SICOMO<0) SICOMO=0" in out
ncap2 -O -s "where(SICOMO<=0.15) SICOMO=0" in out
ncap2 -O -s SICOMO=float(SICOMO>>0.15) in out
```

difference between sea ice area and extent
https://nsidc.org/arcticseaicenews/faq/#area_extent
"Extent would be a measure of the edges of the slice of cheese and all of the space inside it. Area would be the measure of where there is cheese only, not including the holes."
```
--> area   = sum_i (A_cell_i * SIC_cell_i ) for cell i [optional: for which SIC_cell_i > 15 %]
--> extent = sum_i (A_cell_i)               for cell i [optional: for which SIC_cell_i > 15 %]
```

History:
- renamed `bek.nc` to `mpiom_r360x180_bek.nc`
- renamed `SAO_3D_COSMOS_standard.nc` to `mpiom_r360x180L40_geographic_grid.nc`
- changed `SAO_3D_COSMOS_standard.nc` to byte precission to save space:
```bash
ncatted -a _FillValue,,o,f,-127 SAO_3D_COSMOS_standard.nc mpiom_r360x180L40_geographic_grid_byte.nc
ncpdq -O -M hgh_byt mpiom_r360x180L40_geographic_grid_byte.nc mpiom_r360x180L40_geographic_grid_byte.nc
```

