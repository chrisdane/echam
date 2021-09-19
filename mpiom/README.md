# mpiom

# convert from grb to nc with parameter table
convert mpiom grb data to nc and apply default parameter table:
```bash
cdo -t mpiom1 -f nc copy Hol-Tx10_mpiom_32900101_32901231.grb Hol-Tx10_mpiom_32900101_32901231.grb.nc
```

# regridding
regrid from bi-/tripolar mpiom grids
```
GR30: nlon=120, nlat=101, ngridpoints=12120, dx=2.951°, dy=1.8°
GR15: nlon=256, nlat=220, ngridpoints=56320, ~1.5°
TP04: nlon=802, nlat=404, ngridpoints=324008, ~0.4°
```
to regular grids with
```bash
## GR* (bipolar)
# grb scalar
cdo -remapbil,r360x180 -setgrid,grid_s -sethalo,-1,-1 sin sout
# grb vector
cdo -remapbil,r360x180 -mrotuvb -setgrid,grid_u -sethalo,-1,-1 uin -setgrid,grid_v -sethalo,-1,-1 vin uvout
# nc scalar
cdo -remapbil,r360x180 -sethalo,-1,-1 sin sout
# nc vector
cdo -remapbil,r360x180 -sethalo,-1,-1 -mrotuvb uin vin uvout
## TP* (tripolar)
# grb scalar
cdo -remapbil,r360x180 -selindexbox,1,800,3,404 -setgrid,grid_s -sethalo,-1,-1 sin sout
# grb vector
cdo -remapbil,r360x180 -selindexbox,1,800,3,404 -mrotuvb -setgrid,grid_u -sethalo,-1,-1 uin -setgrid,grid_v -sethalo,-1,-1 vin uvout
# nc scalar
cdo -remapbil,r360x180 -selindexbox,2,801,3,404 sin sout
# nc vector
cdo -remapbil,r360x180 -selindexbox,2,801,3,404 -mrotuvb uin vin uvout
```
with
```
grid_s: /pool/data/MPIOM/GR15/GR15s.nc, /pool/data/MPIOM/TP04/TP04s.nc
grid_u: /pool/data/MPIOM/GR15/GR15u.nc, /pool/data/MPIOM/TP04/TP04u.nc
grid_v: /pool/data/MPIOM/GR15/GR15v.nc, /pool/data/MPIOM/TP04/TP04v.nc 
```
This procdure is coded in `mpiom_functions.r:mpiom1_remap2lonlat()`.

```
uin=/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Omon/uo/gn/v20200909/uo_Omon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_191001-192912.nc
vin=/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Omon/vo/gn/v20190710/vo_Omon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_191001-192912.nc
cdo -sethalo,-1,-1 -mrotuvb -sellevel,6 -seltimestep,1 $uin -sellevel,6 -seltimestep,1 $vin rot.nc
cdo ifthen -sethalo,-1,-1 -sellevel,6 -selvar,amsuo /pool/data/MPIOM/input/r0013/GR15/GR15L40_fx.nc -selvar,uo rot.nc u_miss.nc
cdo ifthen -sethalo,-1,-1 -sellevel,6 -selvar,amsue /pool/data/MPIOM/input/r0013/GR15/GR15L40_fx.nc -selvar,vo rot.nc v_miss.nc
cdo merge u_miss.nc v_miss.nc rot_miss.nc
cdo -remapbil,r360x180 rot_miss.nc rot_miss_remapbil.nc
#cdo -remapbil,r3600x1800 rot_miss.nc rot_miss_remap.nc
cdo -remapnn,r360x180 rot_miss.nc rot_miss_remapnn.nc
## input amuso: float amsuo(time, depth_2, y_3, x_3) ; --> (x,y) = (256,220)
# x = 256 ;
# y = 220 ;
# x_2 = 256 ;
# y_2 = 220 ;
# x_3 = 256 ;
# y_3 = 220 ;
# x_4 = 256 ;
# y_4 = 220 ;
# depth = 1 ;
# depth_2 = 40 ;
## input u: float uo(time, lev, y, x) ; --> (x,y) = (254,220)
# time = UNLIMITED ; // (1 currently)
# bnds = 2 ;
# x = 254 ;
# y = 220 ;
# x_2 = 256 ;
# lev = 1 ;
# time = UNLIMITED ; // (1 currently)
```

# levels

## GR15L40, TP04L40 levels

| no | showlevel | lev\_bnds[1] | lev\_bnds[2] |
| ---|-----------|--------------|--------------|
| 1 |	6	|	0	|	12	|
| 2 |	17	|	12	|	22	|
| 3 |	27	|	22	|	32	|
| 4 |	37	|	32	|	42	|
| 5 |	47	|	42	|	52	|
| 6 |	57	|	52	|	62	|
| 7 |	68.5	|	62	|	75	|
| 8 |	82.5	|	75	|	90	|
| 9 |	100	|	90	|	110	|
| 10 |	122.5	|	110	|	135	|
| 11 |	150	|	135	|	165	|
| 12 |	182.5	|	165	|	200	|
| 13 |	220	|	200	|	240	|
| 14 |	262.5	|	240	|	285	|
| 15 |	310	|	285	|	335	|
| 16 |	362.5	|	335	|	390	|
| 17 |	420	|	390	|	450	|
| 18 |	485	|	450	|	520	|
| 19 |	560	|	520	|	600	|
| 20 |	645	|	600	|	690	|
| 21 |	740	|	690	|	790	|
| 22 |	845	|	790	|	900	|
| 23 |	960	|	900	|	1020	|
| 24 |	1085	|	1020	|	1150	|
| 25 |	1220	|	1150	|	1290	|
| 26 |	1365	|	1290	|	1440	|
| 27 |	1525	|	1440	|	1610	|
| 28 |	1700	|	1610	|	1790	|
| 29 |	1885	|	1790	|	1980	|
| 30 |	2080	|	1980	|	2180	|
| 31 |	2290	|	2180	|	2400	|
| 32 |	2525	|	2400	|	2650	|
| 33 |	2785	|	2650	|	2920	|
| 34 |	3070	|	2920	|	3220	|
| 35 |	3395	|	3220	|	3570	|
| 36 |	3770	|	3570	|	3970	|
| 37 |	4195	|	3970	|	4420	|
| 38 |	4670	|	4420	|	4920	|
| 39 |	5170	|	4920	|	5420	|
| 40 |	5720	|	5420	|	6020	|

## wip
add variable lev\_bnds:
```
ncap2 -s 'defdim("lev_bnds",2);lev_bnds[lev_bnds]={6,200};' intlevel.nc intlevel2.nc
--> int lev_bnds(lev_bnds) ;    !=      double lev_bnds(lev, bnds) ;
```

add bounds attribute to lev variable
```
ncatted -a bounds,lev,c,c,"lev_bnds" intlevel2.nc
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

